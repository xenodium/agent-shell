;;; agent-shell-retry.el --- Turn auto-retry support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Zhixu Zhao

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Commentary:
;;
;; Provides auto-retry support for turns failing with retriable errors.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)

(declare-function agent-shell--send-command "agent-shell")
(declare-function agent-shell--separate-transcript-after-agent-message "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")

(defvar agent-shell--transcript-file)

(defvar agent-shell-auto-retry nil
  "Non-nil to auto-retry a turn that ends with a retriable error.

Off by default.

Some agents catch a transient error internally, print it as ordinary
assistant text, and finish the turn normally, leaving the task
unfinished.  When such a turn's final line matches
`agent-shell-retriable-turn-tail-regexps', agent-shell sends
`agent-shell-retry-prompt' as a new user turn (up to
`agent-shell-retry-max-retries' times) rather than stopping.  Unlike
an agent's own internal API retries, this recovers the whole turn at
the ACP layer.  Interrupting the session cancels any pending retry.")

(defvar agent-shell-retry-prompt
  "[agent-shell auto-retry] The previous turn failed with a retriable error. Please continue working on the task where you left off."
  "Prompt sent as a new user turn to recover from a retriable error.
The \"[agent-shell auto-retry]\" marker keeps it distinguishable from a
user-authored prompt in the shell, transcript, and to the agent.")

(defvar agent-shell-retry-max-retries 2
  "Maximum number of auto-retry turns before surfacing the error.")

(defvar agent-shell-retry-backoff-seconds '(2 5)
  "Backoff delays (in seconds) indexed by 1-based retry number.
Has one entry per `agent-shell-retry-max-retries'; the last entry is
reused for any retry beyond the list length.")

(defvar agent-shell-retriable-turn-tail-regexps
  '("^Error: RetriableError:")
  "Case-insensitive regexps for a retriable error emitted as agent text.

Matched against the final non-blank line of a turn that completed with an
\"end_turn\" stop reason.")

(defun agent-shell--retriable-turn-tail-p (text)
  "Return TEXT's final non-blank line when that line is a retriable error.

The line (trimmed) is matched case-insensitively against
`agent-shell-retriable-turn-tail-regexps'; returns it on a match, nil
otherwise (so callers can reuse it as the error message).

Deliberately narrow and anchored to the trailing line; a broad match would
misfire on turns that merely analyze error logs, which mention errors
mid-message rather than ending on one."
  (when-let* ((line (car (last (seq-remove #'string-empty-p
                                           (seq-map #'string-trim
                                                   (split-string (or text "") "\n"))))))
              (case-fold-search t)
              ((seq-some (lambda (regexp)
                           (string-match-p regexp line))
                         agent-shell-retriable-turn-tail-regexps)))
    line))

;; TODO: Not retry-specific: generic per-turn message accumulation,
;; kept here while retry detection is its only consumer.  If another
;; feature ever needs the full turn text, move ownership to
;; agent-shell.el and accumulate a chunk list (O(1) per chunk, joined
;; on read); tail detection can then read that field's tail instead of
;; keeping this capped copy.
(defun agent-shell--accumulate-turn-agent-message (state content)
  "Append CONTENT to STATE's `:turn-agent-message', keeping a bounded tail.

Only the turn's trailing text is needed to detect a retriable error emitted
as the final line, so the stored string is capped rather than growing with
the whole message.  A no-op on sessions predating the field."
  (when (and content (assq :turn-agent-message state))
    (let ((combined (concat (map-elt state :turn-agent-message) content)))
      (map-put! state :turn-agent-message
                (if (> (length combined) 4000)
                    (substring combined -4000)
                  combined)))))

(defun agent-shell--retry-backoff-for-attempt (retry-number)
  "Return the backoff delay in seconds before RETRY-NUMBER (1-based).

Clamps to the last entry of `agent-shell-retry-backoff-seconds'.

For example, with `agent-shell-retry-backoff-seconds' set to (2 5 10):
  (agent-shell--retry-backoff-for-attempt 1) => 2
  (agent-shell--retry-backoff-for-attempt 4) => 10"
  (or (nth (1- retry-number) agent-shell-retry-backoff-seconds)
      (car (last agent-shell-retry-backoff-seconds))
      0))

(defun agent-shell--retry-prompt (state)
  "Return auto-retry prompt for STATE.

Includes STATE's `:last-user-prompt' when available, so a retry turn can
recover even if the agent lost the user request that failed."
  (if-let* ((last-user-prompt (map-elt state :last-user-prompt))
            ((not (string-empty-p (string-trim last-user-prompt)))))
      (format "%s

Last user prompt before the failure:

%s"
              agent-shell-retry-prompt last-user-prompt)
    agent-shell-retry-prompt))

(cl-defun agent-shell--retry-completed-turn-p (&key state stop-reason message)
  "Return the offending trailing line if a COMPLETED turn should be retried.

Returns MESSAGE's trailing line (for use as the error message) when
auto-retry is enabled, STOP-REASON is \"end_turn\", that line matches
`agent-shell-retriable-turn-tail-regexps', and the retry budget (STATE's
`:retry-attempt') is not exhausted; nil otherwise."
  (and agent-shell-auto-retry
       (equal stop-reason "end_turn")
       (< (map-elt state :retry-attempt 0) agent-shell-retry-max-retries)
       (agent-shell--retriable-turn-tail-p message)))

(defun agent-shell--cancel-retry-timer (state)
  "Cancel and clear STATE's pending `:retry-timer', if any."
  (when-let* ((timer (map-elt state :retry-timer)))
    (cancel-timer timer)
    (map-put! state :retry-timer nil)))

(cl-defun agent-shell--schedule-retry (&key state shell-buffer acp-error)
  "Schedule an auto-retry turn after a backoff delay.

Once the delay elapses, sends `agent-shell-retry-prompt' as a new user
turn via `agent-shell--send-command', incrementing STATE's
`:retry-attempt'.  This recovers a turn that ended with a retriable
error by asking the agent to continue, keeping it distinct from an
agent's own internal API retries.

The heartbeat and busy state are left untouched so the shell keeps
signaling work in progress during the wait, and the pending timer is
stored in STATE's `:retry-timer' so `agent-shell-interrupt' or shutdown
can cancel it.

STATE and SHELL-BUFFER identify the shell.  ACP-ERROR is the failure
payload shown in the on-screen retry notice."
  (let* ((retry-number (1+ (map-elt state :retry-attempt 0)))
         (delay (agent-shell--retry-backoff-for-attempt retry-number))
         (error-message (or (map-elt acp-error 'message) "unknown error"))
         (retry-prompt (agent-shell--retry-prompt state)))
    (map-put! state :retry-attempt retry-number)
    ;; A failed turn may have stopped mid agent_message_chunk, leaving the
    ;; transcript body without a trailing newline.  Separate it so the
    ;; retry's user header lands on its own line.
    (agent-shell--separate-transcript-after-agent-message
     :last-entry-type (map-elt state :last-entry-type)
     :file-path agent-shell--transcript-file)
    (agent-shell--update-fragment
     :state state
     :block-id (format "%s-retry-%s" (map-elt state :request-count) retry-number)
     :label-left (propertize "Retrying" 'font-lock-face 'agent-shell-warning)
     :body (format "%s

Retrying in %ds (retry %d/%d) with prompt:

%s"
                   (propertize (format "Retriable error: %s" error-message)
                               'font-lock-face 'agent-shell-warning)
                   delay retry-number agent-shell-retry-max-retries
                   retry-prompt)
     :create-new t)
    (map-put! state :retry-timer
              (run-at-time
               delay nil
               (lambda ()
                 (when (buffer-live-p shell-buffer)
                   (with-current-buffer shell-buffer
                     (map-put! state :retry-timer nil)
                     (agent-shell--send-command
                      :prompt retry-prompt
                      :shell-buffer shell-buffer
                      :retry t))))))))

(provide 'agent-shell-retry)

;;; agent-shell-retry.el ends here
