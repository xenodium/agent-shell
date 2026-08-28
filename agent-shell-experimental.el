;;; agent-shell-experimental.el --- Experimental ACP features -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

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
;; Experimental ACP features for agent-shell.
;;
;; session/push: Server-initiated prompt push.  The server sends
;; a request to the client, followed by session/update notifications,
;; concluded by an session_push_end notification.  The client
;; then responds to the original request.
;;
;; _session/steering: Steer a prompt into the turn already running,
;; instead of queueing it until the turn ends.  The client sends the
;; request; the agent answers whether the prompt joined the running
;; turn, or why it could not.  The steered prompt's own output arrives
;; as ordinary session/update notifications on the turn already in
;; flight, so nothing else changes.

;;; Code:

(require 'map)
(eval-when-compile
  (require 'cl-lib))

(declare-function acp-send-response "acp")
(declare-function acp-make-error "acp")
(declare-function agent-shell--active-requests-p "agent-shell")
(declare-function agent-shell--build-content-blocks "agent-shell")
(declare-function agent-shell--expand-truncated-regions "agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--append-transcript "agent-shell")
(declare-function agent-shell--indent-markdown-headers "agent-shell")
(declare-function agent-shell--live-input-prompt-p "agent-shell")
(declare-function agent-shell--reset-undo-history "agent-shell")
(declare-function agent-shell--make-boxed-message "agent-shell")
(declare-function agent-shell--send-request "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--update-text "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function shell-maker-busy "shell-maker")
(declare-function agent-shell-heartbeat-start "agent-shell-heartbeat")
(declare-function agent-shell-heartbeat-stop "agent-shell-heartbeat")
(declare-function shell-maker-insert-end-of-prompt-marker "shell-maker")

(defvar agent-shell-show-busy-indicator)
(defvar agent-shell--transcript-file)
(defvar shell-maker--busy)

(cl-defun agent-shell-experimental--on-session-push-request (&key state acp-request)
  "Handle an incoming session/push ACP-REQUEST with STATE.

The server pushes a prompt to the client, followed by session/update
notifications.  The client sends the response after receiving an
session_push_end notification.

If the client is busy (an active session/prompt or session/push is
in progress), the request is immediately rejected with an error."
  (if (seq-find (lambda (r)
                  (member (map-elt r :method)
                          '("session/prompt" "session/push")))
                (map-elt state :active-requests))
      ;; Busy. Reject push request.
      (acp-send-response
       :client (map-elt state :client)
       :response (agent-shell-experimental--make-session-push-response
                  :request-id (map-elt acp-request 'id)
                  :error (acp-make-error :code -32000
                                         :message "Busy")))
    (let ((request (agent-shell-experimental--normalize-request acp-request)))
      ;; Track as active so notifications are not treated as stale.
      (unless (assq :active-requests state)
        (nconc state (list (cons :active-requests nil))))
      (map-put! state :active-requests
                (cons request (map-elt state :active-requests))))
    ;; Remove trailing empty shell prompt before push notifications render.
    (agent-shell-experimental--remove-trailing-prompt)
    ;; Give the pushed agent content its own end-of-prompt boundary so it
    ;; renders as an agent turn.  Without it the content attaches to the
    ;; previous turn with no agent marker, and chat mode (which anchors the
    ;; agent label on the marker) mislabels it as a user `Me' turn.
    (when-let* ((buffer (map-elt state :buffer)))
      (with-current-buffer buffer
        (shell-maker-insert-end-of-prompt-marker)))
    ;; Mark busy so requests are queued rather than sent mid-push.
    ;; Cleared on session_push_end via `shell-maker-finish-output'.
    (setq shell-maker--busy t)
    (when agent-shell-show-busy-indicator
      (agent-shell-heartbeat-start
       :heartbeat (map-elt state :heartbeat)))
    (map-put! state :last-entry-type "session/push")))

(defun agent-shell-experimental--remove-trailing-prompt ()
  "Remove the trailing empty shell prompt if it is at end of buffer."
  (when-let* ((comint-last-prompt)
              (prompt-start (car comint-last-prompt))
              (prompt-end (cdr comint-last-prompt))
              ((= (marker-position prompt-end) (point-max))))
    (let ((inhibit-read-only t))
      (delete-region (marker-position prompt-start) (point-max)))))

(cl-defun agent-shell-experimental--on-session-push-end (&key state on-finished)
  "Handle session_push_end notification with STATE.

Finds the active push prompt request, sends the response, and
removes it from active requests.  Calls ON-FINISHED when done
to allow the caller to finalize (e.g. display a new shell prompt)."
  (when-let* ((push-request (seq-find (lambda (r)
                                        (equal (map-elt r :method) "session/push"))
                                      (map-elt state :active-requests))))
    (acp-send-response
     :client (map-elt state :client)
     :response (agent-shell-experimental--make-session-push-response
                :request-id (map-elt push-request :id)))
    (map-put! state :active-requests
              (seq-remove (lambda (r)
                            (equal (map-elt r :method) "session/push"))
                          (map-elt state :active-requests)))
    (agent-shell-heartbeat-stop
     :heartbeat (map-elt state :heartbeat))
    (map-put! state :last-entry-type "session_push_end")
    (when on-finished
      (funcall on-finished))))

(cl-defun agent-shell-experimental--make-session-push-response (&key request-id error)
  "Instantiate a \"session/push\" response.

REQUEST-ID is the ID of the incoming server request this responds to.
ERROR is an optional error object if the push prompt was rejected."
  (unless request-id
    (error ":request-id is required"))
  (if error
      `((:request-id . ,request-id)
        (:error . ,error))
    `((:request-id . ,request-id)
      (:result . nil))))

(defun agent-shell-experimental--methods ()
  "Return the list of experimental methods that replay session notifications."
  '("session/push"))

(cl-defun agent-shell-experimental--make-session-steering-request (&key session-id prompt)
  "Instantiate a \"_session/steering\" request for SESSION-ID carrying PROMPT.

Not part of the ACP spec -- the leading underscore marks it as an
extension -- but implemented under this name by the Claude and Codex
adapters, which advertise it as `_meta.steering.supported' in their
`initialize' response.  The ACP proposal that would standardise this is
`session/inject' (RFD 1261), still unmerged and targeting v2.

PROMPT is a vector of content blocks, the same shape `session/prompt'
takes.

The `idleBehavior' opt-in asks the agent to do nothing and say so when
no turn is running, rather than starting a detached turn out of our
sight.  Only the Claude adapter honours it; Codex's parameter parser
passes unknown keys through and ignores them, so a steer that races the
end of a turn can still come back `startedNewTurn' there.

For example:

  (agent-shell-experimental--make-session-steering-request
   :session-id \"sess-1\"
   :prompt [((type . \"text\") (text . \"just the filenames\"))])

  => ((:method . \"_session/steering\")
      (:params . ((sessionId . \"sess-1\")
                  (prompt . [((type . \"text\")
                              (text . \"just the filenames\"))])
                  (_meta . ((steering
                             . ((idleBehavior . \"promptRequired\"))))))))"
  (unless session-id
    (error ":session-id is required"))
  (unless prompt
    (error ":prompt is required"))
  `((:method . "_session/steering")
    (:params . ((sessionId . ,session-id)
                (prompt . ,(vconcat prompt))
                (_meta . ((steering . ((idleBehavior . "promptRequired")))))))))

(cl-defun agent-shell-experimental--send-steering (&key state prompt)
  "Steer PROMPT into the turn STATE's session is currently running.

Must be called from the shell buffer: PROMPT is converted with
`agent-shell--build-content-blocks', which reads the buffer's prompt
capabilities.

Acts on the `outcome' the agent answers with:

  \"injected\"        - the prompt joined the turn that was running, so
                      render it as the user prompt it is.  Claude and
                      Codex.
  \"startedNewTurn\"  - no turn was running and the agent started one of
                      its own, which we did not ask for and cannot track.
                      Claude and Codex.
  \"promptRequired\"  - no turn was running and the agent left the prompt
                      with us to submit normally.  Claude only, and only
                      because the request opts into it; Codex ignores the
                      opt-in, though codex-acp#363 would bring it in line.
  \"failed\"          - the agent could not apply the steer.  Codex only.

Agents differ on which they answer with, so an unrecognised outcome, and
a request that fails outright, are treated as a steer that did not land:
reported in the shell, and the running turn interrupted.

Nothing here queues.  Queueing is `agent-shell-prompt-queue', and a steer
turned into a queued prompt would reach the agent long after the user
asked for it."
  (let* ((expanded (agent-shell--expand-truncated-regions prompt))
         (content-blocks (condition-case nil
                             (agent-shell--build-content-blocks expanded)
                           (error `[((type . "text")
                                     (text . ,(substring-no-properties expanded)))]))))
    (agent-shell--send-request
     :state state
     :client (map-elt state :client)
     :request (agent-shell-experimental--make-session-steering-request
               :session-id (map-nested-elt state '(:session :id))
               :prompt content-blocks)
     :buffer (current-buffer)
     :on-failure (lambda (acp-error _raw-message)
                   (agent-shell--update-fragment
                    :state (agent-shell--state)
                    :block-id (format "%s-steer-declined"
                                      (map-elt (agent-shell--state) :request-count))
                    :body (agent-shell--make-boxed-message
                           ;; Unlike an agent answering with an outcome, a
                           ;; request that failed outright says nothing about
                           ;; why, so carry its error text.
                           :text (if (map-elt acp-error 'message)
                                     (format "Note: Steered prompt declined (%s)."
                                             (map-elt acp-error 'message))
                                   "Note: Steered prompt declined."))
                    :create-new t
                    :above-last-prompt (not (agent-shell--active-requests-p
                                             (agent-shell--state))))
                   (map-put! (agent-shell--state) :last-entry-type "steering_declined")
                   ;; Interrupted so the agent does not carry on for a long
                   ;; while in a direction the user believes they already
                   ;; corrected.  A declined steer does not say whether
                   ;; continuing is harmless, and the cost of guessing wrong
                   ;; is far higher one way than interrupting current work.
                   (agent-shell-interrupt t))
     :on-success
     (lambda (acp-response)
       (pcase (map-elt acp-response 'outcome)
         ;; Claude and Codex both answer with this one.
         ("injected"
          (agent-shell-experimental--render-steered-prompt
           :state (agent-shell--state) :prompt prompt))
         ;; Claude only, and only because the request opts into it with
         ;; _meta.steering.idleBehavior set to "promptRequired".  A shell
         ;; that has not yet processed its own `session/prompt' response is
         ;; still busy, where submitting errors ("Busy, try later") without
         ;; inserting, so that case falls through to declined.
         ((and "promptRequired" (guard (not (shell-maker-busy))))
          (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t))
         ;; Claude and Codex both answer with this one.
         ("startedNewTurn"
          (agent-shell--update-fragment
           :state (agent-shell--state)
           :block-id (format "%s-steer-detached-turn"
                             (map-elt (agent-shell--state) :request-count))
           :body (agent-shell--make-boxed-message
                  :text "Note: Steered prompt still in progress.")
           :create-new t
           :above-last-prompt (not (agent-shell--active-requests-p
                                    (agent-shell--state))))
          (map-put! (agent-shell--state) :last-entry-type "steering_detached_turn"))
         ;; Codex's "failed", a "promptRequired" this shell is too busy to
         ;; act on, and anything an agent we do not know about answers with.
         (_
          (agent-shell--update-fragment
           :state (agent-shell--state)
           :block-id (format "%s-steer-declined"
                             (map-elt (agent-shell--state) :request-count))
           :body (agent-shell--make-boxed-message
                  :text (if (map-elt acp-response 'outcome)
                            (format "Note: Steered prompt declined (%s)."
                                    (map-elt acp-response 'outcome))
                          "Note: Steered prompt declined."))
           :create-new t
           :above-last-prompt (not (agent-shell--active-requests-p
                                    (agent-shell--state))))
          (map-put! (agent-shell--state) :last-entry-type "steering_declined")
          ;; Interrupted so the agent does not carry on for hours in a
          ;; direction the user believes they already corrected.  A declined
          ;; steer does not say whether continuing is harmless, and the cost
          ;; of guessing wrong is far higher one way than the other.
          (agent-shell-interrupt t)))))))

(cl-defun agent-shell-experimental--render-steered-prompt (&key state prompt)
  "Render PROMPT into STATE's shell as the user prompt it is.

A steered prompt is never echoed back: neither the Claude nor the Codex
adapter forwards it as a `user_message_chunk' while the turn runs, so
the client renders it or it does not appear at all.

Uses the field/face shape comint gives a live prompt, so
`comint-next-prompt', `agent-shell-next-item', copying and screen-reader
prompt navigation treat it as a user prompt rather than as a new kind of
entry.  `shell-maker-insert-end-of-prompt-marker' then closes it, which
is what bounds the prompt for anything measuring it by searching forward
for the marker; without it such a search runs to the next submission and
takes the rest of the turn's output as this prompt's body.

The `[steer]' prefix carries the one thing this shape would otherwise
lose -- that the prompt was injected into a running turn rather than
typed before one.

The turn can end while the steer is in flight, leaving a live input
prompt at the buffer end by the time the agent answers.  Rendering above
it then keeps this out of comint's input area, where submitting would
send it as input."
  (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count)))
  (agent-shell--append-transcript
   :text (format "## User (steered) (%s)\n\n%s\n\n"
                 (format-time-string "%F %T")
                 (agent-shell--indent-markdown-headers prompt))
   :file-path agent-shell--transcript-file)
  (with-current-buffer (map-elt state :buffer)
    ;; Narrow to everything above a live input prompt so both inserts land
    ;; there, and flip the prompt-start marker's insertion type so it
    ;; advances past the new text rather than being stranded inside it.
    ;; `shell-maker-insert-end-of-prompt-marker' documents this narrowing as
    ;; the way to synthesize history above a live prompt.
    (let* ((late-prompt-start (and (not (shell-maker-busy))
                                   comint-last-prompt
                                   (marker-position (car comint-last-prompt))
                                   (agent-shell--live-input-prompt-p comint-last-prompt)
                                   (car comint-last-prompt)))
           (orig-insertion-type (and late-prompt-start
                                     (marker-insertion-type late-prompt-start))))
      (when late-prompt-start
        (set-marker-insertion-type late-prompt-start t))
      (unwind-protect
          (save-restriction
            (when late-prompt-start
              (narrow-to-region (point-min) (marker-position late-prompt-start)))
            (agent-shell--update-text
             :state state
             :block-id (format "%s-steered-user_message_chunk"
                               (map-elt state :chunked-group-count))
             :text (concat (propertize (map-nested-elt state '(:agent-config :shell-prompt))
                                       'font-lock-face 'agent-shell-prompt
                                       'field 'output)
                           (propertize (concat "[steer] " (substring-no-properties prompt))
                                       'font-lock-face 'agent-shell-input))
             :create-new t)
            (shell-maker-insert-end-of-prompt-marker)
            ;; Under the narrowing `point-max' is the live prompt's start,
            ;; so this keeps that prompt on its own line.
            (when late-prompt-start
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "\n"))))
        (when late-prompt-start
          (set-marker-insertion-type late-prompt-start orig-insertion-type)))
      (when late-prompt-start
        ;; Rendering above the prompt pushed any unsubmitted input down, so
        ;; undo entries recorded for it point at the shifted text.
        (agent-shell--reset-undo-history))))
  ;; Deliberately not "user_message_chunk": that value asks the replay path
  ;; to insert the end-of-prompt marker on the next notification, and one
  ;; has already gone in above.
  (map-put! state :last-entry-type "steered_user_message"))

(defun agent-shell-experimental--normalize-request (request)
  "Normalize REQUEST from JSON symbol keys to keyword keys.

Incoming JSON-parsed requests use symbol keys (e.g. \\='method),
while internal request objects use keyword keys (e.g. :method).
This function converts the known keys that `acp--request-sender'
manually translates on the way out.

Example:

  \\='((method . \"session/push\")
    (id . 3)
    (params . ((prompt . [...]))))

becomes:

  \\='((:method . \"session/push\")
    (:id . 3)
    (:params . ((prompt . [...]))))"
  (seq-map (lambda (pair)
             (let ((key (car pair)))
               (cons (pcase key
                       ('method :method)
                       ('params :params)
                       ('id :id)
                       ('jsonrpc :jsonrpc)
                       (_ key))
                     (cdr pair))))
           request))

(provide 'agent-shell-experimental)

;;; agent-shell-experimental.el ends here
