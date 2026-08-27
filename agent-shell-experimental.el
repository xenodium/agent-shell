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
(declare-function agent-shell--build-content-blocks "agent-shell")
(declare-function agent-shell--expand-truncated-regions "agent-shell")
(declare-function agent-shell--send-request "agent-shell")
(declare-function agent-shell-heartbeat-start "agent-shell-heartbeat")
(declare-function agent-shell-heartbeat-stop "agent-shell-heartbeat")
(declare-function shell-maker-insert-end-of-prompt-marker "shell-maker")

(defvar agent-shell-show-busy-indicator)
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

(defconst agent-shell-experimental--steering-method "_session/steering"
  "Request method that steers a prompt into the turn already running.

Not part of the ACP spec -- the leading underscore marks it as an
extension -- but implemented under this name by the Claude and Codex
adapters, which advertise it as `_meta.steering.supported' in their
`initialize' response.  The ACP proposal that would standardise this is
`session/inject' (RFD 1261), still unmerged and targeting v2.")

(cl-defun agent-shell-experimental--make-session-steering-request (&key session-id prompt)
  "Instantiate a steering request for SESSION-ID carrying PROMPT.

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
  `((:method . ,agent-shell-experimental--steering-method)
    (:params . ((sessionId . ,session-id)
                (prompt . ,(vconcat prompt))
                (_meta . ((steering . ((idleBehavior . "promptRequired")))))))))

(defun agent-shell-experimental--steering-outcome (acp-response)
  "Return ACP-RESPONSE's steering outcome as a symbol.

One of:

  `injected'         - the prompt joined the turn that was running.
  `prompt-required'  - no turn was running and the agent left the
                       prompt with us to submit normally.
  `started-new-turn' - no turn was running and the agent started one
                       of its own, which we did not ask for and cannot
                       track.
  `failed'           - the agent could not apply the steer.

An unrecognised outcome maps to `failed' so a future agent's new answer
falls back to queueing rather than being mistaken for success.

For example:

  (agent-shell-experimental--steering-outcome \\='((outcome . \"injected\")))
  => injected

  (agent-shell-experimental--steering-outcome \\='((outcome . \"whatever\")))
  => failed"
  (pcase (map-elt acp-response 'outcome)
    ("injected" 'injected)
    ("promptRequired" 'prompt-required)
    ("startedNewTurn" 'started-new-turn)
    (_ 'failed)))

(cl-defun agent-shell-experimental--send-steering (&key state prompt on-outcome)
  "Steer PROMPT into the turn STATE's session is currently running.

Must be called from the shell buffer: PROMPT is converted with
`agent-shell--build-content-blocks', which reads the buffer's prompt
capabilities.

ON-OUTCOME is called with (OUTCOME MESSAGE): OUTCOME as per
`agent-shell-experimental--steering-outcome', and MESSAGE the agent's
error text when the request itself failed, nil otherwise.  A failed
request reports `failed' rather than propagating the error, so callers
have one code path for \"this did not get steered\" and can fall back to
queueing without losing the prompt."
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
     :on-success (lambda (acp-response)
                   (funcall on-outcome
                            (agent-shell-experimental--steering-outcome acp-response)
                            nil))
     :on-failure (lambda (acp-error _raw-message)
                   (funcall on-outcome 'failed (map-elt acp-error 'message))))))

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
