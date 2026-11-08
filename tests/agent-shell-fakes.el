;;; agent-shell-fakes.el --- A fake agent shell -*- lexical-binding: t; -*-


;;; Commentary:
;;

;;; Code:

(require 'acp)
(require 'acp-fakes)
(require 'acp-traffic)

;; Set up by `agent-shell-mode' in the shell buffer; not compiled here.
(defvar agent-shell--state)
;; A defcustom in agent-shell.el; declared here so the driver can bind it
;; dynamically to drive the restore verbosity of a replayed `session/load'.
(defvar agent-shell-session-restore-verbosity)

(defun agent-shell-fakes--first-text-part (prompt)
  "Return the text of the first text part in PROMPT (a vector of parts)."
  (when prompt
    (map-elt (seq-find (lambda (part)
                         (equal (map-elt part 'type) "text"))
                       prompt)
             'text)))

(defun agent-shell-fakes--complete-capture-p (messages)
  "Return non-nil when MESSAGES is a full capture starting at `initialize'.

A complete capture already carries the `initialize' handshake and every
init response, so the fake client replays it verbatim with matching ids.
Synthesising a prelude for such a capture would duplicate responses and
throw the id sequence off."
  (when-let* ((first-request (seq-find (lambda (item)
                                         (eq (map-elt item :direction) 'outgoing))
                                       messages)))
    (and (equal (map-nested-elt first-request '(:object method)) "initialize")
         (seq-find (lambda (item)
                     (and (eq (map-elt item :direction) 'incoming)
                          (equal (map-nested-elt item '(:object id))
                                 (map-nested-elt first-request '(:object id)))
                          (map-contains-key (map-elt item :object) 'result)))
                   messages))))

(defun agent-shell-fakes--synth-prelude (messages)
  "Prepend a synthetic init handshake to MESSAGES.

Captured traffic files typically start mid-session, holding neither the
`initialize'/`authenticate'/`session/new' requests nor their responses.
Synthesise both halves of each so the replay has a client request to pair
the code under test's own against, and a reply to resolve it with.

A complete capture (starting at `initialize', see
`agent-shell-fakes--complete-capture-p') already carries them, so return
it untouched."
  (if (agent-shell-fakes--complete-capture-p messages)
      messages
    (agent-shell-fakes--synth-prelude-1 messages)))

(defun agent-shell-fakes--synth-exchange (id method params result)
  "Return a synthetic request/response pair for METHOD at ID.
PARAMS is the request\\='s params, RESULT the response\\='s result.

  (agent-shell-fakes--synth-exchange 7 "session/new" nil
                                     \\='((sessionId . "s1")))
  ;; => (((:direction . outgoing) (:kind . request)
  ;;      (:object (jsonrpc . "2.0") (method . "session/new") (id . 7)))
  ;;     ((:direction . incoming) (:kind . response)
  ;;      (:object (jsonrpc . "2.0") (id . 7)
  ;;               (result (sessionId . "s1")))))"
  (list `((:direction . outgoing) (:kind . request)
          (:object (jsonrpc . "2.0") (method . ,method) (id . ,id)
                   ,@(when params `((params . ,params)))))
        `((:direction . incoming) (:kind . response)
          (:object (jsonrpc . "2.0") (id . ,id) (result . ,result)))))

(defun agent-shell-fakes--max-recorded-id (messages)
  "Return the highest numeric id in MESSAGES, or 0 when there is none.
Synthetic ids start above it so they cannot collide with recorded ones.

  (agent-shell-fakes--max-recorded-id
   \\='(((:object (id . 3))) ((:object (id . 11)))))
  ;; => 11"
  (or (seq-max (cons 0 (seq-filter #'numberp
                                   (mapcar (lambda (item)
                                             (map-nested-elt item '(:object id)))
                                           messages))))
      0))

(defun agent-shell-fakes--synth-prelude-1 (messages)
  "Synthesise an init prelude for a mid-session capture MESSAGES.
See `agent-shell-fakes--synth-prelude', which delegates here."
  (let* ((has-auth (and (acp-fakes--get-authenticate-request :messages messages) t))
         (base (1+ (agent-shell-fakes--max-recorded-id messages)))
         (session-id (or (map-nested-elt
                          (seq-find (lambda (item)
                                      (and (eq (map-elt item :direction) 'outgoing)
                                           (equal (map-nested-elt item '(:object method))
                                                  "session/prompt")))
                                    messages)
                          '(:object params sessionId))
                         "fake-session-id"))
         (prelude
          (append
           (agent-shell-fakes--synth-exchange
            base "initialize" nil
            '((protocolVersion . 1)
              (agentCapabilities
               (loadSession . :false)
               (promptCapabilities (image) (audio) (embeddedContext . t)))))
           (when has-auth
             (agent-shell-fakes--synth-exchange (+ base 1) "authenticate" nil nil))
           (agent-shell-fakes--synth-exchange
            (+ base 2) "session/new" nil `((sessionId . ,session-id)))
           ;; `agent-shell--refresh-session-title' fires on `init-finished'
           ;; and issues a `session/list' before the user prompt is sent.
           (agent-shell-fakes--synth-exchange
            (+ base 3) "session/list" nil '((sessions . []))))))
    (append prelude messages)))

(defun agent-shell-fakes--settle (&optional n)
  "Let the fake client's replay and rendering settle (N idle cycles)."
  (dotimes (_ (or n 40)) (accept-process-output nil 0.005) (sit-for 0)))

(defun agent-shell-fakes--barrier-prompt (barrier)
  "Return BARRIER's user prompt text, or nil when it is not a prompt.
BARRIER is a recorded outgoing message (see `acp-fakes-barrier')."
  (when (equal (map-nested-elt barrier '(:object method)) "session/prompt")
    (let ((text (agent-shell-fakes--first-text-part
                 (map-nested-elt barrier '(:object params prompt)))))
      (unless (or (null text) (string-empty-p text))
        text))))

(defun agent-shell-fakes--drive (client buffer)
  "Replay CLIENT's recorded traffic to completion in BUFFER.

The fake client delivers incoming traffic on its own, stopping whenever
the recording sent something from the client side that BUFFER's agent has
not (see `acp-fakes-barrier').  This resolves each of those stops so the
replay runs to the end:

- a recorded `session/prompt' is submitted through the shell, so the user
  turn renders exactly as typing it would (skipped while the shell is
  busy: a restore still running background work has no live prompt);

- anything else is client traffic this code path never sends (say a
  `session/list' a resume-by-id skips), so it is passed over.

Between stops the replay is given time to settle, since the agent answers
pushes and refreshes titles on its own."
  (acp-fakes-pump client)
  (let ((barrier (acp-fakes-barrier client))
        ;; Every pass either advances the replay or skips a barrier, so
        ;; this only guards against a pathological recording.
        (guard 0))
    (while (and barrier (< guard 1000))
      (setq guard (1+ guard))
      (agent-shell-fakes--settle)
      (cond
       ;; The agent advanced the replay itself (a push response, say).
       ((not (eq barrier (acp-fakes-barrier client))))
       ((and (agent-shell-fakes--barrier-prompt barrier)
             (not (with-current-buffer buffer (shell-maker-busy))))
        (with-current-buffer buffer
          (shell-maker-submit :input (agent-shell-fakes--barrier-prompt barrier)))
        (agent-shell-fakes--settle))
       (t
        (acp-fakes-skip-barrier client)))
      (setq barrier (acp-fakes-barrier client)))))

(defun agent-shell-fakes-load-session (&optional traffic-file)
  "Load and replay a recorded ACP session from TRAFFIC-FILE, popping to it.

The recording is replayed as one ordered stream, each message delivered
exactly once: starting the agent drives the handshake, and
`agent-shell-fakes--drive' carries the conversation to the end,
submitting the recorded user prompt and letting pushed turns render in
the order the capture holds them.

Reads TRAFFIC-FILE from the minibuffer when called interactively."
  (interactive)
  (let* ((traffic-file (or traffic-file (read-file-name "Load traffic file: " nil nil t)))
         (messages (acp-traffic-read-file traffic-file))
         ;; A recorded `session/load' restores its whole conversation.
         ;; Replay every buffered turn so the restored history renders as
         ;; the capture recorded it rather than the `minimal' default,
         ;; which would omit the user prompt turns.
         (agent-shell-session-restore-verbosity 'full)
         (buffer (agent-shell-fakes-start-agent messages)))
    (unless buffer
      (error "No shell buffer available"))
    (agent-shell-fakes--settle)
    (agent-shell-fakes--drive
     (map-elt (buffer-local-value 'agent-shell--state buffer) :client)
     buffer)
    (pop-to-buffer buffer)
    buffer))

(defun agent-shell-fakes--recorded-load-session-id (messages)
  "Return the sessionId of a recorded outgoing `session/load' in MESSAGES.
Return nil when the capture established its session with `session/new'
instead, so the caller starts a fresh session rather than resuming."
  (when-let* ((load (seq-find (lambda (item)
                                (and (eq (map-elt item :direction) 'outgoing)
                                     (equal (map-nested-elt item '(:object method))
                                            "session/load")))
                              messages)))
    (map-nested-elt load '(:object params sessionId))))

(defun agent-shell-fakes-start-agent (messages)
  "Start a fake agent with traffic MESSAGES.

When the capture restored its session with `session/load', resume that
session by id so the recorded history (both user and agent turns)
replays through agent-shell's restore path -- the fake client streams
the load's history notifications while the request is in flight, and
agent-shell renders them under the active load.  Otherwise start fresh."
  (let* ((authenticate-message (acp-fakes--get-authenticate-request :messages messages))
         (authenticate-request (when authenticate-message
                                 (list (cons :method (map-nested-elt authenticate-message '(:object method)))
                                       (cons :params (map-nested-elt authenticate-message '(:object params))))))
         (config (agent-shell-make-agent-config
                  :mode-line-name "Fake"
                  :buffer-name "Fake"
                  :shell-prompt "Fake> "
                  :shell-prompt-regexp "Fake> "
                  :icon-name "https://purepng.com/public/uploads/large/purepng.com-futurama-benderfuturamaanimated-sciencefictionsitcomcartoonfuturama-benderbender-17015285631369sm6z.png"
                  :welcome-function #'agent-shell-fakes---welcome-message
                  :client-maker (lambda (buffer)
                                  (let ((client (acp-fakes-make-client
                                                 (agent-shell-fakes--synth-prelude messages))))
                                    (map-put! client :context-buffer buffer)
                                    client))
                  :needs-authentication authenticate-request
                  :authenticate-request-maker (lambda ()
                                                authenticate-request)))
         (load-session-id (agent-shell-fakes--recorded-load-session-id messages))
         ;; Resume by id drives the recorded `session/load'.  The `latest'
         ;; strategy (rather than the default `prompt') is deliberate: it
         ;; skips the interactive session picker and its "Loading..."
         ;; minibuffer spinner, whose hide events the synchronous fake
         ;; client would emit before the picker's subscriptions exist,
         ;; leaking the spinner's timer.  Resume-by-id ignores the
         ;; strategy for the load decision, so `latest' only suppresses
         ;; the picker.
         (buffer (if load-session-id
                     (agent-shell--start :config config
                                         :session-id load-session-id
                                         :session-strategy 'latest)
                   (agent-shell--start :config config :session-strategy 'new))))
    buffer))

(defun agent-shell-fakes---welcome-message (config)
  "Return Fake ASCII art as per own repo using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-fakes--ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-fakes--ascii-art ()
  "Fake ASCII art.

Generated by https://github.com/shinshin86/oh-my-logo."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
░▒▓████████▓▒░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░
░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
░▒▓██████▓▒░░▒▓████████▓▒░▒▓███████▓▒░░▒▓██████▓▒░
░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#b7c3cc" :inherit fixed-pitch)
                                       '(:foreground "#7e909a" :inherit fixed-pitch)))))



(provide 'agent-shell-fakes)

;;; agent-shell-fakes.el ends here
