;;; agent-shell-fakes.el --- A fake agent shell -*- lexical-binding: t; -*-


;;; Commentary:
;;

;;; Code:

(require 'acp-fakes)

(defun agent-shell-fakes--first-text-part (prompt)
  "Return the text of the first text part in PROMPT (a vector of parts)."
  (when prompt
    (map-elt (seq-find (lambda (part)
                         (equal (map-elt part 'type) "text"))
                       prompt)
             'text)))

(defun agent-shell-fakes--with-id (item new-id)
  "Return a copy of ITEM with its :object id set to NEW-ID."
  (let ((object (copy-alist (map-elt item :object)))
        (copy (copy-alist item)))
    (setf (alist-get 'id object) new-id)
    (setf (alist-get :object copy) object)
    copy))

(defun agent-shell-fakes--synth-prelude (messages)
  "Prepend synthetic init responses to MESSAGES and renumber to match.

Captured traffic files typically start mid-session and lack responses
for `initialize', `authenticate' (when applicable), and `session/new'.
Synthesise minimal responses for those at ids 1..N, then renumber the
first captured outgoing `session/prompt' request and its matching
response so they line up with the id the fake client will allocate."
  (let* ((has-auth (and (acp-fakes--get-authenticate-request :messages messages) t))
         (init-id 1)
         (auth-id (when has-auth 2))
         (session-new-id (if has-auth 3 2))
         ;; `agent-shell--refresh-session-title' fires on `init-finished'
         ;; and issues an extra `session/list' before the user prompt
         ;; gets sent. Synth a response so the counter advances cleanly.
         (session-list-id (if has-auth 4 3))
         (prompt-id (if has-auth 5 4))
         (orig-prompt (seq-find (lambda (item)
                                  (and (eq (map-elt item :direction) 'outgoing)
                                       (equal (map-nested-elt item '(:object method))
                                              "session/prompt")))
                                messages))
         (orig-prompt-id (map-nested-elt orig-prompt '(:object id)))
         (orig-session-id (map-nested-elt orig-prompt '(:object params sessionId)))
         (synth-init
          `((:direction . incoming) (:kind . response)
            (:object (jsonrpc . "2.0") (id . ,init-id)
                     (result (protocolVersion . 1)
                             (agentCapabilities
                              (loadSession . :false)
                              (promptCapabilities (image) (audio) (embeddedContext . t)))))))
         (synth-auth (when has-auth
                       `((:direction . incoming) (:kind . response)
                         (:object (jsonrpc . "2.0") (id . ,auth-id)
                                  (result)))))
         (synth-session-new
          `((:direction . incoming) (:kind . response)
            (:object (jsonrpc . "2.0") (id . ,session-new-id)
                     (result (sessionId . ,(or orig-session-id "fake-session-id"))))))
         (synth-session-list
          `((:direction . incoming) (:kind . response)
            (:object (jsonrpc . "2.0") (id . ,session-list-id)
                     (result (sessions . [])))))
         (prelude (delq nil (list synth-init synth-auth synth-session-new synth-session-list)))
         (renumbered
          (mapcar (lambda (item)
                    (let ((id (map-nested-elt item '(:object id)))
                          (direction (map-elt item :direction))
                          (object (map-elt item :object)))
                      (cond
                       ((and orig-prompt-id
                             (eq direction 'outgoing)
                             (equal (map-elt object 'method) "session/prompt")
                             (equal id orig-prompt-id))
                        (agent-shell-fakes--with-id item prompt-id))
                       ((and orig-prompt-id
                             (eq direction 'incoming)
                             (equal id orig-prompt-id)
                             (map-contains-key object 'result))
                        (agent-shell-fakes--with-id item prompt-id))
                       (t item))))
                  messages)))
    (append prelude renumbered)))

(defun agent-shell-fakes-load-session ()
  "Load and replay a traffic session from file."
  (interactive)
  (let* ((traffic-file (read-file-name "Load traffic file: " nil nil t))
         (messages (acp-traffic-read-file traffic-file))
         (buffer (agent-shell-fakes-start-agent messages))
         (first-prompt (progn
                         (unless buffer
                           (error "No shell buffer available"))
                         (seq-find (lambda (item)
                                     (and (eq (map-elt item :direction) 'outgoing)
                                          (equal (map-nested-elt item '(:object method)) "session/prompt")
                                          (let ((text (agent-shell-fakes--first-text-part
                                                       (map-nested-elt item '(:object params prompt)))))
                                            (and text (not (string-empty-p text))))))
                                   messages)))
         (first-prompt-text (agent-shell-fakes--first-text-part
                             (map-nested-elt first-prompt '(:object params prompt)))))
    (unless first-prompt-text
      (error "No first prompt text available to kick replay off"))
    (with-current-buffer buffer
      (shell-maker-submit :input first-prompt-text))))

(defun agent-shell-fakes-start-agent (messages)
  "Start a fake agent with traffic MESSAGES."
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
         (buffer (agent-shell--start :config config :session-strategy 'new-deferred)))
    buffer))

(defun agent-shell-fakes---welcome-message (config)
  "Return Fake ASCII art as per own repo using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-fakes--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
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
