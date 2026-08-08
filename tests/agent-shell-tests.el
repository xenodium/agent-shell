;;; agent-shell-tests.el --- Tests for agent-shell -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)

;;; Code:

(ert-deftest agent-shell-make-environment-variables-test ()
  "Test `agent-shell-make-environment-variables' function."
  ;; Test basic key-value pairs
  (should (equal (agent-shell-make-environment-variables
                  "PATH" "/usr/bin"
                  "HOME" "/home/user")
                 '("PATH=/usr/bin"
                   "HOME=/home/user")))

  ;; Test empty input
  (should (equal (agent-shell-make-environment-variables) '()))

  ;; Test single pair
  (should (equal (agent-shell-make-environment-variables "FOO" "bar")
                 '("FOO=bar")))

  ;; Test with keywords (should be filtered out)
  (should (equal (agent-shell-make-environment-variables
                  "VAR1" "value1"
                  :inherit-env nil
                  "VAR2" "value2")
                 '("VAR1=value1"
                   "VAR2=value2")))

  ;; Test error on incomplete pairs
  (should-error (agent-shell-make-environment-variables "PATH")
                :type 'error)

  ;; Test :inherit-env t
  (let ((process-environment '("EXISTING_VAR=existing_value"
                               "MY_OTHER_VAR=another_value")))
    (should (equal (agent-shell-make-environment-variables
                    "NEW_VAR" "new_value"
                    :inherit-env t)
                   '("NEW_VAR=new_value"
                     "EXISTING_VAR=existing_value"
                     "MY_OTHER_VAR=another_value"))))

  ;; Test :load-env with single file
  (let ((env-file (let ((file (make-temp-file "test-env" nil ".env")))
                    (with-temp-file file
                      (insert "TEST_VAR=test_value\n")
                      (insert "# This is a comment\n")
                      (insert "ANOTHER_TEST=another_value\n")
                      (insert "\n")  ; empty line
                      (insert "THIRD_VAR=third_value\n"))
                    file)))
    (unwind-protect
        (should (equal (agent-shell-make-environment-variables
                        "MANUAL_VAR" "manual_value"
                        :load-env env-file)
                       '("MANUAL_VAR=manual_value"
                         "TEST_VAR=test_value"
                         "ANOTHER_TEST=another_value"
                         "THIRD_VAR=third_value")))
      (delete-file env-file)))

  ;; Test :load-env with multiple files
  (let ((env-file1 (let ((file (make-temp-file "test-env1" nil ".env")))
                     (with-temp-file file
                       (insert "FILE1_VAR=file1_value\n")
                       (insert "SHARED_VAR=from_file1\n"))
                     file))
        (env-file2 (let ((file (make-temp-file "test-env2" nil ".env")))
                     (with-temp-file file
                       (insert "FILE2_VAR=file2_value\n")
                       (insert "SHARED_VAR=from_file2\n"))
                     file)))
    (unwind-protect
        (should (equal (agent-shell-make-environment-variables
                        :load-env (list env-file1 env-file2))
                       '("FILE1_VAR=file1_value"
                         "SHARED_VAR=from_file1"
                         "FILE2_VAR=file2_value"
                         "SHARED_VAR=from_file2")))
      (delete-file env-file1)
      (delete-file env-file2)))

  ;; Test :load-env with non-existent file (should error)
  (should-error (agent-shell-make-environment-variables
                 "TEST_VAR" "test_value"
                 :load-env "/non/existent/file")
                :type 'error)

  ;; Test :load-env combined with :inherit-env
  (let ((env-file (let ((file (make-temp-file "test-env" nil ".env")))
                    (with-temp-file file
                      (insert "ENV_FILE_VAR=env_file_value\n"))
                    file))
        (process-environment '("EXISTING_VAR=existing_value")))
    (unwind-protect
        (should (equal (agent-shell-make-environment-variables
                        "MANUAL_VAR" "manual_value"
                        :load-env env-file
                        :inherit-env t)
                       '("MANUAL_VAR=manual_value"
                         "ENV_FILE_VAR=env_file_value"
                         "EXISTING_VAR=existing_value")))
      (delete-file env-file))))

(ert-deftest agent-shell--shorten-paths-test ()
  "Test `agent-shell--shorten-paths' function."
  ;; Mock agent-shell-cwd to return a predictable value
  (cl-letf (((symbol-function 'agent-shell-cwd)
             (lambda () "/path/to/agent-shell/")))

    ;; Test shortening full paths to project-relative format
    (should (equal (agent-shell--shorten-paths
                    "/path/to/agent-shell/README.org")
                   "README.org"))

    ;; Test with subdirectories
    (should (equal (agent-shell--shorten-paths
                    "/path/to/agent-shell/tests/agent-shell-tests.el")
                   "tests/agent-shell-tests.el"))

    ;; Test mixed text with project path
    (should (equal (agent-shell--shorten-paths
                    "Read /path/to/agent-shell/agent-shell.el (4 - 6)")
                   "Read agent-shell.el (4 - 6)"))

    ;; Test text that doesn't contain project path (should remain unchanged)
    (should (equal (agent-shell--shorten-paths
                    "Some random text without paths")
                   "Some random text without paths"))

    ;; Test text with different paths (should remain unchanged)
    (should (equal (agent-shell--shorten-paths
                    "/some/other/path/file.txt")
                   "/some/other/path/file.txt"))

    ;; Test nil input
    (should (equal (agent-shell--shorten-paths nil) nil))

    ;; Test empty string
    (should (equal (agent-shell--shorten-paths "") ""))))

(ert-deftest agent-shell--format-plan-test ()
  "Test `agent-shell--format-plan' function."
  (dolist (test-case `(;; Graphical display mode
                       ( :graphic t
                         :homogeneous-expected
                         ,(concat " wait  Update state initialization\n"
                                  " wait  Update session initialization")
                         :mixed-expected
                         ,(concat " wait  First task\n"
                                  " busy  Second task\n"
                                  " done  Third task"))
                       ;; Terminal display mode
                       ( :graphic nil
                         :homogeneous-expected
                         ,(concat "[wait] Update state initialization\n"
                                  "[wait] Update session initialization")
                         :mixed-expected
                         ,(concat "[wait] First task\n"
                                  "[busy] Second task\n"
                                  "[done] Third task"))))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _display) (plist-get test-case :graphic))))
      ;; Test homogeneous statuses
      (should (equal (substring-no-properties
                      (agent-shell--format-plan [((content . "Update state initialization")
                                                  (status . "pending"))
                                                 ((content . "Update session initialization")
                                                  (status . "pending"))]))
                     (plist-get test-case :homogeneous-expected)))

      ;; Test mixed statuses
      (should (equal (substring-no-properties
                      (agent-shell--format-plan [((content . "First task")
                                                  (status . "pending"))
                                                 ((content . "Second task")
                                                  (status . "in_progress"))
                                                 ((content . "Third task")
                                                  (status . "completed"))]))
                     (plist-get test-case :mixed-expected)))))

  ;; Test empty entries
  (should (equal (agent-shell--format-plan []) "")))

(ert-deftest agent-shell--make-button-test ()
  "Test `agent-shell--make-button' brackets in terminal mode."
  ;; Graphical mode: spaces with box styling
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda (&optional _display) t)))
    (should (equal (substring-no-properties
                    (agent-shell--make-button
                     :text "Allow (y)"
                     :help "help"
                     :kind 'permission
                     :action #'ignore))
                   " Allow (y) ")))

  ;; Terminal mode: brackets
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda (&optional _display) nil)))
    (should (equal (substring-no-properties
                    (agent-shell--make-button
                     :text "Allow (y)"
                     :help "help"
                     :kind 'permission
                     :action #'ignore))
                   "[ Allow (y) ]"))))

(ert-deftest agent-shell--parse-file-mentions-test ()
  "Test agent-shell--parse-file-mentions function."
  ;; Simple @ mention
  (let ((mentions (agent-shell--parse-file-mentions "@file.txt")))
    (should (= (length mentions) 1))
    (should (equal (map-elt (car mentions) :path) "file.txt")))

  ;; @ mention with quotes
  (let ((mentions (agent-shell--parse-file-mentions "Compare @\"file with spaces.txt\" to @other.txt")))
    (should (= (length mentions) 2))
    (should (equal (map-elt (car mentions) :path) "file with spaces.txt"))
    (should (equal (map-elt (cadr mentions) :path) "other.txt")))

  ;; @ mention at start of line
  (let ((mentions (agent-shell--parse-file-mentions "@README.md is the main file")))
    (should (= (length mentions) 1))
    (should (equal (map-elt (car mentions) :path) "README.md")))

  ;; Multiple @ mentions
  (let ((mentions (agent-shell--parse-file-mentions "Compare @file1.txt with @file2.txt")))
    (should (= (length mentions) 2))
    (should (equal (map-elt (car mentions) :path) "file1.txt"))
    (should (equal (map-elt (cadr mentions) :path) "file2.txt")))

  ;; No @ mentions
  (let ((mentions (agent-shell--parse-file-mentions "No mentions here")))
    (should (= (length mentions) 0))))

(ert-deftest agent-shell--build-content-blocks-test ()
  "Test agent-shell--build-content-blocks function."
  (let* ((temp-file (make-temp-file "agent-shell-test" nil ".txt"))
         (file-content "Test file content")
         (default-directory (file-name-directory temp-file))
         (file-name (file-name-nondirectory temp-file))
         (file-path (expand-file-name temp-file))
         (file-uri (concat "file://" file-path)))

    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert file-content))

          ;; Mock agent-shell-cwd
          (cl-letf (((symbol-function 'agent-shell-cwd)
                     (lambda () default-directory)))

            ;; Test with embedded context support and small file
            (let ((agent-shell--state (list
                                       (cons :prompt-capabilities '((:embedded-context . t))))))
              (let ((blocks (agent-shell--build-content-blocks (format "Analyze @%s" file-name))))
                (should (equal blocks
                               `(((type . "text")
                                  (text . "Analyze"))
                                 ((type . "resource")
                                  (resource . ((uri . ,file-uri)
                                               (text . ,file-content)
                                               (mimeType . "text/plain")))))))))

            ;; Test without embedded context support
            (let ((agent-shell--state (list
                                       (cons :prompt-capabilities nil))))
              (let ((blocks (agent-shell--build-content-blocks (format "Analyze @%s" file-name))))
                (should (equal blocks
                               `(((type . "text")
                                  (text . "Analyze"))
                                 ((type . "resource_link")
                                  (uri . ,file-uri)
                                  (name . ,file-name)
                                  (mimeType . "text/plain")
                                  (size . ,(file-attribute-size (file-attributes temp-file)))))))))

            ;; Test fallback by setting a very small file size limit
            (let ((agent-shell--state (list
                                       (cons :prompt-capabilities '((:embedded-context . t)))))
                  (agent-shell-embed-file-size-limit 5))
              (let ((blocks (agent-shell--build-content-blocks (format "Analyze @%s" file-name))))
                (should (equal blocks
                               `(((type . "text")
                                  (text . "Analyze"))
                                 ((type . "resource_link")
                                  (uri . ,file-uri)
                                  (name . ,file-name)
                                  (mimeType . "text/plain")
                                  (size . ,(file-attribute-size (file-attributes temp-file)))))))))

            ;; Test with no mentions
            (let ((agent-shell--state (list
                                       (cons :prompt-capabilities '((:embedded-context . t))))))
              (let ((blocks (agent-shell--build-content-blocks "No mentions here")))
                (should (equal blocks
                               '(((type . "text")
                                  (text . "No mentions here")))))))))

      (delete-file temp-file))))

(ert-deftest agent-shell--build-content-blocks-binary-file-test ()
  "Test agent-shell--build-content-blocks with binary PNG files."
  (let* ((temp-file (make-temp-file "agent-shell-test" nil ".png"))
         ;; Minimal valid 1x1 PNG file (69 bytes)
         (png-data (unibyte-string
                    #x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A ; PNG signature
                    #x00 #x00 #x00 #x0D #x49 #x48 #x44 #x52 ; IHDR chunk
                    #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x01
                    #x08 #x02 #x00 #x00 #x00 #x90 #x77 #x53
                    #xDE #x00 #x00 #x00 #x0C #x49 #x44 #x41 ; IDAT chunk
                    #x54 #x08 #xD7 #x63 #xF8 #xCF #xC0 #x00
                    #x00 #x03 #x01 #x01 #x00 #x18 #xDD #x8D
                    #xB4 #x00 #x00 #x00 #x00 #x49 #x45 #x4E ; IEND chunk
                    #x44 #xAE #x42 #x60 #x82))
         (default-directory (file-name-directory temp-file))
         (file-name (file-name-nondirectory temp-file))
         (file-path (expand-file-name temp-file))
         (file-uri (concat "file://" file-path)))

    (unwind-protect
        (progn
          ;; Write binary PNG data
          (with-temp-file temp-file
            (set-buffer-multibyte nil)
            (insert png-data))

          ;; Mock agent-shell-cwd
          (cl-letf (((symbol-function 'agent-shell-cwd)
                     (lambda () default-directory)))

            (if (display-images-p)
                ;; Graphical Emacs: image-supported-file-p recognises PNG,
                ;; so the image code-path is reachable.
                (progn
                  ;; Test with image and embedded context support - should use ContentBlock::Image
                  (let ((agent-shell--state (list
                                             (cons :prompt-capabilities '((:image . t) (:embedded-context . t))))))
                    (let ((blocks (agent-shell--build-content-blocks (format "Analyze @%s" file-name))))
                      ;; Should have text block and image block
                      (should (= (length blocks) 2))

                      ;; Check text block
                      (should (equal (map-elt (nth 0 blocks) 'type) "text"))
                      (should (equal (map-elt (nth 0 blocks) 'text) "Analyze"))

                      ;; Check image block
                      (let ((image-block (nth 1 blocks)))
                        (should (equal (map-elt image-block 'type) "image"))

                        ;; Check URI
                        (should (equal (map-elt image-block 'uri) file-uri))

                        ;; Check MIME type is image/png
                        (should (equal (map-elt image-block 'mimeType) "image/png"))

                        ;; Check content is base64-encoded (not raw binary)
                        (let ((content (map-elt image-block 'data)))
                          ;; Should be a string
                          (should (stringp content))
                          ;; Should not contain raw PNG signature
                          (should-not (string-match-p "\x89PNG" content))
                          ;; Should be base64 (alphanumeric + / + = padding)
                          (should (string-match-p "^[A-Za-z0-9+/\n]+=*$" content))
                          ;; Should be longer than original (base64 overhead)
                          (should (< 69 (length content)))))))

                  ;; Test without image capability - should use resource_link with correct mime type
                  (let ((agent-shell--state (list
                                             (cons :prompt-capabilities nil))))
                    (let ((blocks (agent-shell--build-content-blocks (format "Analyze @%s" file-name))))
                      (should (= (length blocks) 2))

                      (let ((resource-link (nth 1 blocks)))
                        (should (equal (map-elt resource-link 'type) "resource_link"))
                        (should (equal (map-elt resource-link 'uri) file-uri))
                        ;; Should have image/png mime type
                        (should (equal (map-elt resource-link 'mimeType) "image/png"))
                        (should (equal (map-elt resource-link 'name) file-name))
                        (should (equal (map-elt resource-link 'size) 69))))))

              ;; Non-graphical Emacs: image-supported-file-p is unavailable,
              ;; so the PNG is treated as text/plain by the MIME resolver.
              ;; Verify the resource_link fallback still works.
              (let ((agent-shell--state (list
                                         (cons :prompt-capabilities '((:image . t) (:embedded-context . t))))))
                (let ((blocks (agent-shell--build-content-blocks (format "Analyze @%s" file-name))))
                  (should (= (length blocks) 2))

                  ;; Text block is still present
                  (should (equal (map-elt (nth 0 blocks) 'type) "text"))
                  (should (equal (map-elt (nth 0 blocks) 'text) "Analyze"))

                  ;; Without image MIME detection the file is embedded as a
                  ;; resource (text/plain), not as an image block.
                  (let ((block (nth 1 blocks)))
                    (should (member (map-elt block 'type) '("resource" "resource_link")))))))))

      (delete-file temp-file))))

(ert-deftest agent-shell--collect-attached-files-test ()
  "Test agent-shell--collect-attached-files function."
  ;; Test with empty list
  (should (equal (agent-shell--collect-attached-files '()) '()))

  ;; Test with resource block
  (let ((blocks '(((type . "resource")
                   (resource . ((uri . "file:///path/to/file.txt")
                                (text . "content"))))
                  ((type . "text")
                   (text . "some text")))))
    (let ((uris (agent-shell--collect-attached-files blocks)))
      (should (= (length uris) 1))
      (should (equal (car uris) "file:///path/to/file.txt"))))

  ;; Test with resource_link block
  (let ((blocks '(((type . "resource_link")
                   (uri . "file:///path/to/file.txt")
                   (name . "file.txt"))
                  ((type . "text")
                   (text . "some text")))))
    (let ((uris (agent-shell--collect-attached-files blocks)))
      (should (= (length uris) 1))
      (should (equal (car uris) "file:///path/to/file.txt"))))

  ;; Test with multiple files
  (let ((blocks '(((type . "resource_link")
                   (uri . "file:///path/to/file1.txt"))
                  ((type . "text")
                   (text . " "))
                  ((type . "resource_link")
                   (uri . "file:///path/to/file2.txt")))))
    (let ((uris (agent-shell--collect-attached-files blocks)))
      (should (= (length uris) 2)))))

(ert-deftest agent-shell--send-command-integration-test ()
  "Integration test: verify agent-shell--send-command calls ACP correctly."
  (let ((sent-request nil)
        (agent-shell--state (list
                             (cons :client 'test-client)
                             (cons :session (list (cons :id "test-session")))
                             (cons :prompt-capabilities '((:embedded-context . t)))
                             (cons :buffer (current-buffer))
                             (cons :last-entry-type nil)
                             (cons :active-requests nil))))

    ;; Mock acp-send-request to capture what gets sent;
    ;; stub viewport--buffer to avoid interactive shell-buffer prompt in batch.
    (cl-letf (((symbol-function 'acp-send-request)
               (lambda (&rest args)
                 (setq sent-request args)))
              ((symbol-function 'agent-shell-viewport--buffer)
               (lambda (&rest _) nil)))

      ;; Send a simple command
      (agent-shell--send-command
       :prompt "Hello agent"
       :shell-buffer nil)

      ;; Verify request was sent
      (should sent-request)

      ;; Verify basic request structure
      (let* ((request (plist-get sent-request :request))
             (params (map-elt request :params))
             (prompt (map-elt params 'prompt)))
        (should prompt)
        (should (equal prompt '[((type . "text") (text . "Hello agent"))]))))))

(ert-deftest agent-shell--send-command-error-fallback-test ()
  "Test agent-shell--send-command falls back to plain text on build-content-blocks error."
  (let ((sent-request nil)
        (agent-shell--state (list
                             (cons :client 'test-client)
                             (cons :session (list (cons :id "test-session")))
                             (cons :prompt-capabilities '((:embedded-context . t)))
                             (cons :buffer (current-buffer))
                             (cons :last-entry-type nil)
                             (cons :active-requests nil))))

    ;; Mock build-content-blocks to throw an error;
    ;; stub viewport--buffer to avoid interactive shell-buffer prompt in batch.
    (cl-letf (((symbol-function 'agent-shell--build-content-blocks)
               (lambda (_prompt)
                 (error "Simulated error in build-content-blocks")))
              ((symbol-function 'acp-send-request)
               (lambda (&rest args)
                 (setq sent-request args)))
              ((symbol-function 'agent-shell-viewport--buffer)
               (lambda (&rest _) nil)))

      ;; First, verify that build-content-blocks actually throws an error
      (should-error (agent-shell--build-content-blocks "Test prompt")
                    :type 'error)

      ;; Now verify send-command handles the error gracefully
      (agent-shell--send-command
       :prompt "Test prompt with @file.txt"
       :shell-buffer nil)

      ;; Verify request was sent (fallback succeeded)
      (should sent-request)

      ;; Verify it fell back to plain text
      (let* ((request (plist-get sent-request :request))
             (params (map-elt request :params))
             (prompt (map-elt params 'prompt)))
        ;; Should still have a prompt
        (should prompt)
        ;; Should be a single text block with the original prompt
        (should (equal prompt '[((type . "text") (text . "Test prompt with @file.txt"))]))))))

(ert-deftest agent-shell--send-command-emits-turn-complete-event-test ()
  "Test `agent-shell--send-command' emits turn-complete on success."
  (let ((received-events nil)
        (captured-on-success nil)
        (agent-shell--state (list (cons :buffer (current-buffer))
                                  (cons :event-subscriptions nil)
                                  (cons :client 'test-client)
                                  (cons :session (list (cons :id "test-session")))
                                  (cons :last-entry-type nil)
                                  (cons :tool-calls nil)
                                  (cons :usage (list (cons :total-tokens 0)))))
        (agent-shell-show-busy-indicator nil)
        (agent-shell-show-usage-at-turn-end nil))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'agent-shell--send-request)
               (lambda (&rest args)
                 (setq captured-on-success (plist-get args :on-success))))
              ((symbol-function 'shell-maker-finish-output)
               (lambda (&rest _)))
              ((symbol-function 'agent-shell--process-pending-request)
               (lambda (&rest _))))
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer)
       :event 'turn-complete
       :on-event (lambda (event)
                   (push event received-events)))
      (agent-shell--send-command
       :prompt "Hello"
       :shell-buffer (current-buffer))
      ;; Simulate the ACP response arriving
      (should captured-on-success)
      (funcall captured-on-success
               `((stopReason . "end_turn")
                 (usage . ((totalTokens . 1500)))))
      (should (= (length received-events) 1))
      (let ((data (map-elt (car received-events) :data)))
        (should (equal (map-elt data :stop-reason) "end_turn"))
        (should (equal (map-elt (map-elt data :usage) :total-tokens)
                       1500))))))

(ert-deftest agent-shell--format-diff-as-text-test ()
  "Test `agent-shell--format-diff-as-text' function."
  ;; Test nil input
  (should (equal (agent-shell--format-diff-as-text nil) nil))

  ;; Test basic diff formatting
  (let* ((old-text "line 1\nline 2\nline 3\n")
         (new-text "line 1\nline 2 modified\nline 3\n")
         (diff-info `((:old . ,old-text)
                      (:new . ,new-text)
                      (:file . "test.txt")))
         (result (agent-shell--format-diff-as-text diff-info)))

    ;; Should return a string
    (should (stringp result))

    ;; Should NOT contain file header lines with timestamps (they should be stripped)
    (should-not (string-match-p "^---" result))
    (should-not (string-match-p "^\\+\\+\\+" result))

    ;; Should contain unified diff hunk headers
    (should (string-match-p "^@@" result))

    ;; Should contain the actual changes
    (should (string-match-p "^-line 2" result))
    (should (string-match-p "^\\+line 2 modified" result))

    ;; Should have syntax highlighting (text properties)
    (let ((has-diff-face nil))
      (dotimes (i (length result))
        (when (get-text-property i 'font-lock-face result)
          (setq has-diff-face t)))
      (should has-diff-face))))

(ert-deftest agent-shell--format-agent-capabilities-test ()
  "Test `agent-shell--format-agent-capabilities' function."
  ;; Test with multiple capabilities (includes comma)
  (let ((capabilities '((promptCapabilities (image . t) (audio . :false) (embeddedContext . t))
                        (mcpCapabilities (http . t) (sse . t)))))
    (should (equal (substring-no-properties
                    (agent-shell--format-agent-capabilities capabilities))
                   (concat
                    "prompt  image and embedded context\n"
                    "mcp     http and sse"))))

  ;; Test with single capability per category (no comma)
  (let ((capabilities '((promptCapabilities (image . t))
                        (mcpCapabilities (http . t)))))
    (should (equal (substring-no-properties
                    (agent-shell--format-agent-capabilities capabilities))
                   (concat "prompt  image\n"
                           "mcp     http"))))

  ;; Test with top-level boolean capability (loadSession)
  (let ((capabilities '((loadSession . t)
                        (promptCapabilities (image . t) (embeddedContext . t)))))
    (should (equal (substring-no-properties
                    (agent-shell--format-agent-capabilities capabilities))
                   (concat "load session\n"
                           "prompt        image and embedded context"))))

  ;; Test with sessionCapabilities (bare keys without boolean values)
  (let ((capabilities '((promptCapabilities (image . t) (embeddedContext . t))
                        (mcpCapabilities (http . t) (sse . t))
                        (sessionCapabilities (fork) (list) (resume)))))
    (should (equal (substring-no-properties
                    (agent-shell--format-agent-capabilities capabilities))
                   (concat "prompt   image and embedded context\n"
                           "mcp      http and sse\n"
                           "session  fork, list and resume"))))

  ;; Test with all capabilities disabled (should return empty string)
  (let ((capabilities '((promptCapabilities (image . :false) (audio . :false)))))
    (should (equal (agent-shell--format-agent-capabilities capabilities) ""))))

(ert-deftest agent-shell--make-transcript-tool-call-entry-test ()
  "Test `agent-shell--make-transcript-tool-call-entry' function."
  ;; Mock format-time-string to return a predictable value
  (cl-letf (((symbol-function 'format-time-string)
             (lambda (format &optional _time _zone)
               (cond
                ((string= format "%F %T") "2025-11-02 18:17:41")
                (t (error "Unexpected format-time-string format: %s" format))))))

    ;; Test with all parameters provided
    (let ((entry (agent-shell--make-transcript-tool-call-entry
                  :status "completed"
                  :title "grep \"transcript\""
                  :kind "search"
                  :description "Search for transcript references"
                  :command "grep \"transcript\""
                  :output "Found 6 files\n/path/to/file1.md\n/path/to/file2.md")))
      (should (equal entry "\n\n### Tool Call [completed]: grep \"transcript\"

**Tool:** search
**Timestamp:** 2025-11-02 18:17:41
**Description:** Search for transcript references
**Command:** grep \"transcript\"

```
Found 6 files
/path/to/file1.md
/path/to/file2.md
```
")))

    ;; Test with minimal parameters
    (let ((entry (agent-shell--make-transcript-tool-call-entry
                  :status "completed"
                  :title "test command"
                  :output "simple output")))
      (should (equal entry "\n\n### Tool Call [completed]: test command

**Timestamp:** 2025-11-02 18:17:41

```
simple output
```
")))

    ;; Test with nil status and title
    (let ((entry (agent-shell--make-transcript-tool-call-entry
                  :status nil
                  :title nil
                  :output "output")))
      (should (equal entry "

### Tool Call [no status]: \n
**Timestamp:** 2025-11-02 18:17:41

```
output
```
")))

    ;; Test that output whitespace is trimmed
    (let ((entry (agent-shell--make-transcript-tool-call-entry
                  :status "completed"
                  :title "test"
                  :output "  \n  output with spaces  \n  ")))
      (should (equal entry "\n\n### Tool Call [completed]: test

**Timestamp:** 2025-11-02 18:17:41

```
output with spaces
```
")))

    ;; Test that code blocks in output are stripped and output containing backtick fences gets a longer outer fence
    (let ((entry (agent-shell--make-transcript-tool-call-entry
                  :status "completed"
                  :title "test"
                  :output "```\ncode block content\n```")))
      (should (equal entry "

### Tool Call [completed]: test

**Timestamp:** 2025-11-02 18:17:41

````
```
code block content
```
````
")))

    ;; Test that output containing backtick fences with whitespace is trimmed and output containing backtick fences gets a longer outer fence
    (let ((entry (agent-shell--make-transcript-tool-call-entry
                  :status "completed"
                  :title "test"
                  :output "  \n  ```\ncode block content with spaces\n```\n")))
      (should (equal entry "

### Tool Call [completed]: test

**Timestamp:** 2025-11-02 18:17:41

````
```
code block content with spaces
```
````
")))

    ;; Test output with 4-backtick fences gets 5-backtick outer fence
    (let ((entry (agent-shell--make-transcript-tool-call-entry
                  :status "completed"
                  :title "test"
                  :output "````\ncode block content\n````")))
      (should (equal entry "\n\n### Tool Call [completed]: test

**Timestamp:** 2025-11-02 18:17:41

`````
````
code block content
````
`````
")))))

(ert-deftest agent-shell--longest-backtick-run-test ()
  "Test `agent-shell--longest-backtick-run'."
  (should (= (agent-shell--longest-backtick-run "") 0))
  (should (= (agent-shell--longest-backtick-run "no backticks here") 0))
  (should (= (agent-shell--longest-backtick-run "has `one` inline") 1))
  (should (= (agent-shell--longest-backtick-run "has ``` three") 3))
  (should (= (agent-shell--longest-backtick-run "```elisp\n(foo)\n```") 3))
  (should (= (agent-shell--longest-backtick-run "has ```` four and ``` three") 4))
  (should (= (agent-shell--longest-backtick-run "``````") 6)))

(ert-deftest agent-shell--indent-markdown-headers-test ()
  "Test `agent-shell--indent-markdown-headers'."
  ;; Text without headers is unchanged.
  (should (equal (agent-shell--indent-markdown-headers "no headers here")
                 "no headers here"))
  ;; Simple H1 becomes H3.
  (should (equal (agent-shell--indent-markdown-headers "# Foo")
                 "### Foo"))
  ;; H2 becomes H4.
  (should (equal (agent-shell--indent-markdown-headers "## Bar")
                 "#### Bar"))
  ;; H4 becomes H6.
  (should (equal (agent-shell--indent-markdown-headers "#### Deep")
                 "###### Deep"))
  ;; H5 is capped at H6.
  (should (equal (agent-shell--indent-markdown-headers "##### Five")
                 "###### Five"))
  ;; H6 stays at H6.
  (should (equal (agent-shell--indent-markdown-headers "###### Six")
                 "###### Six"))
  ;; Mixed content with multiple headers.
  (should (equal (agent-shell--indent-markdown-headers
                  "some text\n# Heading 1\nmore text\n## Heading 2\nend")
                 "some text\n### Heading 1\nmore text\n#### Heading 2\nend"))
  ;; Headers inside code blocks are left unchanged.
  (should (equal (agent-shell--indent-markdown-headers
                  "before\n```\n# code comment\n## also code\n```\nafter")
                 "before\n```\n# code comment\n## also code\n```\nafter"))
  ;; Headers outside code blocks are indented, inside are not.
  (should (equal (agent-shell--indent-markdown-headers
                  "# Top\n```\n# Inside\n```\n# Bottom")
                 "### Top\n```\n# Inside\n```\n### Bottom"))
  ;; Code blocks with 4+ backticks.
  (should (equal (agent-shell--indent-markdown-headers
                  "````\n# Inside\n````\n# Outside")
                 "````\n# Inside\n````\n### Outside"))
  ;; Nested code blocks (inner fence shorter than outer).
  (should (equal (agent-shell--indent-markdown-headers
                  "````\n```\n# Inside\n```\n````\n# Outside")
                 "````\n```\n# Inside\n```\n````\n### Outside"))
  ;; Nil input returns empty string.
  (should (equal (agent-shell--indent-markdown-headers nil) ""))
  ;; Empty string.
  (should (equal (agent-shell--indent-markdown-headers "") ""))
  ;; Hash without space is not a header.
  (should (equal (agent-shell--indent-markdown-headers "#not-a-header")
                 "#not-a-header"))
  ;; Simulated LLM output with mixed headers and code blocks.
  ;; This is the primary transcript use case: an agent response containing
  ;; its own markdown structure that must be indented to stay below the
  ;; transcript's ## section headers.
  (should (equal (agent-shell--indent-markdown-headers
                  (concat "Here's my analysis:\n"
                          "# Summary\n"
                          "Some text\n"
                          "## Details\n"
                          "More text\n"
                          "```elisp\n"
                          "# this is a comment in code\n"
                          "(defun foo () nil)\n"
                          "```\n"
                          "### Conclusion\n"
                          "Final thoughts"))
                 (concat "Here's my analysis:\n"
                          "### Summary\n"
                          "Some text\n"
                          "#### Details\n"
                          "More text\n"
                          "```elisp\n"
                          "# this is a comment in code\n"
                          "(defun foo () nil)\n"
                          "```\n"
                          "##### Conclusion\n"
                          "Final thoughts")))
  ;; Tool call entries (### Tool Call) are NOT passed through this function
  ;; because they are code-generated, not LLM output.  Verify that if
  ;; they hypothetically were, they would be indented -- this confirms the
  ;; function is agnostic and the correct behavior comes from applying it
  ;; only to LLM text.
  (should (equal (agent-shell--indent-markdown-headers "### Tool Call [completed]: grep")
                 "##### Tool Call [completed]: grep")))

(ert-deftest agent-shell-mcp-servers-test ()
  "Test `agent-shell-mcp-servers' function normalization."
  ;; Test with nil
  (let ((agent-shell-mcp-servers nil))
    (should (equal (agent-shell--mcp-servers) nil)))

  ;; Test with empty list
  (let ((agent-shell-mcp-servers '()))
    (should (equal (agent-shell--mcp-servers) nil)))

  ;; Test stdio transport with lists that need normalization
  (let ((agent-shell-mcp-servers
         '(((name . "filesystem")
            (command . "npx")
            (args . ("-y" "@modelcontextprotocol/server-filesystem" "/tmp"))
            (env . (((name . "DEBUG") (value . "true"))
                    ((name . "LOG_LEVEL") (value . "info"))))))))
    (should (equal (agent-shell--mcp-servers)
                   [((name . "filesystem")
                     (command . "npx")
                     (args . ["-y" "@modelcontextprotocol/server-filesystem" "/tmp"])
                     (env . [((name . "DEBUG") (value . "true"))
                             ((name . "LOG_LEVEL") (value . "info"))]))])))

  ;; Test HTTP transport with lists that need normalization
  (let ((agent-shell-mcp-servers
         '(((name . "notion")
            (type . "http")
            (url . "https://mcp.notion.com/mcp")
            (headers . (((name . "Authorization") (value . "Bearer token"))
                        ((name . "Content-Type") (value . "application/json"))))))))
    (should (equal (agent-shell--mcp-servers)
                   [((name . "notion")
                     (type . "http")
                     (url . "https://mcp.notion.com/mcp")
                     (headers . [((name . "Authorization") (value . "Bearer token"))
                                 ((name . "Content-Type") (value . "application/json"))]))])))

  ;; Test empty list fields normalize to empty vectors
  (let ((agent-shell-mcp-servers
         '(((name . "empty")
            (command . "npx")
            (args . ())
            (env . ())
            (headers . ())))))
    (should (equal (agent-shell--mcp-servers)
                   [((name . "empty")
                     (command . "npx")
                     (args . [])
                     (env . [])
                     (headers . []))])))

  ;; Test with already-vectorized fields (should remain unchanged)
  (let ((agent-shell-mcp-servers
         '(((name . "filesystem")
            (command . "npx")
            (args . ["-y" "@modelcontextprotocol/server-filesystem" "/tmp"])
            (env . [])))))
    (should (equal (agent-shell--mcp-servers)
                   [((name . "filesystem")
                     (command . "npx")
                     (args . ["-y" "@modelcontextprotocol/server-filesystem" "/tmp"])
                     (env . []))])))

  ;; Test multiple servers
  (let ((agent-shell-mcp-servers
         '(((name . "notion")
            (type . "http")
            (url . "https://mcp.notion.com/mcp")
            (headers . ()))
           ((name . "filesystem")
            (command . "npx")
            (args . ("-y" "@modelcontextprotocol/server-filesystem" "/tmp"))
            (env . ())))))
    (should (equal (agent-shell--mcp-servers)
                   [((name . "notion")
                     (type . "http")
                     (url . "https://mcp.notion.com/mcp")
                     (headers . []))
                    ((name . "filesystem")
                     (command . "npx")
                     (args . ["-y" "@modelcontextprotocol/server-filesystem" "/tmp"])
                     (env . []))])))

  ;; Test server without optional fields
  (let ((agent-shell-mcp-servers
         '(((name . "simple")
            (command . "simple-server")))))
    (should (equal (agent-shell--mcp-servers)
                   [((name . "simple")
                     (command . "simple-server"))]))))

(ert-deftest agent-shell--completion-bounds-test ()
  "Test `agent-shell--completion-bounds' function."
  (let ((path-chars "[:alnum:]/_.-"))

    ;; Test finding bounds after @ trigger
    (with-temp-buffer
      (insert "@file.txt")
      (goto-char (point-min))
      (forward-char 1)
      (let ((bounds (agent-shell--completion-bounds path-chars ?@)))
        (should bounds)
        (should (equal (map-elt bounds :start) 2))  ; start after @
        (should (equal (map-elt bounds :end) 10)))) ; end of file.txt

    ;; Test with cursor in middle of word
    (with-temp-buffer
      (insert "@some/path/file.el")
      (goto-char 8)
      (let ((bounds (agent-shell--completion-bounds path-chars ?@)))
        (should bounds)
        (should (equal (map-elt bounds :start) 2))
        (should (equal (map-elt bounds :end) 19))))

    ;; Test returns nil when trigger character is missing
    (with-temp-buffer
      (insert "file.txt")
      (goto-char (point-min))
      (let ((bounds (agent-shell--completion-bounds path-chars ?@)))
        (should-not bounds)))

    ;; Test with empty word after trigger
    (with-temp-buffer
      (insert "@ ")
      (goto-char 2) ; Right after @
      (let ((bounds (agent-shell--completion-bounds path-chars ?@)))
        (should bounds)
        (should (equal (map-elt bounds :start) 2))
        (should (equal (map-elt bounds :end) 2)))) ; Empty range

    ;; Test with text before trigger
    (with-temp-buffer
      (insert "Look at @README.md please")
      (goto-char 12) ; In middle of README
      (let ((bounds (agent-shell--completion-bounds path-chars ?@)))
        (should bounds)
        (should (equal (map-elt bounds :start) 10))
        (should (equal (map-elt bounds :end) 19))))))

(ert-deftest agent-shell--capf-exit-with-space-test ()
  "Test `agent-shell--capf-exit-with-space' function."
  (with-temp-buffer
    (insert "test")
    (agent-shell--capf-exit-with-space "ignored" 'finished)
    (should (equal (buffer-string) "test "))
    (should (equal (point) 6))))

(ert-deftest agent-shell-subscribe-to-test ()
  "Test `agent-shell-subscribe-to' and event dispatching."
  (let* ((received-events nil)
         (agent-shell--state (list (cons :buffer (current-buffer))
                                   (cons :event-subscriptions nil))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer)
       :on-event (lambda (event)
                   (push event received-events)))

      (agent-shell--emit-event :event 'init-client)
      (agent-shell--emit-event :event 'init-session)
      (agent-shell--emit-event :event 'init-model)

      (should (= (length received-events) 3))

      ;; Events are pushed, so most recent is first
      (should (equal (map-elt (nth 2 received-events) :event) 'init-client))
      (should (equal (map-elt (nth 1 received-events) :event) 'init-session))
      (should (equal (map-elt (nth 0 received-events) :event) 'init-model)))))

(ert-deftest agent-shell-subscribe-to-filtered-test ()
  "Test `agent-shell-subscribe-to' with :event filter."
  (let* ((received-events nil)
         (agent-shell--state (list (cons :buffer (current-buffer))
                                   (cons :event-subscriptions nil))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer)
       :event 'init-session
       :on-event (lambda (event)
                   (push event received-events)))

      (agent-shell--emit-event :event 'init-client)
      (agent-shell--emit-event :event 'init-session)
      (agent-shell--emit-event :event 'init-client)
      (agent-shell--emit-event :event 'init-session)

      ;; Only init-session events should be received
      (should (= (length received-events) 2))
      (should (equal (map-elt (nth 0 received-events) :event) 'init-session))
      (should (equal (map-elt (nth 1 received-events) :event) 'init-session)))))

(ert-deftest agent-shell-unsubscribe-test ()
  "Test `agent-shell-unsubscribe' removes subscription."
  (let* ((received-events nil)
         (agent-shell--state (list (cons :buffer (current-buffer))
                                   (cons :event-subscriptions nil))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (let ((token (agent-shell-subscribe-to
                    :shell-buffer (current-buffer)
                    :on-event (lambda (event)
                                (push event received-events)))))

        (agent-shell--emit-event :event 'init-client)
        (should (= (length received-events) 1))

        (agent-shell-unsubscribe :subscription token)

        (agent-shell--emit-event :event 'init-session)
        ;; Should still be 1 — no new events after unsubscribe
        (should (= (length received-events) 1))))))

(ert-deftest agent-shell--emit-event-with-data-test ()
  "Test `agent-shell--emit-event' passes :data to subscribers."
  (let* ((received-events nil)
         (agent-shell--state (list (cons :buffer (current-buffer))
                                   (cons :event-subscriptions nil))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer)
       :on-event (lambda (event)
                   (push event received-events)))

      (agent-shell--emit-event
       :event 'file-write
       :data (list (cons :path "/tmp/test.txt")
                   (cons :content "hello")))

      (should (= (length received-events) 1))
      (let ((event (car received-events)))
        (should (equal (map-elt event :event) 'file-write))
        (should (equal (map-elt (map-elt event :data) :path) "/tmp/test.txt"))
        (should (equal (map-elt (map-elt event :data) :content) "hello"))))))

(ert-deftest agent-shell--emit-event-data-omitted-when-nil-test ()
  "Test `agent-shell--emit-event' omits :data when nil."
  (let* ((received-events nil)
         (agent-shell--state (list (cons :buffer (current-buffer))
                                   (cons :event-subscriptions nil))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer)
       :on-event (lambda (event)
                   (push event received-events)))

      (agent-shell--emit-event :event 'init-client)

      (should (= (length received-events) 1))
      (let ((event (car received-events)))
        (should (equal (map-elt event :event) 'init-client))
        (should-not (assoc :data event))))))

(ert-deftest agent-shell--emit-event-no-subscribers-test ()
  "Test `agent-shell--emit-event' works with no subscribers."
  (let ((agent-shell--state (list (cons :buffer (current-buffer))
                                  (cons :event-subscriptions nil))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      ;; Should not error when no subscriptions exist
      (agent-shell--emit-event :event 'init-client))))

(ert-deftest agent-shell-subscribe-to-prompt-ready-test ()
  "Test subscribing to `prompt-ready' event."
  (let* ((received-events nil)
         (agent-shell--state (list (cons :buffer (current-buffer))
                                   (cons :event-subscriptions nil))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer)
       :event 'prompt-ready
       :on-event (lambda (event)
                   (push event received-events)))

      ;; Other events should not be received.
      (agent-shell--emit-event :event 'init-session)
      (agent-shell--emit-event :event 'init-finished)
      (should (= (length received-events) 0))

      ;; prompt-ready should be received.
      (agent-shell--emit-event :event 'prompt-ready)
      (should (= (length received-events) 1))
      (should (equal (map-elt (nth 0 received-events) :event) 'prompt-ready)))))

(ert-deftest agent-shell-idle-event-fires-after-timeout-test ()
  "Test that idle event fires after timeout following a trigger event."
  (with-temp-buffer
    (let ((agent-shell--state (list (cons :buffer (current-buffer))
                                    (cons :event-subscriptions nil)
                                    (cons :idle-timer nil)))
          (agent-shell-idle-timeout 0.01)
          (fired nil))
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () agent-shell--state)))
        (agent-shell-subscribe-to
         :shell-buffer (current-buffer)
         :event 'idle
         :on-event (lambda (_event) (setq fired t)))
        (agent-shell--start-idle-timer :event 'permission-request)
        (sit-for 0.05)
        (should fired)))))

(ert-deftest agent-shell-idle-event-does-not-fire-immediately-test ()
  "Test that idle event does not fire synchronously."
  (with-temp-buffer
    (let ((agent-shell--state (list (cons :buffer (current-buffer))
                                    (cons :event-subscriptions nil)
                                    (cons :idle-timer nil)))
          (agent-shell-idle-timeout 999)
          (fired nil))
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () agent-shell--state)))
        (agent-shell-subscribe-to
         :shell-buffer (current-buffer)
         :event 'idle
         :on-event (lambda (_event) (setq fired t)))
        (agent-shell--start-idle-timer :event 'permission-request)
        (should-not fired)))))

(ert-deftest agent-shell-idle-event-cancelled-by-activity-test ()
  "Test that activity cancels the idle timer."
  (with-temp-buffer
    (let ((agent-shell--state (list (cons :buffer (current-buffer))
                                    (cons :event-subscriptions nil)
                                    (cons :idle-timer nil)))
          (agent-shell-idle-timeout 0.01)
          (fired nil))
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () agent-shell--state)))
        (agent-shell-subscribe-to
         :shell-buffer (current-buffer)
         :event 'idle
         :on-event (lambda (_event) (setq fired t)))
        (agent-shell--start-idle-timer :event 'permission-request)
        (agent-shell--cancel-idle-timer)
        (sit-for 0.05)
        (should-not fired)))))

(ert-deftest agent-shell-idle-event-rearms-on-new-trigger-test ()
  "Test that re-firing a trigger event restarts the idle timer."
  (with-temp-buffer
    (let ((agent-shell--state (list (cons :buffer (current-buffer))
                                    (cons :event-subscriptions nil)
                                    (cons :idle-timer nil)))
          (agent-shell-idle-timeout 0.05)
          (count 0))
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () agent-shell--state)))
        (agent-shell-subscribe-to
         :shell-buffer (current-buffer)
         :event 'idle
         :on-event (lambda (_event) (setq count (1+ count))))
        (agent-shell--start-idle-timer :event 'permission-request)
        (sit-for 0.02)
        (agent-shell--start-idle-timer :event 'permission-request)
        (sit-for 0.08)
        (should (= count 1))))))

(ert-deftest agent-shell-idle-event-defaults-to-30-when-nil-test ()
  "Test that idle timer falls back to 30 seconds when timeout is nil."
  (with-temp-buffer
    (let ((agent-shell--state (list (cons :buffer (current-buffer))
                                    (cons :event-subscriptions nil)
                                    (cons :idle-timer nil)))
          (agent-shell-idle-timeout nil))
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () agent-shell--state)))
        (agent-shell--start-idle-timer :event 'permission-request)
        (should (timerp (map-elt agent-shell--state :idle-timer)))))))

(ert-deftest agent-shell-idle-event-adds-missing-idle-timer-state-test ()
  "Test idle timer works with legacy alist state lacking :idle-timer."
  (with-temp-buffer
    (let ((agent-shell--state (list (cons :buffer (current-buffer))
                                    (cons :event-subscriptions nil)))
          (agent-shell-idle-timeout 999))
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () agent-shell--state)))
        (agent-shell--start-idle-timer :event 'permission-request)
        (should (timerp (map-elt agent-shell--state :idle-timer)))
        (agent-shell--cancel-idle-timer)
        (should-not (map-elt agent-shell--state :idle-timer))))))

(ert-deftest agent-shell-idle-event-per-event-timeout-test ()
  "Test that idle timer uses per-event timeout from alist."
  (with-temp-buffer
    (let ((agent-shell--state (list (cons :buffer (current-buffer))
                                    (cons :event-subscriptions nil)
                                    (cons :idle-timer nil)))
          (agent-shell-idle-timeout '((permission-request . 0.01)
                                      (turn-complete . 999)))
          (fired nil))
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () agent-shell--state)))
        (agent-shell-subscribe-to
         :shell-buffer (current-buffer)
         :event 'idle
         :on-event (lambda (_event) (setq fired t)))
        (agent-shell--start-idle-timer :event 'permission-request)
        (sit-for 0.05)
        (should fired)))))

(ert-deftest agent-shell-idle-event-includes-trigger-and-buffer-test ()
  "Test that idle event data includes the trigger event and buffer."
  (with-temp-buffer
    (let ((agent-shell--state (list (cons :buffer (current-buffer))
                                    (cons :event-subscriptions nil)
                                    (cons :idle-timer nil)))
          (agent-shell-idle-timeout 0.01)
          (buf (current-buffer))
          (received nil))
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () agent-shell--state)))
        (agent-shell-subscribe-to
         :shell-buffer (current-buffer)
         :event 'idle
         :on-event (lambda (event) (setq received event)))
        (agent-shell--start-idle-timer :event 'turn-complete)
        (sit-for 0.05)
        (should (equal (map-nested-elt received '(:data :idle-event))
                       'turn-complete))
        (should (equal (map-nested-elt received '(:data :buffer))
                       buf))))))

(ert-deftest agent-shell-dwim-carries-context-to-first-viewport-open-test ()
  "Test `agent-shell--dwim' carries context into deferred viewport open."
  (let ((agent-shell-prefer-viewport-interaction t))
    (with-temp-buffer
      (let ((source-buffer (current-buffer))
            (show-buffer-args nil)
            (shell-buffer (generate-new-buffer " *agent-shell shell*")))
        (unwind-protect
            (progn
              (with-current-buffer shell-buffer
                (setq-local agent-shell-session-strategy 'prompt)
                (setq-local agent-shell--state
                            `((:buffer . ,shell-buffer)
                              (:session . ((:id . nil)))
                              (:event-subscriptions . nil))))
              (cl-letf (((symbol-function 'derived-mode-p)
                         (lambda (&rest modes)
                           (and (eq (current-buffer) shell-buffer)
                                (memq 'agent-shell-mode modes))))
                        ((symbol-function 'agent-shell--shell-buffer)
                         (lambda (&rest _) shell-buffer))
                        ((symbol-function 'agent-shell--context)
                         (lambda (&key shell-buffer)
                           (ignore shell-buffer)
                           (when (eq (current-buffer) source-buffer)
                             "context from source")))
                        ((symbol-function 'agent-shell-viewport--show-buffer)
                         (lambda (&rest args)
                           (setq show-buffer-args args))))
                (with-current-buffer source-buffer
                  (agent-shell--dwim))
                (should-not show-buffer-args)
                (with-current-buffer shell-buffer
                  (agent-shell--emit-event :event 'session-selected))
                (should (equal (plist-get show-buffer-args :shell-buffer) shell-buffer))
                (should (equal (plist-get show-buffer-args :append)
                               "context from source"))))
          (kill-buffer shell-buffer))))))

(ert-deftest agent-shell--on-request-emits-permission-request-event-test ()
  "Test `agent-shell--on-request' emits permission-request event."
  (let ((received-events nil)
        (agent-shell--state (list (cons :buffer (current-buffer))
                                  (cons :event-subscriptions nil)
                                  (cons :tool-calls nil)
                                  (cons :last-entry-type nil))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest _))))
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer)
       :event 'permission-request
       :on-event (lambda (event)
                   (push event received-events)))
      (agent-shell--on-request
       :state agent-shell--state
       :acp-request `((id . "req-123")
                      (method . "session/request_permission")
                      (params . ((toolCall . ((toolCallId . "tc-456")
                                              (title . "Run command")
                                              (status . "pending")
                                              (kind . "bash")))))))
      (should (= (length received-events) 1))
      (let ((data (map-elt (car received-events) :data)))
        (should (equal (map-elt data :request-id) "req-123"))
        (should (equal (map-elt data :tool-call-id) "tc-456"))
        (should (equal (map-elt (map-elt data :tool-call) :title)
                       "Run command"))))))

(ert-deftest agent-shell-mode-hook-subscriptions-survive-state-init ()
  "Subscriptions registered via `agent-shell-mode-hook' should persist."
  (let ((test-buffer nil)
        (hook-fn (lambda ()
                   (agent-shell-subscribe-to
                    :shell-buffer (current-buffer)
                    :event 'turn-complete
                    :on-event #'ignore)))
        (fake-process (start-process "fake-agent" nil "cat"))
        (config (list (cons :buffer-name "test-agent")
                      (cons :client-maker
                            (lambda (_buf)
                              (list (cons :command "cat")))))))
    (unwind-protect
        (progn
          (add-hook 'agent-shell-mode-hook hook-fn)
          (cl-letf (((symbol-function 'shell-maker-start)
                     (lambda (_config &rest _args)
                       (setq test-buffer (get-buffer-create "*test-agent-shell*"))
                       (with-current-buffer test-buffer
                         (setq major-mode 'agent-shell-mode)
                         (run-hooks 'agent-shell-mode-hook))
                       test-buffer))
                    ((symbol-function 'shell-maker--process) (lambda () fake-process))
                    ((symbol-function 'shell-maker-finish-output) #'ignore)
                    ((symbol-function 'agent-shell--handle) #'ignore)
                    (agent-shell-file-completion-enabled nil))
            (let* ((shell-buffer (agent-shell--start :config config
                                                     :no-focus t
                                                     :new-session t))
                   (subs (map-elt (buffer-local-value 'agent-shell--state shell-buffer)
                                  :event-subscriptions)))
              (should (seq-find (lambda (sub)
                                  (eq 'turn-complete (map-elt sub :event)))
                                subs)))))
      (remove-hook 'agent-shell-mode-hook hook-fn)
      (when (process-live-p fake-process)
        (delete-process fake-process))
      (when (and test-buffer (buffer-live-p test-buffer))
        (kill-buffer test-buffer)))))

(ert-deftest agent-shell--initiate-session-prefers-list-and-load-when-supported ()
  "Test `agent-shell--initiate-session' prefers session/list + session/load."
  (with-temp-buffer
    (let* ((agent-shell-session-strategy 'latest)
           (requests '())
           (session-init-called nil)
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:session . ((:id . nil)
                                 (:mode-id . nil)
                                 (:modes . nil)))
                    (:supports-session-list . t)
                    (:supports-session-load . t)
                    (:active-requests)
                    (:event-subscriptions . nil))))
      (setq-local agent-shell--state state)
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () agent-shell--state))
                ((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest _args) nil))
                ((symbol-function 'agent-shell--update-header-and-mode-line)
                 (lambda () nil))
                ((symbol-function 'agent-shell-cwd)
                 (lambda () "/tmp"))
                ((symbol-function 'agent-shell--resolve-path)
                 (lambda (path) path))
                ((symbol-function 'agent-shell--mcp-servers)
                 (lambda () []))
                ((symbol-function 'acp-send-request)
                 (lambda (&rest args)
                   (push args requests)
                   (let* ((request (plist-get args :request))
                          (method (map-elt request :method)))
                     (pcase method
                       ("session/list"
                        (funcall (plist-get args :on-success)
                                 '((sessions . [((sessionId . "session-123")
                                                 (cwd . "/tmp")
                                                 (title . "Recent session"))]))))
                       ("session/load"
                        (funcall (plist-get args :on-success)
                                 '((modes (currentModeId . "default")
                                          (availableModes . [((id . "default")
                                                              (name . "Default")
                                                              (description . "Default mode"))]))
                                   (models (currentModelId . "gpt-5")
                                           (availableModels . [((modelId . "gpt-5")
                                                                (name . "GPT-5")
                                                                (description . "Test model"))])))))
                       (_ (error "Unexpected method: %s" method)))))))
        (agent-shell--initiate-session
         :shell-buffer (current-buffer)
         :on-session-init (lambda ()
                            (setq session-init-called t)))
        (let ((ordered-requests (nreverse requests)))
          (should (equal (mapcar (lambda (req)
                                   (map-elt (plist-get req :request) :method))
                                 ordered-requests)
                         '("session/list" "session/load")))
          (let* ((load-request (plist-get (nth 1 ordered-requests) :request))
                 (load-params (map-elt load-request :params)))
            (should (equal (map-elt load-params 'sessionId) "session-123"))
            (should (equal (map-elt load-params 'cwd) "/tmp"))))
        (should session-init-called)
        (should (equal (map-nested-elt agent-shell--state '(:session :id)) "session-123"))))))

(ert-deftest agent-shell--initiate-session-falls-back-to-new-on-list-failure ()
  "Test `agent-shell--initiate-session' falls back to session/new on list failure."
  (with-temp-buffer
    (let* ((agent-shell-session-strategy 'latest)
           (requests '())
           (session-init-called nil)
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:session . ((:id . nil)
                                 (:mode-id . nil)
                                 (:modes . nil)))
                    (:supports-session-list . t)
                    (:supports-session-load . t)
                    (:active-requests)
                    (:event-subscriptions . nil))))
      (setq-local agent-shell--state state)
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () agent-shell--state))
                ((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest _args) nil))
                ((symbol-function 'agent-shell--update-header-and-mode-line)
                 (lambda () nil))
                ((symbol-function 'agent-shell-cwd)
                 (lambda () "/tmp"))
                ((symbol-function 'agent-shell--resolve-path)
                 (lambda (path) path))
                ((symbol-function 'agent-shell--mcp-servers)
                 (lambda () []))
                ((symbol-function 'acp-send-request)
                 (lambda (&rest args)
                   (push args requests)
                   (let* ((request (plist-get args :request))
                          (method (map-elt request :method)))
                     (pcase method
                       ("session/list"
                        (funcall (plist-get args :on-failure)
                                 '((code . -32601)
                                   (message . "Method not found"))
                                 nil))
                       ("session/new"
                        (funcall (plist-get args :on-success)
                                 '((sessionId . "new-session-456"))))
                       (_ (error "Unexpected method: %s" method)))))))
        (agent-shell--initiate-session
         :shell-buffer (current-buffer)
         :on-session-init (lambda ()
                            (setq session-init-called t)))
        (let ((ordered-requests (nreverse requests)))
          (should (equal (mapcar (lambda (req)
                                   (map-elt (plist-get req :request) :method))
                                 ordered-requests)
                         '("session/list" "session/new"))))
        (should session-init-called)
        (should (equal (map-nested-elt agent-shell--state '(:session :id)) "new-session-456"))))))

(ert-deftest agent-shell--format-session-date-test ()
  "Test `agent-shell--format-session-date' humanizes timestamps."
  ;; Pin timezone to UTC so assertions are deterministic.
  (let ((orig-tz (getenv "TZ")))
    (unwind-protect
        (progn
          (set-time-zone-rule "UTC")
          ;; Today
          (let* ((now (current-time))
                 (today-iso (format-time-string "%Y-%m-%dT10:30:00Z" now)))
            (should (equal (agent-shell--format-session-date today-iso)
                           "Today, 10:30")))
          ;; Yesterday
          (let* ((yesterday (time-subtract (current-time) (* 24 60 60)))
                 (yesterday-iso (format-time-string "%Y-%m-%dT15:45:00Z" yesterday)))
            (should (equal (agent-shell--format-session-date yesterday-iso)
                           "Yesterday, 15:45")))
          ;; Same year, older
          (should (string-match-p "^[A-Z][a-z]+ [0-9]+, [0-9]+:[0-9]+"
                                   (agent-shell--format-session-date "2026-01-05T09:00:00Z")))
          ;; Different year
          (should (string-match-p "^[A-Z][a-z]+ [0-9]+, [0-9]\\{4\\}"
                                   (agent-shell--format-session-date "2025-06-15T12:00:00Z")))
          ;; Invalid input falls back gracefully
          (should (equal (agent-shell--format-session-date "not-a-date")
                         "not-a-date")))
      (set-time-zone-rule orig-tz))))

(ert-deftest agent-shell--prompt-select-session-test ()
  "Test `agent-shell--prompt-select-session' choices."
  (let* ((noninteractive t)
         (session-a '((sessionId . "session-1")
                      (title . "First")
                      (cwd . "/home/user/project-a")
                      (updatedAt . "2026-01-19T14:00:00Z")))
         (session-b '((sessionId . "session-2")
                      (title . "Second")
                      (cwd . "/home/user/project-b")
                      (updatedAt . "2026-01-20T16:00:00Z")))
         (sessions (list session-a session-b)))
    ;; noninteractive falls back to (car acp-sessions)
    (should (equal (agent-shell--prompt-select-session sessions)
                   session-a))))

(ert-deftest agent-shell--prompt-select-session-nil-sessions-test ()
  "Test `agent-shell--prompt-select-session' returns nil for empty sessions."
  (cl-letf (((symbol-function 'agent-shell-buffers)
             (lambda () nil)))
    (should-not (agent-shell--prompt-select-session nil))))

(ert-deftest agent-shell--initiate-session-strategy-new-skips-list-load ()
  "Test `agent-shell--initiate-session' skips list/load when strategy is `new'."
  (with-temp-buffer
    (let* ((agent-shell-session-strategy 'new)
           (requests '())
           (session-init-called nil)
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:session . ((:id . nil)
                                 (:mode-id . nil)
                                 (:modes . nil)))
                    (:supports-session-list . t)
                    (:supports-session-load . t)
                    (:active-requests)
                    (:event-subscriptions . nil))))
      (setq-local agent-shell--state state)
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () agent-shell--state))
                ((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest _args) nil))
                ((symbol-function 'agent-shell--update-header-and-mode-line)
                 (lambda () nil))
                ((symbol-function 'agent-shell-cwd)
                 (lambda () "/tmp"))
                ((symbol-function 'agent-shell--resolve-path)
                 (lambda (path) path))
                ((symbol-function 'agent-shell--mcp-servers)
                 (lambda () []))
                ((symbol-function 'acp-send-request)
                 (lambda (&rest args)
                   (push args requests)
                   (let* ((request (plist-get args :request))
                          (method (map-elt request :method)))
                     (pcase method
                       ("session/new"
                        (funcall (plist-get args :on-success)
                                 '((sessionId . "new-session-789"))))
                       (_ (error "Unexpected method: %s" method)))))))
        (agent-shell--initiate-session
         :shell-buffer (current-buffer)
         :on-session-init (lambda ()
                            (setq session-init-called t)))
        (let ((ordered-requests (nreverse requests)))
          (should (equal (mapcar (lambda (req)
                                   (map-elt (plist-get req :request) :method))
                                 ordered-requests)
                         '("session/new"))))
        (should session-init-called)
        (should (equal (map-nested-elt agent-shell--state '(:session :id)) "new-session-789"))))))

(ert-deftest agent-shell--outgoing-request-decorator-reaches-client ()
  "Test that :outgoing-request-decorator from state reaches the ACP client."
  (with-temp-buffer
    (let* ((my-decorator (lambda (request) request))
           (agent-shell--state (agent-shell--make-state
                                :agent-config nil
                                :buffer (current-buffer)
                                :client-maker (lambda (_buffer)
                                                (agent-shell--make-acp-client
                                                 :command "cat"
                                                 :context-buffer (current-buffer)))
                                :outgoing-request-decorator my-decorator)))
      ;; setq-local needed for buffer-local-value in agent-shell--make-acp-client
      (setq-local agent-shell--state agent-shell--state)
      (let ((client (funcall (map-elt agent-shell--state :client-maker)
                             (current-buffer))))
        (should (eq (map-elt client :outgoing-request-decorator) my-decorator))))))

(ert-deftest agent-shell--outgoing-request-decorator-modifies-request ()
  "Test that :outgoing-request-decorator modifies the sent request."
  (with-temp-buffer
    (let* ((sent-json nil)
           (decorator (lambda (request)
                        (when (equal (map-elt request :method) "session/new")
                          (map-put! request :params
                                    (cons '(_meta . ((systemPrompt . ((append . "extra instructions")))))
                                          (map-elt request :params))))
                        request))
           (agent-shell--state (agent-shell--make-state
                                :agent-config nil
                                :buffer (current-buffer)
                                :client-maker (lambda (_buffer)
                                                (agent-shell--make-acp-client
                                                 :command "cat"
                                                 :context-buffer (current-buffer)))
                                :outgoing-request-decorator decorator)))
      (setq-local agent-shell--state agent-shell--state)
      (let ((client (funcall (map-elt agent-shell--state :client-maker)
                             (current-buffer))))
        ;; Give client a fake process so acp--request-sender proceeds
        (map-put! client :process (start-process "fake" nil "cat"))
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (_proc json)
                     (setq sent-json json))))
          (acp-send-request
           :client client
           :request (acp-make-session-new-request :cwd "/tmp")
           :on-success #'ignore))
        (delete-process (map-elt client :process))
        ;; Verify the decorator's modification is in the sent JSON
        (let ((parsed (json-parse-string (string-trim sent-json) :object-type 'alist)))
          (should (equal (map-nested-elt parsed '(params _meta systemPrompt append))
                         "extra instructions")))))))

(ert-deftest agent-shell--extract-tool-parameters-test ()
  "Test `agent-shell--extract-tool-parameters' function."
  ;; Test nil input
  (should (null (agent-shell--extract-tool-parameters nil)))

  ;; Test empty alist
  (should (null (agent-shell--extract-tool-parameters '())))

  ;; Test with filePath parameter
  (should (equal (agent-shell--extract-tool-parameters
                  '((filePath . "/home/user/file.txt")))
                 "filePath: /home/user/file.txt"))

  ;; Test with multiple parameters
  (let ((result (agent-shell--extract-tool-parameters
                 '((filePath . "/home/user/file.txt")
                   (offset . 100)
                   (limit . 50)))))
    (should (string-match-p "filePath: /home/user/file.txt" result))
    (should (string-match-p "offset: 100" result))
    (should (string-match-p "limit: 50" result)))

  ;; Test that command and description are excluded
  (should (null (agent-shell--extract-tool-parameters
                 '((command . "ls -la")
                   (description . "List files")))))

  ;; Test that command/description are excluded but other params included
  (should (equal (agent-shell--extract-tool-parameters
                  '((command . "ls -la")
                    (description . "List files")
                    (workdir . "/tmp")))
                 "workdir: /tmp"))

  ;; Test with boolean value
  (should (equal (agent-shell--extract-tool-parameters
                  '((replaceAll . t)))
                 "replaceAll: true"))

  ;; Test with nil value (should be excluded)
  (should (null (agent-shell--extract-tool-parameters
                 '((filePath . nil)))))

  ;; Test with empty string (should be excluded)
  (should (null (agent-shell--extract-tool-parameters
                 '((pattern . "")))))

  ;; Test plan is excluded (shown separately)
  (should (null (agent-shell--extract-tool-parameters
                 '((plan . "Step 1: do something"))))))

(ert-deftest agent-shell--make-transcript-tool-call-entry-parameters-test ()
  "Test `agent-shell--make-transcript-tool-call-entry' with parameters."
  ;; Test basic entry without parameters
  (let ((entry (agent-shell--make-transcript-tool-call-entry
                :status "completed"
                :title "Read file"
                :kind "read"
                :output "file content here")))
    (should (string-match-p "### Tool Call \\[completed\\]: Read file" entry))
    (should (string-match-p "\\*\\*Tool:\\*\\* read" entry))
    (should (string-match-p "file content here" entry))
    (should-not (string-match-p "\\*\\*Parameters:\\*\\*" entry)))

  ;; Test entry with parameters
  (let ((entry (agent-shell--make-transcript-tool-call-entry
                :status "completed"
                :title "Read file"
                :kind "read"
                :parameters "filePath: /home/user/test.txt\noffset: 100"
                :output "file content here")))
    (should (string-match-p "\\*\\*Parameters:\\*\\*" entry))
    (should (string-match-p "filePath: /home/user/test.txt" entry))
    (should (string-match-p "offset: 100" entry))))

(ert-deftest agent-shell--tool-call-output-text-test ()
  "Test tool call output extraction from ACP updates."
  (should (equal
           (agent-shell--tool-call-output-text
            '((rawOutput . ((formatted_output . "stdout\nstderr\n")))
              (content . [((type . "content")
                           (content . ((text . "legacy content"))))])))
           "stdout\nstderr\n"))
  (should (equal
           (agent-shell--tool-call-output-text
            '((content . [((type . "content")
                           (content . ((text . "first"))))
                          ((type . "content")
                           (content . ((text . "second"))))])))
           "first\n\nsecond"))
  (should (equal (agent-shell--tool-call-output-text nil) "")))

(ert-deftest agent-shell--session-column-value-test ()
  "Test `agent-shell--session-column-value' extracts correct values."
  (let ((session '((sessionId . "abc-123")
                   (title . "My session")
                   (cwd . "/home/user/project")
                   (updatedAt . "2026-01-19T14:00:00Z"))))
    ;; directory extracts last path component
    (should (equal (agent-shell--session-column-value 'directory session)
                   "project"))
    ;; title returns session title
    (should (equal (agent-shell--session-column-value 'title session)
                   "My session"))
    ;; session-id returns full sessionId
    (should (equal (agent-shell--session-column-value 'session-id session)
                   "abc-123"))
    ;; date returns formatted date string
    (should (stringp (agent-shell--session-column-value 'date session)))
    ;; unknown column returns empty string
    (should (equal (agent-shell--session-column-value 'unknown session)
                   ""))))

(ert-deftest agent-shell--session-column-value-missing-fields-test ()
  "Test `agent-shell--session-column-value' handles missing fields."
  (let ((session '((sessionId . "s1"))))
    ;; missing cwd
    (should (equal (agent-shell--session-column-value 'directory session)
                   ""))
    ;; missing title
    (should (equal (agent-shell--session-column-value 'title session)
                   "Untitled"))
    ;; missing sessionId
    (should (equal (agent-shell--session-column-value 'session-id '())
                   ""))))

(ert-deftest agent-shell--session-column-face-test ()
  "Test `agent-shell--session-column-face' returns correct faces."
  (should (eq (agent-shell--session-column-face 'directory)
              'font-lock-keyword-face))
  (should (eq (agent-shell--session-column-face 'date)
              'font-lock-comment-face))
  (should (eq (agent-shell--session-column-face 'session-id)
              'font-lock-constant-face))
  ;; title and unknown have no face
  (should-not (agent-shell--session-column-face 'title))
  (should-not (agent-shell--session-column-face 'unknown)))

(ert-deftest agent-shell--session-choice-label-default-columns-test ()
  "Test `agent-shell--session-choice-label' with default columns."
  (let ((agent-shell-show-session-id nil)
        (session '((sessionId . "s1")
                   (title . "My session")
                   (cwd . "/home/user/project")
                   (updatedAt . "2026-01-19T14:00:00Z")))
        (max-widths '((directory . 10) (title . 15) (date . 20))))
    (let ((label (substring-no-properties
                  (agent-shell--session-choice-label
                   :acp-session session
                   :max-widths max-widths))))
      ;; All three columns present
      (should (string-match-p "project" label))
      (should (string-match-p "My session" label))
      ;; Directory and title are padded, date is not (last column)
      (should (string-match-p "project   " label))
      (should (string-match-p "My session      " label)))))

(ert-deftest agent-shell--session-choice-label-with-session-id-test ()
  "Test `agent-shell--session-choice-label' includes session-id column."
  (let ((agent-shell-show-session-id t)
        (session '((sessionId . "abc-123")
                   (title . "My session")
                   (cwd . "/home/user/project")
                   (updatedAt . "2026-01-19T14:00:00Z")))
        (max-widths '((directory . 10) (title . 15) (date . 20) (session-id . 10))))
    (let ((label (substring-no-properties
                  (agent-shell--session-choice-label
                   :acp-session session
                   :max-widths max-widths))))
      (should (string-match-p "abc-123" label))
      (should (string-match-p "project" label))
      (should (string-match-p "My session" label)))))

(ert-deftest agent-shell--session-id-indicator-disabled-test ()
  "Test `agent-shell--session-id-indicator' returns nil when disabled."
  (with-temp-buffer
    (setq-local agent-shell--state
                `((:session . ((:id . "test-session-id")))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (let ((agent-shell-show-session-id nil))
        (should-not (agent-shell--session-id-indicator))))))

(ert-deftest agent-shell--session-id-indicator-enabled-test ()
  "Test `agent-shell--session-id-indicator' returns formatted ID when enabled."
  (with-temp-buffer
    (setq-local agent-shell--state
                `((:session . ((:id . "test-session-id")))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (let ((agent-shell-show-session-id t))
        (let ((indicator (agent-shell--session-id-indicator)))
          (should indicator)
          (should (equal (substring-no-properties indicator)
                         "test-session-id")))))))

(ert-deftest agent-shell--session-id-indicator-no-session-test ()
  "Test `agent-shell--session-id-indicator' returns nil without active session."
  (with-temp-buffer
    (setq-local agent-shell--state
                `((:session . ((:id . nil)))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (let ((agent-shell-show-session-id t))
        (should-not (agent-shell--session-id-indicator))))))

(ert-deftest agent-shell-copy-session-id-test ()
  "Test `agent-shell-copy-session-id' copies ID to kill ring."
  (with-temp-buffer
    (setq-local agent-shell--state
                `((:session . ((:id . "test-session-id")))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'derived-mode-p)
               (lambda (&rest _) t)))
      (agent-shell-copy-session-id)
      (should (equal (current-kill 0) "test-session-id")))))

(ert-deftest agent-shell-copy-session-id-no-session-test ()
  "Test `agent-shell-copy-session-id' errors without active session."
  (with-temp-buffer
    (setq-local agent-shell--state
                `((:session . ((:id . nil)))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'derived-mode-p)
               (lambda (&rest _) t)))
      (should-error (agent-shell-copy-session-id)
                    :type 'user-error))))

(ert-deftest agent-shell--make-header-model-includes-session-id-test ()
  "Test `agent-shell--make-header-model' includes :session-id field."
  (with-temp-buffer
    (setq-local agent-shell--state
                `((:agent-config . ((:buffer-name . "Claude Code")
                                    (:icon-name . nil)))
                  (:session . ((:id . "test-session-id")
                               (:model-id . nil)
                               (:models . nil)
                               (:mode-id . nil)
                               (:modes . nil)))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'agent-shell--context-usage-indicator)
               (lambda () nil))
              ((symbol-function 'agent-shell--busy-indicator-frame)
               (lambda () nil)))
      ;; Enabled
      (let ((agent-shell-show-session-id t))
        (let ((model (agent-shell--make-header-model agent-shell--state)))
          (should (assq :session-id model))
          (should (equal (substring-no-properties (map-elt model :session-id))
                         "test-session-id"))))
      ;; Disabled
      (let ((agent-shell-show-session-id nil))
        (let ((model (agent-shell--make-header-model agent-shell--state)))
          (should (assq :session-id model))
          (should-not (map-elt model :session-id)))))))

(ert-deftest agent-shell--make-header-text-includes-session-id-test ()
  "Test `agent-shell--make-header' text mode includes session ID."
  (with-temp-buffer
    (setq-local agent-shell--state
                `((:agent-config . ((:buffer-name . "Claude Code")
                                    (:icon-name . nil)))
                  (:session . ((:id . "test-session-id")
                               (:model-id . nil)
                               (:models . nil)
                               (:mode-id . nil)
                               (:modes . nil)))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'agent-shell--context-usage-indicator)
               (lambda () nil))
              ((symbol-function 'agent-shell--busy-indicator-frame)
               (lambda () nil)))
      (let ((agent-shell-header-style 'text)
            (agent-shell-show-session-id t))
        (let ((header (agent-shell--make-header agent-shell--state)))
          (should (string-match-p "test-session-id"
                                  (substring-no-properties header)))))
      ;; Disabled: session ID absent
      (let ((agent-shell-header-style 'text)
            (agent-shell-show-session-id nil))
        (let ((header (agent-shell--make-header agent-shell--state)))
          (should-not (string-match-p "test-session-id"
                                      (substring-no-properties header))))))))

;;; Tests for agent-shell--dot-subdir-in-repo

(ert-deftest agent-shell--dot-subdir-in-repo-returns-path-test ()
  "Test that `agent-shell--dot-subdir-in-repo' returns the correct path."
  (cl-letf (((symbol-function 'agent-shell-cwd)
             (lambda () "/home/user/myproject")))
    (should (equal (agent-shell--dot-subdir-in-repo "screenshots")
                   "/home/user/myproject/.agent-shell/screenshots"))))

;;; Tests for agent-shell--dot-subdir

(ert-deftest agent-shell--dot-subdir-creates-directory-test ()
  "Test that `agent-shell--dot-subdir' creates the directory."
  (let* ((temp-dir (make-temp-file "agent-shell-test" t))
         (expected-dir (expand-file-name ".agent-shell/screenshots" temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () temp-dir))
                  ((symbol-function 'agent-shell--ensure-gitignore) #'ignore))
          (let ((agent-shell-dot-subdir-function #'agent-shell--dot-subdir-in-repo))
            (agent-shell--dot-subdir "screenshots")
            (should (file-directory-p expected-dir))))
      (delete-directory temp-dir t))))

(ert-deftest agent-shell--dot-subdir-returns-path-test ()
  "Test that `agent-shell--dot-subdir' returns the resolved path."
  (let* ((temp-dir (make-temp-file "agent-shell-test" t))
         (expected-dir (expand-file-name ".agent-shell/screenshots" temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () temp-dir))
                  ((symbol-function 'agent-shell--ensure-gitignore) #'ignore))
          (let ((agent-shell-dot-subdir-function #'agent-shell--dot-subdir-in-repo))
            (should (equal (agent-shell--dot-subdir "screenshots") expected-dir))))
      (delete-directory temp-dir t))))

(ert-deftest agent-shell--dot-subdir-noop-if-directory-exists-test ()
  "Test that `agent-shell--dot-subdir' does not error if the directory already exists."
  (let* ((temp-dir (make-temp-file "agent-shell-test" t))
         (expected-dir (expand-file-name ".agent-shell/screenshots" temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () temp-dir))
                  ((symbol-function 'agent-shell--ensure-gitignore) #'ignore))
          (let ((agent-shell-dot-subdir-function #'agent-shell--dot-subdir-in-repo))
            (make-directory expected-dir t)
            (should (equal (agent-shell--dot-subdir "screenshots") expected-dir))
            (should (file-directory-p expected-dir))))
      (delete-directory temp-dir t))))

(ert-deftest agent-shell--dot-subdir-uses-configured-function-test ()
  "Test that `agent-shell--dot-subdir' delegates to `agent-shell-dot-subdir-function'."
  (let* ((temp-dir (make-temp-file "agent-shell-test" t))
         (custom-called-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () temp-dir))
                  ((symbol-function 'agent-shell--ensure-gitignore) #'ignore))
          (let ((agent-shell-dot-subdir-function
                 (lambda (subdir)
                   (setq custom-called-with subdir)
                   (expand-file-name subdir temp-dir))))
            (agent-shell--dot-subdir "screenshots")
            (should (equal custom-called-with "screenshots"))))
      (delete-directory temp-dir t))))

(ert-deftest agent-shell--dot-subdir-errors-if-function-not-callable-test ()
  "Test that `agent-shell--dot-subdir' errors when `agent-shell-dot-subdir-function' is not a function."
  (let ((agent-shell-dot-subdir-function "not-a-function"))
    (should-error (agent-shell--dot-subdir "screenshots") :type 'error)))

(ert-deftest agent-shell--dot-subdir-errors-if-function-returns-non-string-test ()
  "Test that `agent-shell--dot-subdir' errors when `agent-shell-dot-subdir-function' returns a non-string."
  (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () "/tmp")))
    (let ((agent-shell-dot-subdir-function (lambda (_subdir) nil)))
      (should-error (agent-shell--dot-subdir "screenshots") :type 'error))
    (let ((agent-shell-dot-subdir-function (lambda (_subdir) 42)))
      (should-error (agent-shell--dot-subdir "screenshots") :type 'error))))

(ert-deftest agent-shell--dot-subdir-errors-if-function-returns-blank-string-test ()
  "Test that `agent-shell--dot-subdir' errors when `agent-shell-dot-subdir-function' returns a blank string."
  (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () "/tmp")))
    (let ((agent-shell-dot-subdir-function (lambda (_subdir) "  ")))
      (should-error (agent-shell--dot-subdir "screenshots") :type 'error))))

(ert-deftest agent-shell--on-request-calls-permission-request-handler-test ()
  "Test `agent-shell--on-request' calls handler and :respond auto-approves."
  (with-temp-buffer
    (let* ((responded-option-id nil)
           (handler-received nil)
           (agent-shell-permission-responder-function
            (lambda (request)
              (setq handler-received request)
              (when-let ((opt (seq-find
                               (lambda (o) (equal (map-elt o :kind) "allow_once"))
                               (map-elt request :options))))
                (funcall (map-elt request :respond)
                         (map-elt opt :option-id)))))
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:tool-calls . nil)
                    (:last-entry-type . nil)
                    (:event-subscriptions . nil))))
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () state))
                ((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest _)))
                ((symbol-function 'agent-shell-jump-to-latest-permission-button-row)
                 (lambda ()))
                ((symbol-function 'agent-shell--make-tool-call-permission-text)
                 (lambda (&rest _) "mock"))
                ((symbol-function 'agent-shell-viewport--buffer)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-shell--send-permission-response)
                 (lambda (&rest args)
                   (setq responded-option-id (plist-get args :option-id)))))
        (agent-shell--on-request
         :state state
         :acp-request `((id . "req-1")
                        (method . "session/request_permission")
                        (params . ((toolCall . ((toolCallId . "tc-1")
                                                (title . "Read file")
                                                (status . "pending")
                                                (kind . "read")))
                                   (options . [((kind . "allow_once")
                                                (name . "Allow")
                                                (optionId . "opt-allow"))
                                               ((kind . "reject_once")
                                                (name . "Reject")
                                                (optionId . "opt-reject"))])))))
        (should handler-received)
        (should (equal (map-elt (map-elt handler-received :tool-call) :kind) "read"))
        (should (equal (map-elt (map-elt handler-received :tool-call) :title) "Read file"))
        (should (= (length (map-elt handler-received :options)) 2))
        (should (equal responded-option-id "opt-allow"))))))

(ert-deftest agent-shell--on-request-handler-nil-leaves-prompt-test ()
  "Test `agent-shell--on-request' leaves interactive prompt when handler returns nil."
  (with-temp-buffer
    (let* ((responded nil)
           (agent-shell-permission-responder-function
            (lambda (_request) nil))
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:tool-calls . nil)
                    (:last-entry-type . nil)
                    (:event-subscriptions . nil))))
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () state))
                ((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest _)))
                ((symbol-function 'agent-shell-jump-to-latest-permission-button-row)
                 (lambda ()))
                ((symbol-function 'agent-shell--make-tool-call-permission-text)
                 (lambda (&rest _) "mock"))
                ((symbol-function 'agent-shell-viewport--buffer)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-shell--send-permission-response)
                 (lambda (&rest _)
                   (setq responded t))))
        (agent-shell--on-request
         :state state
         :acp-request `((id . "req-1")
                        (method . "session/request_permission")
                        (params . ((toolCall . ((toolCallId . "tc-1")
                                                (title . "Run command")
                                                (status . "pending")
                                                (kind . "execute")))
                                   (options . [((kind . "allow_once")
                                                (name . "Allow")
                                                (optionId . "opt-allow"))])))))
        (should-not responded)
        (should (equal (map-elt state :last-entry-type) "session/request_permission"))))))

(ert-deftest agent-shell--on-request-sends-error-for-unhandled-method-test ()
  "Test `agent-shell--on-request' responds with an error for unknown methods."
  (with-temp-buffer
    (let* ((captured-response nil)
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:event-subscriptions . nil)
                    (:last-entry-type . "previous-entry"))))
      (cl-letf (((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest _)))
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (setq captured-response (plist-get args :response))))
                ((symbol-function 'acp-make-error)
                 (lambda (&rest args)
                   `((:code . ,(plist-get args :code))
                     (:message . ,(plist-get args :message))))))
        (agent-shell--on-request
         :state state
         :acp-request '((id . "req-404")
                        (method . "unknown/method")))
        (should (equal (map-elt captured-response :request-id) "req-404"))
        (let ((error (map-elt captured-response :error)))
          (should (equal (map-elt error :code) -32601))
          (should (equal (map-elt error :message)
                         "Method not found: unknown/method")))
        (should-not (map-elt state :last-entry-type))))))

;;; Tests for agent-shell-show-context-usage-indicator

(ert-deftest agent-shell--context-usage-indicator-bar-test ()
  "Test `agent-shell--context-usage-indicator' bar mode."
  (let ((agent-shell--state
         (list (cons :buffer (current-buffer))
               (cons :usage (list (cons :context-used 50000)
                                  (cons :context-size 200000)
                                  (cons :total-tokens 50000))))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (let ((agent-shell-show-context-usage-indicator t))
        (let ((result (agent-shell--context-usage-indicator)))
          (should result)
          (should (= (length (substring-no-properties result)) 1))
          (should (eq (get-text-property 0 'face result) 'success)))))))

(ert-deftest agent-shell--context-usage-indicator-detailed-test ()
  "Test `agent-shell--context-usage-indicator' detailed mode."
  (let ((agent-shell--state
         (list (cons :buffer (current-buffer))
               (cons :usage (list (cons :context-used 30000)
                                  (cons :context-size 200000)
                                  (cons :total-tokens 30000))))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (let ((agent-shell-show-context-usage-indicator 'detailed))
        (let ((result (agent-shell--context-usage-indicator)))
          (should result)
          (should (string-match-p "30k/200k" (substring-no-properties result)))
          (should (string-match-p "15%%" (substring-no-properties result)))
          (should (eq (get-text-property 0 'face result) 'success)))))))

(ert-deftest agent-shell--context-usage-indicator-detailed-warning-test ()
  "Test `agent-shell--context-usage-indicator' detailed mode with warning face."
  (let ((agent-shell--state
         (list (cons :buffer (current-buffer))
               (cons :usage (list (cons :context-used 140000)
                                  (cons :context-size 200000)
                                  (cons :total-tokens 140000))))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (let ((agent-shell-show-context-usage-indicator 'detailed))
        (let ((result (agent-shell--context-usage-indicator)))
          (should (eq (get-text-property 0 'face result) 'warning)))))))

(ert-deftest agent-shell--context-usage-indicator-nil-test ()
  "Test `agent-shell--context-usage-indicator' returns nil when disabled."
  (let ((agent-shell--state
         (list (cons :buffer (current-buffer))
               (cons :usage (list (cons :context-used 50000)
                                  (cons :context-size 200000)
                                  (cons :total-tokens 50000))))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (let ((agent-shell-show-context-usage-indicator nil))
        (should-not (agent-shell--context-usage-indicator))))))

;;; Tests for agent-shell--permission-title

(ert-deftest agent-shell--permission-title-read-shows-filename-test ()
  "Test `agent-shell--permission-title' includes filename for read permission.
Based on ACP traffic from https://github.com/xenodium/agent-shell/issues/415."
  (should (equal
           "external_directory (_event.rs)"
           (agent-shell--permission-title
            :acp-request
            '((params . ((toolCall . ((toolCallId . "call_ad19e402fcb548c3acd48bbd")
                                      (status . "pending")
                                      (title . "external_directory")
                                      (rawInput . ((filepath . "/home/pmw/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/aws-sdk-s3-1.112.0/src/types/_event.rs")
                                                   (parentDir . "/home/pmw/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/aws-sdk-s3-1.112.0/src/types")))
                                      (kind . "other"))))))))))

(ert-deftest agent-shell--permission-title-edit-shows-filename-test ()
  "Test `agent-shell--permission-title' includes filename for edit permission.
Based on ACP traffic from https://github.com/xenodium/agent-shell/issues/415."
  (should (equal
           "edit (s3notifications.rs)"
           (agent-shell--permission-title
            :acp-request
            '((params . ((toolCall . ((toolCallId . "call_451e5acf91884aecaadf3173")
                                      (status . "pending")
                                      (title . "edit")
                                      (rawInput . ((filepath . "/home/pmw/Repos/warmup-s3-archives/src/s3notifications.rs")
                                                   (diff . "Index: /home/pmw/Repos/warmup-s3-archives/src/s3notifications.rs\n")))
                                      (kind . "edit"))))))))))

(ert-deftest agent-shell--permission-title-no-duplicate-filename-test ()
  "Test `agent-shell--permission-title' does not duplicate filename already in title."
  (should (equal
           "Read s3notifications.rs"
           (agent-shell--permission-title
            :acp-request
            '((params . ((toolCall . ((toolCallId . "tc-1")
                                      (title . "Read s3notifications.rs")
                                      (rawInput . ((filepath . "/home/user/src/s3notifications.rs")))
                                      (kind . "read"))))))))))

(ert-deftest agent-shell--permission-title-execute-fenced-test ()
  "Test `agent-shell--permission-title' fences execute commands."
  (should (equal
           "```console\nls -la\n```"
           (agent-shell--permission-title
            :acp-request
            '((params . ((toolCall . ((toolCallId . "tc-1")
                                      (title . "Bash")
                                      (rawInput . ((command . "ls -la")))
                                      (kind . "execute"))))))))))

(ert-deftest agent-shell-restart-preserves-default-directory ()
  "Restart should use the shell's directory, not the fallback buffer's.

After `kill-buffer' happens during restart, Emacs falls back to another
buffer.  Without the fix, `default-directory' would be inherited from
that fallback buffer, potentially starting the new shell in the wrong project."
  (let ((shell-buffer nil)
        (other-buffer nil)
        (captured-dir nil)
        (frame (make-frame '((visibility . nil))))
        (project-a "/tmp/project-a/")
        (project-b "/tmp/project-b/")
        (config (list (cons :buffer-name "test-agent")
                      (cons :client-maker
                            (lambda (_buf)
                              (list (cons :command "cat")))))))
    (unwind-protect
        (progn
          ;; Create a buffer from "project B" that Emacs will fall back to
          ;; after the shell buffer is killed.
          (setq other-buffer (get-buffer-create "*project-b-file*"))
          (with-current-buffer other-buffer
            (setq default-directory project-b))
          ;; Create the shell buffer in "project A".
          (setq shell-buffer (get-buffer-create "*test-restart-shell*"))
          (with-current-buffer shell-buffer
            (setq major-mode 'agent-shell-mode)
            (setq default-directory project-a)
            (setq-local agent-shell-session-strategy 'new)
            (setq-local agent-shell--state
                        `((:agent-config . ,config)
                          (:active-requests))))
          ;; Use a hidden frame and swap buffers around
          ;; so that when kill-buffer happens it will fallback to project-b
          ;; rather than the last buffer in the user's frame.
          (with-selected-frame frame
            (switch-to-buffer other-buffer)
            (switch-to-buffer shell-buffer)
            ;; Mock agent-shell--start to capture default-directory
            ;; instead of actually starting a shell.
            (cl-letf (((symbol-function 'agent-shell--start)
                       (lambda (&rest _args)
                         (setq captured-dir default-directory)
                         (get-buffer-create "*test-restart-new-shell*")))
                      ((symbol-function 'agent-shell--display-buffer)
                       #'ignore))
              (agent-shell-restart)))
          (should (equal captured-dir project-a)))
      (when (and frame (frame-live-p frame))
        (delete-frame frame))
      (when (and shell-buffer (buffer-live-p shell-buffer))
        (kill-buffer shell-buffer))
      (when (and other-buffer (buffer-live-p other-buffer))
        (kill-buffer other-buffer))
      (when-let ((buf (get-buffer "*test-restart-new-shell*")))
        (kill-buffer buf)))))

(ert-deftest agent-shell-sort-sessions-by-recency-test ()
  "Test `agent-shell--sort-sessions-by-recency' ordering."
  ;; Newest `updatedAt' first.
  (should (equal (agent-shell--sort-sessions-by-recency
                  '(((sessionId . "a") (updatedAt . "2024-01-01T00:00:00Z"))
                    ((sessionId . "b") (updatedAt . "2024-02-01T00:00:00Z"))
                    ((sessionId . "c") (updatedAt . "2024-01-15T00:00:00Z"))))
                 '(((sessionId . "b") (updatedAt . "2024-02-01T00:00:00Z"))
                   ((sessionId . "c") (updatedAt . "2024-01-15T00:00:00Z"))
                   ((sessionId . "a") (updatedAt . "2024-01-01T00:00:00Z")))))

  ;; Falls back to `createdAt' when `updatedAt' is missing.
  (should (equal (agent-shell--sort-sessions-by-recency
                  '(((sessionId . "a") (createdAt . "2024-01-01T00:00:00Z"))
                    ((sessionId . "b") (updatedAt . "2024-02-01T00:00:00Z"))))
                 '(((sessionId . "b") (updatedAt . "2024-02-01T00:00:00Z"))
                   ((sessionId . "a") (createdAt . "2024-01-01T00:00:00Z")))))

  ;; Sessions without either timestamp sort last.
  (should (equal (agent-shell--sort-sessions-by-recency
                  '(((sessionId . "a"))
                    ((sessionId . "b") (updatedAt . "2024-02-01T00:00:00Z"))))
                 '(((sessionId . "b") (updatedAt . "2024-02-01T00:00:00Z"))
                   ((sessionId . "a")))))

  ;; Empty input returns empty output.
  (should (equal (agent-shell--sort-sessions-by-recency '()) '())))

(ert-deftest agent-shell--clean-up-tolerates-mode-change-test ()
  "Test `kill-buffer' succeeds after the major mode is manually changed.

`kill-buffer-hook' is permanent-local, so the buffer-local
`agent-shell--clean-up' entry survives a mode change,
and it must handle that cleanly."
  (let ((shell-buf (generate-new-buffer " *test-shell*")))
    (unwind-protect
        (progn
          (with-current-buffer shell-buf
            (setq major-mode 'agent-shell-mode)
            (setq-local agent-shell--state
                        (agent-shell--make-state :buffer shell-buf))
            (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t)
            (text-mode))
          (kill-buffer shell-buf)
          (should-not (buffer-live-p shell-buf)))
      (when (buffer-live-p shell-buf)
        (with-current-buffer shell-buf
          (remove-hook 'kill-buffer-hook #'agent-shell--clean-up t))
        (kill-buffer shell-buf)))))

(provide 'agent-shell-tests)
;;; agent-shell-tests.el ends here
