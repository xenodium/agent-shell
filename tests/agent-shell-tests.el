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

(ert-deftest agent-shell--resolve-devcontainer-path-test ()
  "Test `agent-shell--resolve-devcontainer-path' function."
  ;; Mock agent-shell--get-devcontainer-workspace-path
  (cl-letf (((symbol-function 'agent-shell--get-devcontainer-workspace-path)
             (lambda (_) "/workspace")))

    ;; Need to run in an existing directory (requirement of `file-in-directory-p')
    (let ((default-directory "/tmp"))
      ;; With text file capabilities enabled
      (let ((agent-shell-text-file-capabilities t))

        ;; Resolves container paths to local filesystem paths
        (should (equal (agent-shell--resolve-devcontainer-path "/workspace/d/f.el") "/tmp/d/f.el"))
        (should (equal (agent-shell--resolve-devcontainer-path "/workspace/f.el") "/tmp/f.el"))
        (should (equal (agent-shell--resolve-devcontainer-path "/workspace") "/tmp"))

        ;; Prevents attempts to leave local working directory
        (should-error (agent-shell--resolve-devcontainer-path "/workspace/..") :type 'error)

        ;; Resolves local filesystem paths to container paths
        (should (equal (agent-shell--resolve-devcontainer-path "/tmp/d/f.el") "/workspace/d/f.el"))
        (should (equal (agent-shell--resolve-devcontainer-path "/tmp/f.el") "/workspace/f.el"))
        (should (equal (agent-shell--resolve-devcontainer-path "/tmp") "/workspace"))

        ;; Does not resolve unexpected paths
        (should-error (agent-shell--resolve-devcontainer-path "/unexpected") :type 'error))

      ;; With text file capabilities disabled (ie. never resolve to local filesystem)
      (let ((agent-shell-text-file-capabilities nil))

        ;; Does not resolve container paths to local filesystem paths
        (should-error (agent-shell--resolve-devcontainer-path "/workspace/d/f.el") :type 'error)
        (should-error (agent-shell--resolve-devcontainer-path "/workspace/f.el.") :type 'error)
        (should-error (agent-shell--resolve-devcontainer-path "/workspace") :type 'error)
        (should-error (agent-shell--resolve-devcontainer-path "/workspace/..") :type 'error)

        ;; Resolves local filesystem paths to container paths
        (should (equal (agent-shell--resolve-devcontainer-path "/tmp/d/f.el") "/workspace/d/f.el"))
        (should (equal (agent-shell--resolve-devcontainer-path "/tmp/f.el") "/workspace/f.el"))
        (should (equal (agent-shell--resolve-devcontainer-path "/tmp") "/workspace"))

        ;; Does not resolve unexpected paths
        (should-error (agent-shell--resolve-devcontainer-path "/unexpected") :type 'error)))))

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
                         ,(concat " pending   Update state initialization  \n"
                                  " pending   Update session initialization")
                         :mixed-expected
                         ,(concat " pending       First task \n"
                                  " in progress   Second task\n"
                                  " completed     Third task "))
                       ;; Terminal display mode
                       ( :graphic nil
                         :homogeneous-expected
                         ,(concat "[pending]  Update state initialization  \n"
                                  "[pending]  Update session initialization")
                         :mixed-expected
                         ,(concat "[pending]      First task \n"
                                  "[in progress]  Second task\n"
                                  "[completed]    Third task "))))
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
                    (should (> (length content) 69))))))

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
                  (should (equal (map-elt resource-link 'size) 69)))))))

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
                             (cons :last-entry-type nil))))

    ;; Mock acp-send-request to capture what gets sent
    (cl-letf (((symbol-function 'acp-send-request)
               (lambda (&rest args)
                 (setq sent-request args))))

      ;; Send a simple command
      (agent-shell--send-command
       :prompt "Hello agent"
       :shell nil)

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
                             (cons :last-entry-type nil))))

    ;; Mock build-content-blocks to throw an error
    (cl-letf (((symbol-function 'agent-shell--build-content-blocks)
               (lambda (_prompt)
                 (error "Simulated error in build-content-blocks")))
              ((symbol-function 'acp-send-request)
               (lambda (&rest args)
                 (setq sent-request args))))

      ;; First, verify that build-content-blocks actually throws an error
      (should-error (agent-shell--build-content-blocks "Test prompt")
                    :type 'error)

      ;; Now verify send-command handles the error gracefully
      (agent-shell--send-command
       :prompt "Test prompt with @file.txt"
       :shell nil)

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
                    "mcp     http and sse              "))))

  ;; Test with single capability per category (no comma)
  (let ((capabilities '((promptCapabilities (image . t))
                        (mcpCapabilities (http . t)))))
    (should (equal (substring-no-properties
                    (agent-shell--format-agent-capabilities capabilities))
                   (concat "prompt  image\n"
                           "mcp     http "))))

  ;; Test with top-level boolean capability (loadSession)
  (let ((capabilities '((loadSession . t)
                        (promptCapabilities (image . t) (embeddedContext . t)))))
    (should (equal (substring-no-properties
                    (agent-shell--format-agent-capabilities capabilities))
                   (concat "load session                            \n"
                           "prompt        image and embedded context"))))

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

    ;; Test that code blocks in output are stripped
    (let ((entry (agent-shell--make-transcript-tool-call-entry
                  :status "completed"
                  :title "test"
                  :output "```\ncode block content\n```")))
      (should (equal entry "

### Tool Call [completed]: test

**Timestamp:** 2025-11-02 18:17:41

```
code block content
```
")))

    ;; Test that code blocks in output are stripped
    (let ((entry (agent-shell--make-transcript-tool-call-entry
                  :status "completed"
                  :title "test"
                  :output "  \n  ```\ncode block content with spaces\n```\n")))
      (should (equal entry "

### Tool Call [completed]: test

**Timestamp:** 2025-11-02 18:17:41

```
code block content with spaces
```
")))))

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

(ert-deftest agent-shell--initiate-session-prefers-list-and-load-when-supported ()
  "Test `agent-shell--initiate-session' prefers session/list + session/load."
  (with-temp-buffer
    (let* ((agent-shell-session-load-strategy 'latest)
           (requests '())
           (session-init-called nil)
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:session . ((:id . nil)
                                 (:mode-id . nil)
                                 (:modes . nil)))
                    (:supports-session-list . t)
                    (:supports-session-load . t))))
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
         :shell `((:buffer . ,(current-buffer)))
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
    (let* ((agent-shell-session-load-strategy 'latest)
           (requests '())
           (session-init-called nil)
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:session . ((:id . nil)
                                 (:mode-id . nil)
                                 (:modes . nil)))
                    (:supports-session-list . t)
                    (:supports-session-load . t))))
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
         :shell `((:buffer . ,(current-buffer)))
         :on-session-init (lambda ()
                            (setq session-init-called t)))
        (let ((ordered-requests (nreverse requests)))
          (should (equal (mapcar (lambda (req)
                                   (map-elt (plist-get req :request) :method))
                                 ordered-requests)
                         '("session/list" "session/new"))))
        (should session-init-called)
        (should (equal (map-nested-elt agent-shell--state '(:session :id)) "new-session-456"))))))

(ert-deftest agent-shell--prompt-select-session-to-load-test ()
  "Test `agent-shell--prompt-select-session-to-load' choices."
  (let* ((session-a '((sessionId . "session-1")
                      (title . "First")
                      (updatedAt . "2026-01-19T14:00:00Z")))
         (session-b '((sessionId . "session-2")
                      (title . "Second")
                      (updatedAt . "2026-01-20T16:00:00Z")))
         (sessions (list session-a session-b)))
    ;; Select existing session
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (agent-shell--session-choice-label session-b))))
      (should (equal (agent-shell--prompt-select-session-to-load sessions)
                     session-b)))
    ;; Select "new session" option
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 agent-shell--start-new-session-choice)))
      (should-not (agent-shell--prompt-select-session-to-load sessions)))))

(ert-deftest agent-shell--session-picker-sort-test ()
  "Test `agent-shell--session-picker-sort' keeps the new-session option first."
  (let* ((session-a-label "First | 2026-01-19T14:00:00Z | session-1")
         (session-b-label "Second | 2026-01-20T16:00:00Z | session-2")
         (candidates (list session-a-label
                           agent-shell--start-new-session-choice
                           session-b-label)))
    (should (equal (agent-shell--session-picker-sort candidates)
                   (list agent-shell--start-new-session-choice
                         session-a-label
                         session-b-label)))))

(ert-deftest agent-shell--initiate-session-strategy-new-skips-list-load ()
  "Test `agent-shell--initiate-session' skips list/load when strategy is `new'."
  (with-temp-buffer
    (let* ((agent-shell-session-load-strategy 'new)
           (requests '())
           (session-init-called nil)
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:session . ((:id . nil)
                                 (:mode-id . nil)
                                 (:modes . nil)))
                    (:supports-session-list . t)
                    (:supports-session-load . t))))
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
         :shell `((:buffer . ,(current-buffer)))
         :on-session-init (lambda ()
                            (setq session-init-called t)))
        (let ((ordered-requests (nreverse requests)))
          (should (equal (mapcar (lambda (req)
                                   (map-elt (plist-get req :request) :method))
                                 ordered-requests)
                         '("session/new"))))
        (should session-init-called)
        (should (equal (map-nested-elt agent-shell--state '(:session :id)) "new-session-789"))))))

(provide 'agent-shell-tests)
;;; agent-shell-tests.el ends here
