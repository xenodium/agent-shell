;;; agent-shell-tests.el --- Tests for agent-shell -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'subr-x)

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
  ;; Plan steps carry no kind, so the default label is the status icon
  ;; alone, which renders the same on graphical and text frames.
  (dolist (test-case `(;; Graphical display mode
                       ( :graphic t
                         :homogeneous-expected
                         ,(concat "◔ Update state initialization\n"
                                  "◔ Update session initialization")
                         :mixed-expected
                         ,(concat "◔ First task\n"
                                  "◔ Second task\n"
                                  "✓ Third task"))
                       ;; Terminal display mode
                       ( :graphic nil
                         :homogeneous-expected
                         ,(concat "◔ Update state initialization\n"
                                  "◔ Update session initialization")
                         :mixed-expected
                         ,(concat "◔ First task\n"
                                  "◔ Second task\n"
                                  "✓ Third task"))))
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
  "Test `agent-shell--parse-file-mentions' function."
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
  "Test `agent-shell--build-content-blocks' function."
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
  "Test `agent-shell--build-content-blocks' with binary PNG files."
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

            ;; PNG resolves to `image/png' via mailcap regardless of
            ;; whether `image-supported-file-p' recognises the file
            ;; (mailcap is the fallback when image lib support is
            ;; unavailable), so the image code-path is reachable in
            ;; both graphical and batch Emacs.
            ;; Test with image and embedded context support — should use ContentBlock::Image
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

            ;; Test without image capability — should use resource_link with correct mime type
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

(ert-deftest agent-shell--content-block-to-markdown-test ()
  "Test `agent-shell--content-block-to-markdown'.

Agent `session/update' content blocks may be text OR image (a content block
whose `type' is \"image\", e.g. an agent returning a screenshot, with a file
`uri' and/or base64 `data').  The render path historically extracted only
`(content text)', silently dropping image blocks.  This helper makes
extraction image-aware so images flow into the existing markdown
image-rendering path as `![alt](uri)'."
  ;; Text block -> its text, unchanged.
  (should (equal (agent-shell--content-block-to-markdown
                  '((type . "text") (text . "hello world")))
                 "hello world"))

  ;; Image block with a file URI -> a markdown image referencing that URI,
  ;; so `agent-shell--render-markdown :render-images t' renders it inline.
  (should (equal (agent-shell--content-block-to-markdown
                  '((type . "image")
                    (mimeType . "image/png")
                    (uri . "file:///tmp/shot.png")))
                 "\n\n![image](file:///tmp/shot.png)\n\n"))

  ;; Image block with an explicit alt/name is honored in the alt text.
  (should (equal (agent-shell--content-block-to-markdown
                  '((type . "image")
                    (mimeType . "image/png")
                    (name . "chart")
                    (uri . "file:///tmp/chart.png")))
                 "\n\n![chart](file:///tmp/chart.png)\n\n"))

  ;; THE BUG: an image block must NOT extract to nil/empty -- that is the
  ;; silent drop. Any non-empty rendering is acceptable here.
  (should-not (string-empty-p
               (or (agent-shell--content-block-to-markdown
                    '((type . "image")
                      (mimeType . "image/png")
                      (uri . "file:///tmp/x.png")))
                   "")))

  ;; A uri -- local or remote -- is emitted verbatim; the renderer resolves
  ;; it (downloading remote uris on demand), so conversion stays I/O-free.
  (should (equal (agent-shell--content-block-to-markdown
                  '((type . "image")
                    (mimeType . "image/png")
                    (uri . "https://example.com/x.png")))
                 "\n\n![image](https://example.com/x.png)\n\n"))

  ;; A resource_link block -> a markdown link (name as label, uri as target)
  ;; so the renderer's link machinery makes it clickable.
  (should (equal (agent-shell--content-block-to-markdown
                  '((type . "resource_link")
                    (name . "report.pdf")
                    (uri . "file:///tmp/report.pdf")))
                 "\n\n[report.pdf](file:///tmp/report.pdf)\n\n"))

  ;; An embedded resource with text -> a blockquote (content set apart, not
  ;; dropped).
  (should (equal (agent-shell--content-block-to-markdown
                  '((type . "resource")
                    (resource . ((uri . "file:///tmp/note.txt")
                                 (mimeType . "text/plain")
                                 (text . "line1\nline2")))))
                 "\n\n> line1\n> line2\n\n"))

  ;; An audio block -> a link labelled "audio (EXT)" to a decoded cache file
  ;; (binary, opens externally when followed).
  (let* ((wav "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR4nGP4DwQACfsD/fteaysAAAAASUVORK5CYII=")
         (markdown (agent-shell--content-block-to-markdown
                    `((type . "audio") (mimeType . "audio/wav") (data . ,wav)))))
    (should (string-prefix-p "\n\n[audio (wav)](" markdown))
    (should (string-suffix-p ".wav)\n\n" markdown))
    (should (string-match "](\\([^)]+\\))" markdown))
    (should (file-exists-p (match-string 1 markdown)))
    (delete-file (match-string 1 markdown)))

  ;; An embedded binary (blob) resource -> a link to a decoded cache file,
  ;; labelled by the resource's filename.
  (let* ((blob "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR4nGP4DwQACfsD/fteaysAAAAASUVORK5CYII=")
         (markdown (agent-shell--content-block-to-markdown
                    `((type . "resource")
                      (resource . ((uri . "file:///tmp/report.pdf")
                                   (mimeType . "application/pdf")
                                   (blob . ,blob)))))))
    (should (string-match "\\`\n\n\\[report.pdf\\](\\(/.*\\.pdf\\))\n\n\\'" markdown))
    (should (file-exists-p (match-string 1 markdown)))
    (delete-file (match-string 1 markdown)))

  ;; A future/unknown block type -> a visible placeholder (so lagging support
  ;; is spottable), not a silent drop.
  (should (equal (agent-shell--content-block-to-markdown
                  '((type . "video") (mimeType . "video/mp4")))
                 "[unsupported content: video]"))

  ;; Image block carrying base64 `data' (the spec-required field, no uri) is
  ;; decoded to a cache file and referenced as a markdown image rather than
  ;; dropped.
  (let* ((png "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR4nGP4DwQACfsD/fteaysAAAAASUVORK5CYII=")
         (markdown (agent-shell--content-block-to-markdown
                    `((type . "image") (mimeType . "image/png") (data . ,png)))))
    ;; A bare absolute cache path (no `file://'), so paths with URI-special
    ;; characters still resolve.
    (should (string-match "\\`\n\n!\\[image\\](\\(/.*\\.png\\))\n\n\\'" markdown))
    (should (file-exists-p (match-string 1 markdown)))
    (delete-file (match-string 1 markdown)))

  ;; When both `uri' and `data' are present the `uri' is used directly (no
  ;; redundant decode); `data' is only a fallback when `uri' is absent.
  (should (equal (agent-shell--content-block-to-markdown
                  '((type . "image") (mimeType . "image/png")
                    (uri . "file:///tmp/shot.png")
                    (data . "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR4nGP4DwQACfsD/fteaysAAAAASUVORK5CYII=")))
                 "\n\n![image](file:///tmp/shot.png)\n\n")))

(ert-deftest agent-shell--tool-call-update-output-markdown-test ()
  "Test `agent-shell--tool-call-update-output-markdown'."
  ;; Content blocks win over rawOutput when both are present.
  (should (equal
           (agent-shell--tool-call-update-output-markdown
            '((rawOutput . ((formatted_output . "stdout\nstderr\n")))
              (content . [((type . "content")
                           (content . ((type . "text")
                                       (text . "block content"))))])))
           "block content"))
  ;; Codex sends output via rawOutput and leaves content empty.
  (should (equal
           (agent-shell--tool-call-update-output-markdown
            '((rawOutput . ((formatted_output . "stdout\nstderr\n")))))
           "stdout\nstderr\n"))
  ;; `rawOutput' is agent-defined, so non-string payloads are ignored.
  (should (equal
           (agent-shell--tool-call-update-output-markdown
            '((rawOutput . ((formatted_output . 42)))))
           ""))
  (should (equal
           (agent-shell--tool-call-update-output-markdown
            '((rawOutput . "plain string raw output")))
           ""))
  (should (equal
           (agent-shell--tool-call-update-output-markdown
            '((content . [((type . "content")
                           (content . ((type . "text") (text . "first"))))
                          ((type . "content")
                           (content . ((type . "text") (text . "second"))))])))
           "first\n\nsecond"))
  (should (equal (agent-shell--tool-call-update-output-markdown nil) "")))

(ert-deftest agent-shell--image-data-to-file-test ()
  "Test `agent-shell--image-data-to-file'.

Image content blocks carry their payload as base64 `data' (the spec-required
field).  This writes that payload to a cache file so it can be rendered or
opened.  The extension is validated against `image-file-name-extensions', so
an agent-supplied MIME-TYPE cannot inject a path or stray characters into the
file name."
  (let ((png "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR4nGP4DwQACfsD/fteaysAAAAASUVORK5CYII="))
    ;; Valid PNG data -> a real file with a .png extension under the cache dir.
    (let ((file (agent-shell--image-data-to-file png "image/png")))
      (should (stringp file))
      (should (string-suffix-p ".png" file))
      (should (file-exists-p file))
      (delete-file file))

    ;; image/svg+xml maps to a .svg extension (not the literal "svg+xml").
    (let ((file (agent-shell--image-data-to-file png "image/svg+xml")))
      (should (string-suffix-p ".svg" file))
      (delete-file file))

    ;; Missing data -> nil (nothing to write).
    (should-not (agent-shell--image-data-to-file nil "image/png"))

    ;; Unknown image subtype -> nil (not in `image-file-name-extensions').
    (should-not (agent-shell--image-data-to-file png "image/whatever"))

    ;; SECURITY: a MIME-TYPE smuggling a path or stray characters past the
    ;; `image/' prefix is rejected outright by the extension allowlist, so
    ;; nothing is written outside the cache dir.
    (should-not (agent-shell--image-data-to-file png "image/../../../tmp/evil"))
    (should-not (agent-shell--image-data-to-file png "image/png ../evil"))))

(ert-deftest agent-shell--image-extension-from-content-type-test ()
  "Test `agent-shell--image-extension-from-content-type'.

Maps the response `Content-Type' header (parsed from the current buffer's
HTTP headers) to a file extension, so a URL without one -- like a GitHub
avatar -- can be cached under a name `image-supported-file-p' recognizes."
  (cl-flet ((extension-for (content-type)
              (with-temp-buffer
                (insert (format "HTTP/1.1 200 OK\r\nContent-Type: %s\r\n\r\nBODY"
                                content-type))
                (goto-char (point-min))
                (agent-shell--image-extension-from-content-type))))
    (should (equal (extension-for "image/png") "png"))
    (should (equal (extension-for "image/jpeg") "jpg"))
    (should (equal (extension-for "image/svg+xml") "svg"))
    ;; Parameters after the type (e.g. "; charset=...") are ignored.
    (should (equal (extension-for "image/png; charset=binary") "png"))
    (should (equal (extension-for "image/vnd.microsoft.icon") "ico"))
    ;; Non-image or unknown content types yield nil.
    (should-not (extension-for "text/html"))
    ;; No Content-Type header at all -> nil.
    (should-not (with-temp-buffer
                  (insert "HTTP/1.1 200 OK\r\n\r\nBODY")
                  (goto-char (point-min))
                  (agent-shell--image-extension-from-content-type)))))

(ert-deftest agent-shell--fetch-agent-icon-extensionless-url-test ()
  "Test `agent-shell--fetch-agent-icon' with an extensionless URL.

A GitHub avatar URL carries no file extension, so the cached copy must be
named from the response `Content-Type' -- otherwise `image-supported-file-p'
rejects it and no icon (not even a fallback) is shown.  Also verifies a
second call reuses the cached file instead of downloading again."
  (let* ((cache-dir (make-temp-file "agent-shell-icon-cache" t))
         (url "https://avatars.githubusercontent.com/u/131064358")
         (downloads 0)
         (fake-response
          (lambda (&rest _)
            (setq downloads (1+ downloads))
            (let ((buffer (generate-new-buffer " *fake-http*")))
              (with-current-buffer buffer
                (set-buffer-multibyte nil)
                (insert "HTTP/1.1 200 OK\r\nContent-Type: image/png\r\n\r\nPNGBYTES"))
              buffer))))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-cache-dir)
                   (lambda (&rest _) cache-dir))
                  ((symbol-function 'url-retrieve-synchronously) fake-response))
          ;; First call downloads and caches under a Content-Type-derived
          ;; extension, so the result is a recognizable image file.
          (let ((path (agent-shell--fetch-agent-icon url)))
            (should (stringp path))
            (should (string-suffix-p ".png" path))
            (should (file-exists-p path))
            ;; `image-supported-file-p' depends on the running Emacs being
            ;; built with PNG support, which headless CI may lack, so only
            ;; assert it where PNG is actually available.
            (when (image-type-available-p 'png)
              (should (image-supported-file-p path)))
            (should (equal downloads 1))
            ;; Second call reuses the cached file (globbed by base name), no
            ;; additional download.
            (should (equal (agent-shell--fetch-agent-icon url) path))
            (should (equal downloads 1))))
      (delete-directory cache-dir t))))

(ert-deftest agent-shell--content-extension-test ()
  "Test `agent-shell--content-extension'."
  (should (equal (agent-shell--content-extension "audio/wav") "wav"))
  (should (equal (agent-shell--content-extension "application/pdf") "pdf"))
  ;; Case-insensitive.
  (should (equal (agent-shell--content-extension "IMAGE/PNG") "png"))
  ;; Vendor/compound types don't reduce to a plain extension.
  (should-not (agent-shell--content-extension "application/octet-stream"))
  (should-not (agent-shell--content-extension "image/svg+xml"))
  (should-not (agent-shell--content-extension nil)))

(ert-deftest agent-shell--data-to-cache-file-test ()
  "Test `agent-shell--data-to-cache-file'."
  (let ((data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR4nGP4DwQACfsD/fteaysAAAAASUVORK5CYII="))
    ;; Valid data + extension -> a real file with that extension.
    (let ((file (agent-shell--data-to-cache-file data "pdf")))
      (should (stringp file))
      (should (string-suffix-p ".pdf" file))
      (should (file-exists-p file))
      (delete-file file))

    ;; Non-string data -> nil (nothing to write).
    (should-not (agent-shell--data-to-cache-file nil "pdf"))

    ;; SECURITY: an extension that isn't plain alphanumeric is rejected, so a
    ;; crafted value can't inject a path or stray characters into the name.
    (should-not (agent-shell--data-to-cache-file data "../evil"))
    (should-not (agent-shell--data-to-cache-file data "tar.gz"))))

(ert-deftest agent-shell--on-notification-agent-message-chunk-markdown-test ()
  "Test `agent_message_chunk' rendering wires through to markdown.

Drives an ACP `session/update' notification through
`agent-shell--on-notification' and asserts the body handed to the renderer
is what `agent-shell--content-block-to-markdown' produces for the content
block -- covering the dispatch path, not just the helper in isolation."
  (let ((state (list (cons :chunked-group-count 0)
                     ;; Pre-set so the header/end-of-prompt branches are
                     ;; skipped; the test only exercises content rendering.
                     (cons :last-entry-type "agent_message_chunk")
                     (cons :last-agent-message-id nil)
                     (cons :last-activity-time nil)))
        (rendered nil))
    (cl-letf (((symbol-function 'agent-shell--active-requests-p)
               (lambda (_state) t))
              ((symbol-function 'agent-shell--append-transcript)
               #'ignore)
              ((symbol-function 'agent-shell--emit-event)
               #'ignore)
              ((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args) (setq rendered (plist-get args :body)))))
      ;; Text content block -> its text.
      (agent-shell--on-notification
       :state state
       :acp-notification '((method . "session/update")
                           (params
                            (update
                             (sessionUpdate . "agent_message_chunk")
                             (content (type . "text") (text . "hello"))))))
      (should (equal rendered "hello"))

      ;; Image content block with a uri -> a markdown image, not a dropped
      ;; or text-only chunk.
      (agent-shell--on-notification
       :state state
       :acp-notification '((method . "session/update")
                           (params
                            (update
                             (sessionUpdate . "agent_message_chunk")
                             (content (type . "image")
                                      (mimeType . "image/png")
                                      (uri . "file:///tmp/x.png"))))))
      (should (equal rendered "\n\n![image](file:///tmp/x.png)\n\n")))))

(ert-deftest agent-shell--on-notification-session-info-update-test ()
  "Test `session_info_update' updates the session title.

Drives an ACP `session/update' notification through
`agent-shell--on-notification' and asserts the title from the update
is handed to `agent-shell--set-session-title'."
  (with-temp-buffer
    (let ((state (list (cons :buffer (current-buffer))
                       (cons :last-entry-type nil)
                       (cons :last-activity-time nil)))
          (title nil))
      (cl-letf (((symbol-function 'agent-shell--set-session-title)
                 (lambda (new-title) (setq title new-title))))
        (agent-shell--on-notification
         :state state
         :acp-notification '((method . "session/update")
                             (params
                              (update
                               (sessionUpdate . "session_info_update")
                               (title . "Render Tool Updates")))))
        (should (equal title "Render Tool Updates"))))))

(ert-deftest agent-shell--collect-attached-files-test ()
  "Test `agent-shell--collect-attached-files' function."
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

(ert-deftest agent-shell--get-numbered-region-test ()
  "Test `agent-shell--get-numbered-region' preserves selection and respects TRIM."
  (with-temp-buffer
    ;; Lines: 1="", 2="foo", 3="", 4="bar", 5="" (including trailing newline).
    (insert "
foo

bar

")
    ;; Without TRIM: empty boundary lines (1 and 5) are preserved.
    (should (equal (agent-shell--get-numbered-region
                    :buffer (current-buffer)
                    :from (point-min)
                    :to (point-max))
                   (string-join
                    '("   1: " "   2: foo" "   3: " "   4: bar" "   5: ")
                    "\n")))
    ;; With TRIM: empty boundary lines are stripped, internal empty kept.
    (should (equal (agent-shell--get-numbered-region
                    :buffer (current-buffer)
                    :from (point-min)
                    :to (point-max)
                    :trim t)
                   (string-join
                    '("   2: foo" "   3: " "   4: bar")
                    "\n"))))
  (with-temp-buffer
    (insert "foo
bar
baz
")
    (let (from to)
      (goto-char (point-min))
      (forward-line 1)
      (setq from (point))
      (forward-line 1)
      (setq to (point))
      ;; When selecting whole lines including trailing newline, adjust
      ;; region-end
      (should (equal (agent-shell--get-numbered-region
                    :buffer (current-buffer)
                    :from from
                    :to to)
                   "   2: bar")))))

(ert-deftest agent-shell--get-region-context-preserves-source-faces-only ()
  "Region context must keep faces but not source control properties.

A `markdown-mode' source buffer fonts emphasis markup (e.g. underscores)
with `invisible' and `face' properties.  When a single-line region is
grabbed for the file-link preview, source control properties must not
leak into the context, otherwise the compose buffer may hide literal
text.  Face properties are intentionally preserved for syntax
highlighting."
  (let* ((temp-file (make-temp-file "agent-shell-region" nil ".txt"))
         (default-directory (file-name-directory temp-file)))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "_hello_world_"))
          (with-current-buffer (find-file-noselect temp-file)
            ;; Simulate markdown-mode font-lock properties on the
            ;; underscores, as a real markdown/poly-markdown buffer
            ;; would carry.
            (put-text-property (point-min) (point-max)
                               'face 'markdown-markup-face)
            (put-text-property (point-min) (point-max)
                               'font-lock-face 'font-lock-string-face)
            (goto-char (point-min))
            (while (search-forward "_" nil t)
              (put-text-property (1- (point)) (point)
                                 'invisible 'markdown-markup))
            (goto-char (point-min))
            (set-mark (point-max))
            (activate-mark)
            (let ((ctx (agent-shell--get-region-context :deactivate t)))
              (should-not (text-property-any 0 (length ctx)
                                             'invisible 'markdown-markup ctx))
              (should (text-property-any 0 (length ctx)
                                         'face 'markdown-markup-face ctx))
              (should (text-property-any 0 (length ctx)
                                         'font-lock-face
                                         'font-lock-string-face ctx)))))
      (when (get-file-buffer temp-file)
        (with-current-buffer (get-file-buffer temp-file)
          (set-buffer-modified-p nil)))
      (ignore-errors (delete-file temp-file)))))

(ert-deftest agent-shell--get-numbered-region-preserves-source-faces-only ()
  "Numbered region preview must keep faces but not source control properties.

A multi-line region spanning lines that carry foreign properties (e.g.
markdown-mode's `invisible' on emphasis markup) must drop control
properties so the compose buffer shows literal source text.  Face
properties are intentionally preserved for syntax highlighting."
  (with-temp-buffer
    (insert "_hello_
_world_")
    (put-text-property (point-min) (point-max) 'face 'markdown-markup-face)
    (put-text-property (point-min) (point-max)
                       'font-lock-face 'font-lock-string-face)
    (goto-char (point-min))
    (while (search-forward "_" nil t)
      (put-text-property (1- (point)) (point) 'invisible 'markdown-markup))
    (let ((result (agent-shell--get-numbered-region
                   :buffer (current-buffer)
                   :from (point-min)
                   :to (point-max))))
      (should-not (text-property-any 0 (length result)
                                     'invisible 'markdown-markup result))
      (should (text-property-any 0 (length result)
                                 'face 'markdown-markup-face result))
      (should (text-property-any 0 (length result)
                                 'font-lock-face
                                 'font-lock-string-face result)))))

(ert-deftest agent-shell--expand-truncated-regions-test ()
  "Test `agent-shell--expand-truncated-regions' substitutes marked spans for their full text."
  ;; No marked regions: prompt unchanged.
  (should (equal (agent-shell--expand-truncated-regions "plain prompt") "plain prompt"))

  ;; Single marked region: span replaced with `agent-shell-region-text'.
  (let* ((preview (propertize "1: foo\n   Expand..."
                              'agent-shell-region-id 'r1
                              'agent-shell-region-text "1: foo\n2: bar\n3: baz"))
         (prompt (concat "before " preview " after")))
    (should (equal (agent-shell--expand-truncated-regions prompt)
                   "before 1: foo\n2: bar\n3: baz after")))

  ;; Multiple marked regions: each expanded; forward iteration handles all.
  (let* ((a (propertize "A-preview"
                        'agent-shell-region-id 'a
                        'agent-shell-region-text "A-full"))
         (b (propertize "B-preview"
                        'agent-shell-region-id 'b
                        'agent-shell-region-text "B-full-LONGER"))
         (prompt (concat "x " a " y " b " z")))
    (should (equal (agent-shell--expand-truncated-regions prompt)
                   "x A-full y B-full-LONGER z")))

  ;; Region with id but missing text property: span left alone.
  (let ((prompt (concat "keep "
                        (propertize "preview" 'agent-shell-region-id 'r)
                        " me")))
    (should (equal (agent-shell--expand-truncated-regions prompt) "keep preview me"))))

(ert-deftest agent-shell--send-command-integration-test ()
  "Integration test: verify `agent-shell--send-command' calls ACP correctly."
  (let ((sent-request nil)
        (agent-shell--state (list
                             (cons :client 'test-client)
                             (cons :session (list (cons :id "test-session") (cons :title nil)))
                             (cons :prompt-capabilities '((:embedded-context . t)))
                             (cons :buffer (current-buffer))
                             (cons :last-entry-type nil)
                             (cons :active-requests nil)
                             (cons :idle-timer nil))))

    ;; Mock acp-send-request to capture what gets sent;
    ;; stub viewport--buffer to avoid interactive shell-buffer prompt in batch.
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'acp-send-request)
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
  "Test `agent-shell--send-command' falls back to plain text on error.
The fallback triggers when `agent-shell--build-content-blocks' fails."
  (let ((sent-request nil)
        (agent-shell--state (list
                             (cons :client 'test-client)
                             (cons :session (list (cons :id "test-session") (cons :title nil)))
                             (cons :prompt-capabilities '((:embedded-context . t)))
                             (cons :buffer (current-buffer))
                             (cons :last-entry-type nil)
                             (cons :active-requests nil)
                             (cons :idle-timer nil))))

    ;; Mock build-content-blocks to throw an error;
    ;; stub viewport--buffer to avoid interactive shell-buffer prompt in batch.
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'agent-shell--build-content-blocks)
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
                                  (cons :session (list (cons :id "test-session") (cons :title nil)))
                                  (cons :last-entry-type nil)
                                  (cons :tool-calls nil)
                                  (cons :usage (list (cons :total-tokens 0)))
                                  (cons :idle-timer nil)))
        (agent-shell-show-busy-indicator nil)
        (agent-shell-show-usage-at-turn-end nil))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'agent-shell--send-request)
               (lambda (&rest args)
                 (setq captured-on-success (plist-get args :on-success))))
              ((symbol-function 'shell-maker-finish-output)
               (lambda (&rest _)))
              ((symbol-function 'agent-shell--prompt-queue-process-next)
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

(ert-deftest agent-shell--send-command-emits-input-submitted-with-prompt-test ()
  "Test `input-submitted' carries the expanded prompt text."
  (let ((received-events nil)
        (agent-shell--state (list (cons :buffer (current-buffer))
                                  (cons :event-subscriptions nil)
                                  (cons :client 'test-client)
                                  (cons :session (list (cons :id "test-session") (cons :title nil)))
                                  (cons :last-entry-type nil)
                                  (cons :tool-calls nil)
                                  (cons :idle-timer nil)))
        (agent-shell-show-busy-indicator nil))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'agent-shell--send-request)
               (lambda (&rest _)))
              ((symbol-function 'shell-maker-finish-output)
               (lambda (&rest _))))
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer)
       :event 'input-submitted
       :on-event (lambda (event)
                   (push event received-events)))
      (agent-shell--send-command
       :prompt (concat "Explain "
                       (propertize "region.el:1-10"
                                   'agent-shell-region-id "region-1"
                                   'agent-shell-region-text "(defun hello ())"))
       :shell-buffer (current-buffer))
      (should (= (length received-events) 1))
      (let ((prompt (map-nested-elt (car received-events) '(:data :prompt))))
        ;; Truncated regions are expanded, and no text properties leak out.
        (should (equal prompt "Explain (defun hello ())"))
        (should-not (text-properties-at 0 prompt))))))

(ert-deftest agent-shell--send-command-preserves-viewport-edit-draft-test ()
  "Sending a command must not disturb an in-progress viewport edit draft.

When a queued or external request is processed while the user is
composing in the viewport edit buffer, `agent-shell--send-command'
must leave that buffer untouched.  It only refreshes the viewport
when it is displaying the conversation (view mode), so an active
compose buffer keeps its draft in place and stays in edit mode."
  (let ((agent-shell-header-style 'graphical)
        (agent-shell-show-busy-indicator nil)
        (agent-shell--state (list (cons :buffer (current-buffer))
                                  (cons :event-subscriptions nil)
                                  (cons :client 'test-client)
                                  (cons :session (list (cons :id "test-session")
                                                       (cons :title "a title")))
                                  (cons :last-entry-type nil)
                                  (cons :tool-calls nil)
                                  (cons :idle-timer nil))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'agent-shell--send-request)
               (lambda (&rest _)))
              ((symbol-function 'agent-shell--append-transcript)
               (lambda (&rest _)))
              ((symbol-function 'agent-shell--set-session-title)
               (lambda (&rest _)))
              ((symbol-function 'agent-shell-viewport--update-header)
               (lambda (&rest _)))
              ((symbol-function 'agent-shell-viewport--position)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (let ((viewport-buffer (current-buffer)))
          (agent-shell-viewport-edit-mode)
          (insert "my important draft")
          (cl-letf (((symbol-function 'agent-shell-viewport--buffer)
                     (lambda (&rest _) viewport-buffer)))
            (agent-shell--send-command
             :prompt "queued prompt"
             :shell-buffer (current-buffer)))
          ;; The draft is left in place, untouched.
          (should (string-match-p "my important draft" (buffer-string)))
          ;; The buffer stays in edit mode, not hijacked to view mode.
          (should (derived-mode-p 'agent-shell-viewport-edit-mode))
          ;; The submitted prompt did not take over the buffer.
          (should-not (string-match-p "queued prompt" (buffer-string)))
          ;; No snapshot is needed since nothing was wiped.
          (should-not agent-shell-viewport--compose-snapshot))))))

(ert-deftest agent-shell-viewport-compose-send-and-dismiss-test ()
  "Composed prompts are queued, cleared, and dismissed or kept.

`agent-shell-viewport--compose-queue' hands the draft to
`agent-shell-prompt-queue' and clears the compose buffer;
`agent-shell-viewport-compose-send-and-dismiss' additionally dismisses
the window.  An empty draft signals an error."
  (let ((agent-shell-header-style 'graphical)
        queued dismissed)
    (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
               (lambda (&rest _) (current-buffer)))
              ((symbol-function 'agent-shell-prompt-queue)
               (lambda (prompt) (setq queued prompt)))
              ((symbol-function 'agent-shell-viewport--dismiss)
               (lambda (&rest _) (setq dismissed t)))
              ((symbol-function 'agent-shell-viewport--position)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-shell-viewport--update-header)
               (lambda (&rest _) nil)))
      ;; Sends, clears the draft, and dismisses the window.
      (with-temp-buffer
        (agent-shell-viewport-edit-mode)
        (insert "my prompt")
        (agent-shell-viewport-compose-send-and-dismiss)
        (should (equal queued "my prompt"))
        (should (string-empty-p (string-trim (buffer-string))))
        (should dismissed))
      ;; Keep composing: sends and clears, but does not dismiss.
      (setq queued nil dismissed nil)
      (with-temp-buffer
        (agent-shell-viewport-edit-mode)
        (insert "another prompt")
        (agent-shell-viewport--compose-queue)
        (should (equal queued "another prompt"))
        (should (string-empty-p (string-trim (buffer-string))))
        (should-not dismissed))
      ;; An empty draft is rejected.
      (setq queued nil)
      (with-temp-buffer
        (agent-shell-viewport-edit-mode)
        (should-error (agent-shell-viewport-compose-send-and-dismiss))
        (should-not queued)))))

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

(ert-deftest agent-shell--diff-line-stats-test ()
  "Test `agent-shell--diff-line-stats' function."
  ;; Test nil input
  (should (equal (agent-shell--diff-line-stats nil) nil))

  ;; Test a replacement of 5 old lines with 23 new lines
  (let* ((old-text (mapconcat (lambda (n) (format "old line %d" n))
                              (number-sequence 1 5) "\n"))
         (new-text (mapconcat (lambda (n) (format "new line %d" n))
                              (number-sequence 1 23) "\n"))
         (stats (agent-shell--diff-line-stats `((:old . ,old-text)
                                                (:new . ,new-text)))))
    (should (equal (map-elt stats :added) 23))
    (should (equal (map-elt stats :removed) 5)))

  ;; Test a new file (no old text)
  (let ((stats (agent-shell--diff-line-stats '((:old . "") (:new . "a\nb\nc")))))
    (should (equal (map-elt stats :added) 3))
    (should (equal (map-elt stats :removed) 0)))

  ;; Test a deletion (no new text)
  (let ((stats (agent-shell--diff-line-stats '((:old . "a\nb\nc\nd") (:new . "")))))
    (should (equal (map-elt stats :added) 0))
    (should (equal (map-elt stats :removed) 4)))

  ;; Test a single-line swap
  (let ((stats (agent-shell--diff-line-stats '((:old . "a\nb\nc") (:new . "a\nB\nc")))))
    (should (equal (map-elt stats :added) 1))
    (should (equal (map-elt stats :removed) 1)))

  ;; Test no change
  (let ((stats (agent-shell--diff-line-stats '((:old . "a\nb") (:new . "a\nb")))))
    (should (equal (map-elt stats :added) 0))
    (should (equal (map-elt stats :removed) 0))))

(ert-deftest agent-shell--format-diff-line-stats-test ()
  "Test `agent-shell--format-diff-line-stats' function."
  ;; Test nil input
  (should (equal (agent-shell--format-diff-line-stats nil) nil))

  ;; Test no change returns nil rather than an empty summary
  (should (equal (agent-shell--format-diff-line-stats
                  '((:old . "a\nb") (:new . "a\nb")))
                 nil))

  ;; Test added and removed
  (should (equal (substring-no-properties
                  (agent-shell--format-diff-line-stats
                   '((:old . "a\nb\nc") (:new . "a\nB\nc"))))
                 "+1 -1"))

  ;; Test additions only (no leading/trailing space)
  (should (equal (substring-no-properties
                  (agent-shell--format-diff-line-stats
                   '((:old . "") (:new . "x\ny\nz"))))
                 "+3"))

  ;; Test deletions only
  (should (equal (substring-no-properties
                  (agent-shell--format-diff-line-stats
                   '((:old . "x\ny") (:new . ""))))
                 "-2")))

(ert-deftest agent-shell--make-diff-infos-test ()
  "Test `agent-shell--make-diff-infos' function."
  ;; Test no diff content
  (should (equal (agent-shell--make-diff-infos
                  :acp-tool-call '((content . [((type . "text") (text . "hello"))])))
                 nil))

  ;; Test a single diff object in content
  (should (equal (agent-shell--make-diff-infos
                  :acp-tool-call '((content . ((type . "diff")
                                               (oldText . "a")
                                               (newText . "b")
                                               (path . "foo.el")))))
                 '(((:old . "a") (:new . "b") (:file . "foo.el")))))

  ;; Test multiple diff items in a content vector (issue #580)
  (should (equal (agent-shell--make-diff-infos
                  :acp-tool-call '((content . [((type . "diff")
                                                (oldText . "a1")
                                                (newText . "b1")
                                                (path . "one.el"))
                                               ((type . "text")
                                                (text . "ignore me"))
                                               ((type . "diff")
                                                (oldText . "a2")
                                                (newText . "b2")
                                                (path . "two.el"))])))
                 '(((:old . "a1") (:new . "b1") (:file . "one.el"))
                   ((:old . "a2") (:new . "b2") (:file . "two.el")))))

  ;; Test oldText defaulting to "" for new files
  (should (equal (agent-shell--make-diff-infos
                  :acp-tool-call '((content . [((type . "diff")
                                                (newText . "new")
                                                (path . "created.el"))])))
                 '(((:old . "") (:new . "new") (:file . "created.el")))))

  ;; Test the ACP `locations' line is carried as a hint when its path
  ;; matches the diff.
  (should (equal (agent-shell--make-diff-infos
                  :acp-tool-call '((content . ((type . "diff")
                                               (oldText . "a")
                                               (newText . "b")
                                               (path . "foo.el")))
                                   (locations . [((path . "foo.el") (line . 42))])))
                 '(((:old . "a") (:new . "b") (:file . "foo.el") (:line . 42)))))

  ;; Test a non-matching location path contributes no hint.
  (should (equal (agent-shell--make-diff-infos
                  :acp-tool-call '((content . ((type . "diff")
                                               (oldText . "a")
                                               (newText . "b")
                                               (path . "foo.el")))
                                   (locations . [((path . "other.el") (line . 42))])))
                 '(((:old . "a") (:new . "b") (:file . "foo.el")))))

  ;; Test a diff from goose's before/after rawInput (issue #569)
  (should (equal (agent-shell--make-diff-infos
                  :acp-tool-call '((rawInput . ((path . "/tmp/test.txt")
                                                (before . "")
                                                (after . "Test")))))
                 '(((:old . "") (:new . "Test") (:file . "/tmp/test.txt"))))))

(ert-deftest agent-shell--diffs-line-stats-test ()
  "Test `agent-shell--diffs-line-stats' aggregates across diffs."
  ;; Test nil input
  (should (equal (agent-shell--diffs-line-stats nil) nil))

  ;; Test counts are summed across every diff
  (let ((stats (agent-shell--diffs-line-stats
                '(((:old . "a\nb\nc") (:new . "a\nB\nc"))
                  ((:old . "") (:new . "x\ny\nz"))))))
    (should (equal (map-elt stats :added) 4))
    (should (equal (map-elt stats :removed) 1))))

(ert-deftest agent-shell--format-diffs-line-stats-test ()
  "Test `agent-shell--format-diffs-line-stats' function."
  ;; Test nil input
  (should (equal (agent-shell--format-diffs-line-stats nil) nil))

  ;; Test aggregated summary across diffs
  (should (equal (substring-no-properties
                  (agent-shell--format-diffs-line-stats
                   '(((:old . "a\nb\nc") (:new . "a\nB\nc"))
                     ((:old . "") (:new . "x\ny\nz")))))
                 "+4 -1")))

(ert-deftest agent-shell--format-diffs-as-text-test ()
  "Test `agent-shell--format-diffs-as-text' function."
  ;; Test nil input
  (should (equal (agent-shell--format-diffs-as-text nil) nil))

  ;; Test each diff is rendered under a header naming its file
  (let ((result (substring-no-properties
                 (agent-shell--format-diffs-as-text
                  '(((:old . "a\n") (:new . "b\n") (:file . "one.el"))
                    ((:old . "c\n") (:new . "d\n") (:file . "two.el")))))))
    (should (string-match-p "one.el" result))
    (should (string-match-p "two.el" result))
    (should (string-match-p "^-a" result))
    (should (string-match-p "^\\+b" result))
    (should (string-match-p "^-c" result))
    (should (string-match-p "^\\+d" result))))

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

(ert-deftest agent-shell--normalize-config-options-test ()
  "Test `agent-shell--normalize-config-options'."
  (let ((options (agent-shell--normalize-config-options
                  [((id . "mode")
                    (name . "Session Mode")
                    (description . "Controls permissions")
                    (category . "mode")
                    (type . "select")
                    (currentValue . "ask")
                    (options . [((value . "ask")
                                 (name . "Ask")
                                 (description . "Ask first"))
                                ((value . "code")
                                 (name . "Code"))]))
                    ((id . "verbosity")
                     (name . "Verbosity")
                     (type . "select")
                     (currentValue . "normal")
                     (options . [((value . "normal")
                                  (name . "Normal"))]))])))
    (should (equal (map-elt (car options) :id) "mode"))
    (should (equal (map-elt (car options) :category) "mode"))
    (should (equal (map-elt (car options) :current-value) "ask"))
    (should (equal (map-elt (car (map-elt (car options) :options)) :value)
                   "ask"))
    (should-not (agent-shell--config-option-by-category
                 (list (cons :config-options options))
                 "model"))))

(ert-deftest agent-shell--format-available-config-options-test ()
  "Test `agent-shell--format-available-config-options' enumerates values."
  (let ((rendered (agent-shell--format-available-config-options
                   (agent-shell--normalize-config-options
                    [((id . "thought_level")
                      (name . "Effort")
                      (description . "Reasoning effort")
                      (category . "thought_level")
                      (type . "select")
                      (currentValue . "high")
                      (options . [((value . "high") (name . "High"))
                                  ((value . "low") (name . "Low"))]))]))))
    ;; The option id (the alist key) is shown, with the description inline.
    (should (string-match-p "id: thought_level): Reasoning effort" rendered))
    ;; The current value shows both its name and id.
    (should (string-match-p "current: High (id: high)" rendered))
    ;; Every selectable value shows its name and the id the alist stores,
    ;; one per line.
    (should (string-match-p "values: High (id: high)" rendered))
    (should (string-match-p "\n *Low (id: low)" rendered))))

(ert-deftest agent-shell--config-option-value-label-test ()
  "Test `agent-shell--config-option-value-label'."
  ;; Annotates the name with its id.
  (should (equal (agent-shell--config-option-value-label "High" "high")
                 "High (id: high)"))
  ;; Falls back to the bare id when the name adds no information.
  (should (equal (agent-shell--config-option-value-label "high" "high")
                 "high"))
  (should (equal (agent-shell--config-option-value-label nil "high")
                 "high")))

(ert-deftest agent-shell--config-option-by-category-prefers-id-match-test ()
  "Test `agent-shell--config-option-by-category' tie-breaks on `:id'.

Cline tags both its `provider' and `model' options with category
\"model\".  Matching on category alone would resolve \"model\" to the
first match (provider); the lookup must prefer the option whose `:id'
equals the category."
  (let* ((options (agent-shell--normalize-config-options
                   [((id . "provider")
                     (name . "Provider")
                     (category . "model")
                     (type . "select")
                     (currentValue . "openai-codex")
                     (options . [((value . "cline") (name . "Cline"))
                                 ((value . "openai-codex")
                                  (name . "OpenAI ChatGPT Subscription"))]))
                    ((id . "model")
                     (name . "Model")
                     (category . "model")
                     (type . "select")
                     (currentValue . "gpt-5.5")
                     (options . [((value . "gpt-5.5") (name . "GPT-5.5"))]))]))
         (state (list (cons :config-options options))))
    ;; "model" category must resolve to the model option, not provider.
    (should (equal (map-elt (agent-shell--config-option-by-category state "model") :id)
                   "model"))
    ;; And available models must list models, not providers.
    (should (equal (mapcar (lambda (m) (map-elt m :model-id))
                           (agent-shell--get-available-models state))
                   '("gpt-5.5")))))

(ert-deftest agent-shell--config-option-by-category-falls-back-to-first-match-test ()
  "Test `agent-shell--config-option-by-category' falls back when no `:id' match.

When a single option carries a category but its `:id' differs from
the category, the option is still returned."
  (let* ((options (agent-shell--normalize-config-options
                   [((id . "model_id")
                     (name . "Model")
                     (category . "model")
                     (type . "select")
                     (currentValue . "sonnet")
                     (options . [((value . "sonnet") (name . "Sonnet"))]))]))
         (state (list (cons :config-options options))))
    (should (equal (map-elt (agent-shell--config-option-by-category state "model") :id)
                   "model_id"))))

(ert-deftest agent-shell--session-from-response-config-options-test ()
  "Test `agent-shell--session-from-response' stores config options."
  (let ((session (agent-shell--session-from-response
                  :acp-session-id "session-1"
                  :acp-response
                  '((configOptions . [((id . "model")
                                       (name . "Model")
                                       (category . "model")
                                       (type . "select")
                                       (currentValue . "gpt-5")
                                       (options . [((value . "gpt-5")
                                                    (name . "GPT-5"))]))])))))
    (should (equal (map-elt session :id) "session-1"))
    (should (equal (map-elt (car (map-elt session :config-options)) :id)
                   "model"))))

(ert-deftest agent-shell--config-option-update-test ()
  "Test config_option_update refreshes config option state."
  (let ((state (list (cons :session (list (cons :id "session-1")
                                          (cons :config-options nil)))
                     (cons :config-options nil)
                     (cons :last-activity-time nil)))
        (config-options [((id . "mode")
                          (name . "Mode")
                          (category . "mode")
                          (type . "select")
                          (currentValue . "code")
                          (options . [((value . "code")
                                       (name . "Code"))]))]))
    (cl-letf (((symbol-function 'agent-shell--update-header-and-mode-line)
               #'ignore)
              ;; `--emit-event' calls `(agent-shell--state)' which errors
              ;; outside of an `agent-shell-mode' buffer; the test exercises
              ;; the data layer, not subscription dispatch.
              ((symbol-function 'agent-shell--emit-event)
               #'ignore))
      (agent-shell--on-notification
       :state state
       :acp-notification `((method . "session/update")
                           (params
                            (update
                             (sessionUpdate . "config_option_update")
                             (configOptions . ,config-options))))))
    (should (equal (map-elt (car (map-elt state :config-options)) :current-value)
                   "code"))))

(ert-deftest agent-shell--config-option-set-model-id-config-option-test ()
  "Test model changes prefer session config options."
  (let* ((initial-config-options [((id . "model")
                                   (name . "Model")
                                   (category . "model")
                                   (type . "select")
                                   (currentValue . "gpt-5")
                                   (options . [((value . "gpt-5")
                                                (name . "GPT-5"))
                                               ((value . "gpt-5.5")
                                                (name . "GPT-5.5"))]))])
         (normalized-options (agent-shell--normalize-config-options
                              initial-config-options))
         (state (list (cons :client 'test-client)
                      (cons :session (list (cons :id "session-1")
                                           (cons :config-options normalized-options)))
                      (cons :config-options normalized-options)))
         (sent-request nil)
         (success-callback nil)
         (updated-config-options [((id . "model")
                                   (name . "Model")
                                   (category . "model")
                                   (type . "select")
                                   (currentValue . "gpt-5.5")
                                   (options . [((value . "gpt-5")
                                                (name . "GPT-5"))
                                               ((value . "gpt-5.5")
                                                (name . "GPT-5.5"))]))]))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () state))
              ((symbol-function 'agent-shell--send-request)
               (lambda (&rest args)
                 (setq sent-request (plist-get args :request))
                 (setq success-callback (plist-get args :on-success))))
              ((symbol-function 'agent-shell--update-header-and-mode-line)
               #'ignore))
      (agent-shell--config-option-set-model-id :model-id "gpt-5.5")
      (should (equal (map-elt sent-request :method)
                     "session/set_config_option"))
      (should (equal (map-nested-elt sent-request '(:params configId))
                     "model"))
      (funcall success-callback
               `((configOptions . ,updated-config-options)))
      (should (equal (agent-shell--current-model-id state) "gpt-5.5")))))

(ert-deftest agent-shell--config-option-set-mode-id-config-option-test ()
  "Test mode changes prefer session config options."
  (let* ((initial-config-options [((id . "mode")
                                   (name . "Mode")
                                   (category . "mode")
                                   (type . "select")
                                   (currentValue . "ask")
                                   (options . [((value . "ask")
                                                (name . "Ask"))
                                               ((value . "auto")
                                                (name . "Auto"))]))])
         (normalized-options (agent-shell--normalize-config-options
                              initial-config-options))
         (state (list (cons :client 'test-client)
                      (cons :session (list (cons :id "session-1")
                                           (cons :config-options normalized-options)))
                      (cons :config-options normalized-options)))
         (sent-request nil)
         (success-callback nil)
         (updated-config-options [((id . "mode")
                                   (name . "Mode")
                                   (category . "mode")
                                   (type . "select")
                                   (currentValue . "auto")
                                   (options . [((value . "ask")
                                                (name . "Ask"))
                                               ((value . "auto")
                                                (name . "Auto"))]))]))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () state))
              ((symbol-function 'agent-shell--send-request)
               (lambda (&rest args)
                 (setq sent-request (plist-get args :request))
                 (setq success-callback (plist-get args :on-success))))
              ((symbol-function 'agent-shell--update-header-and-mode-line)
               #'ignore))
      (agent-shell--config-option-set-mode-id :mode-id "auto")
      (should (equal (map-elt sent-request :method)
                     "session/set_config_option"))
      (should (equal (map-nested-elt sent-request '(:params configId))
                     "mode"))
      (should (equal (map-nested-elt sent-request '(:params value))
                     "auto"))
      (funcall success-callback
               `((configOptions . ,updated-config-options)))
      (should (equal (agent-shell--current-mode-id state) "auto")))))

(ert-deftest agent-shell--config-option-set-model-id-legacy-fallback-test ()
  "Test model changes fall back to legacy ACP model requests."
  (let* ((models '(((:model-id . "gpt-5")
                    (:name . "GPT-5"))
                   ((:model-id . "gpt-5.5")
                    (:name . "GPT-5.5"))))
         (session (list (cons :id "session-1")
                        (cons :model-id "gpt-5")
                        (cons :models models)))
         (state (list (cons :client 'test-client)
                      (cons :session session)))
         (sent-request nil)
         (success-callback nil))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () state))
              ((symbol-function 'agent-shell--send-request)
               (lambda (&rest args)
                 (setq sent-request (plist-get args :request))
                 (setq success-callback (plist-get args :on-success))))
              ((symbol-function 'agent-shell--update-header-and-mode-line)
               #'ignore))
      (agent-shell--config-option-set-model-id :model-id "gpt-5.5")
      (should (equal (map-elt sent-request :method)
                     "session/set_model"))
      (should (equal (map-nested-elt sent-request '(:params modelId))
                     "gpt-5.5"))
      (funcall success-callback nil)
      (should (equal (agent-shell--current-model-id state) "gpt-5.5")))))

(ert-deftest agent-shell--config-option-set-model-id-config-option-no-echo-test ()
  "Test model changes update local state when response omits configOptions."
  (let* ((initial-config-options [((id . "model")
                                   (name . "Model")
                                   (category . "model")
                                   (type . "select")
                                   (currentValue . "gpt-5")
                                   (options . [((value . "gpt-5")
                                                (name . "GPT-5"))
                                               ((value . "gpt-5.5")
                                                (name . "GPT-5.5"))]))])
         (normalized-options (agent-shell--normalize-config-options
                              initial-config-options))
         (state (list (cons :client 'test-client)
                      (cons :session (list (cons :id "session-1")
                                           (cons :config-options normalized-options)))
                      (cons :config-options normalized-options)))
         (success-callback nil))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () state))
              ((symbol-function 'agent-shell--send-request)
               (lambda (&rest args)
                 (setq success-callback (plist-get args :on-success))))
              ((symbol-function 'agent-shell--update-header-and-mode-line)
               #'ignore))
      (agent-shell--config-option-set-model-id :model-id "gpt-5.5")
      (funcall success-callback nil)
      (should (equal (agent-shell--current-model-id state) "gpt-5.5")))))

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

(ert-deftest agent-shell--separate-transcript-after-agent-message-test ()
  "Ensure a turn ending mid-agent-message leaves a blank-line separator.

Reproduces the interrupted-turn bug where an interrupted agent
message had no trailing newline, so the next `## User' heading was
glued onto the same line as the partial message:

    Actually, I should## User (2026-06-20 19:45:42)

The separator must be written whether the turn ends in success or
failure (interrupt), so `agent-shell--append-transcript' can be
driven by a single helper on both paths."
  (let ((file (make-temp-file "agent-shell-transcript")))
    (unwind-protect
        ;; `agent-shell--ensure-transcript-file' guards on the major mode
        ;; and creates the file with a header; the file is pre-seeded
        ;; here, so stub it to just hand back the path and exercise the
        ;; real conditional + real `write-region' append.
        (cl-letf (((symbol-function 'agent-shell--ensure-transcript-file)
                   (lambda () file)))
          ;; Simulate content left by an interrupted agent_message_chunk:
          ;; a header plus partial body with NO trailing newline.
          (write-region (format "## Agent (%s)\n\nActually, I should"
                                (format-time-string "%F %T"))
                        nil file)
          ;; The turn ending mid-message must add the blank-line separator.
          (agent-shell--separate-transcript-after-agent-message
           :last-entry-type "agent_message_chunk"
           :file-path file)
          (with-temp-buffer
            (insert-file-contents file)
            ;; The next `## User' must land on its own line, i.e. the
            ;; agent's partial body must be followed by a blank line.
            (should (string-suffix-p "Actually, I should\n\n"
                                     (buffer-string))))
          ;; When the turn did not end on an agent message, no separator
          ;; is written (avoids spurious blank lines elsewhere).
          (let ((size-before (file-attribute-size
                              (file-attributes file))))
            (agent-shell--separate-transcript-after-agent-message
             :last-entry-type "tool_call"
             :file-path file)
            (should (= (file-attribute-size (file-attributes file))
                       size-before))))
      (delete-file file))))

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

  ;; Test stdio transport defaults missing ACP collection fields
  (let ((agent-shell-mcp-servers
         '(((name . "simple")
            (command . "simple-server")))))
    (should (equal (agent-shell--mcp-servers)
                   [((name . "simple")
                     (command . "simple-server")
                     (args . [])
                     (env . []))])))

  ;; Test HTTP transport defaults missing ACP collection fields
  (let ((agent-shell-mcp-servers
         '(((name . "remote")
            (type . "http")
            (url . "https://example.com/mcp")))))
    (should (equal (agent-shell--mcp-servers)
                   [((name . "remote")
                     (type . "http")
                     (url . "https://example.com/mcp")
                     (headers . []))]))))

(ert-deftest agent-shell-mcp-servers-per-agent-test ()
  "Per-agent `:mcp-servers' take precedence over `agent-shell-mcp-servers'."
  (let ((agent-shell-mcp-servers
         '(((name . "global") (type . "http") (url . "https://global/mcp")))))
    ;; The agent config's servers win when present.
    (let ((agent-shell--state
           (agent-shell--make-state
            :agent-config (agent-shell-make-agent-config
                           :identifier 'test
                           :mcp-servers '(((name . "per-agent")
                                           (type . "http")
                                           (url . "https://per-agent/mcp")))))))
      (should (equal (agent-shell--mcp-servers)
                     [((name . "per-agent")
                       (type . "http")
                       (url . "https://per-agent/mcp")
                       (headers . []))])))
    ;; Falls back to the global variable when the config has no override.
    (let ((agent-shell--state
           (agent-shell--make-state
            :agent-config (agent-shell-make-agent-config :identifier 'test))))
      (should (equal (agent-shell--mcp-servers)
                     [((name . "global")
                       (type . "http")
                       (url . "https://global/mcp")
                       (headers . []))])))))

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

(ert-deftest agent-shell--emit-event-isolates-throwing-subscriber-test ()
  "Test `agent-shell--emit-event' isolates a throwing subscriber.
A subscriber signaling an error must not abort dispatch to the
remaining subscribers nor propagate out of `agent-shell--emit-event'."
  (let* ((received-events nil)
         (agent-shell--state (list (cons :buffer (current-buffer))
                                   (cons :event-subscriptions nil))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer)
       :on-event (lambda (_event) (error "Boom")))
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer)
       :on-event (lambda (event) (push event received-events)))

      ;; Should not propagate the subscriber's error.
      (agent-shell--emit-event :event 'init-client)

      ;; The non-throwing subscriber must still have received the event.
      (should (= (length received-events) 1))
      (should (equal (map-elt (car received-events) :event) 'init-client)))))

(ert-deftest agent-shell--sync-system-sleep-tracks-status-test ()
  "Test system sleep tracks `agent-shell-status' across a turn."
  ;; The `cl-letf' below is what makes
  ;; `agent-shell--system-sleep-available-p' return non-nil, so these
  ;; run on Emacs < 31 too, where the library is absent.
  (let ((blocked 0)
        (status 'busy)
        (state (list (cons :buffer (current-buffer))
                     (cons :event-subscriptions nil)
                     (cons :sleep-token nil)))
        (agent-shell-inhibit-system-sleep t))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () state))
              ((symbol-function 'agent-shell-status)
               (lambda (&rest _) status))
              ((symbol-function 'system-sleep-block-sleep)
               (lambda (&rest _) (setq blocked (1+ blocked)) 'token))
              ((symbol-function 'system-sleep-unblock-sleep)
               (lambda (_token) (setq blocked (1- blocked)))))
      ;; Busy blocks sleep.
      (setq status 'busy)
      (agent-shell--emit-event :event 'input-submitted)
      (should (equal (map-elt state :sleep-token) 'token))
      (should (= blocked 1))

      ;; Blocked (waiting on a permission) is waiting on user input, so
      ;; it releases the block.
      (setq status 'blocked)
      (agent-shell--emit-event :event 'permission-request)
      (should-not (map-elt state :sleep-token))
      (should (= blocked 0))

      ;; Resuming work blocks it again.
      (setq status 'busy)
      (agent-shell--emit-event :event 'permission-response)
      (should (= blocked 1))

      ;; Ready releases it.
      (setq status 'ready)
      (agent-shell--emit-event :event 'turn-complete)
      (should-not (map-elt state :sleep-token))
      (should (= blocked 0)))))

(ert-deftest agent-shell--sync-system-sleep-terminal-event-releases-test ()
  "Test `error'/`clean-up' release the block even when status reads busy."
  ;; The `cl-letf' below is what makes
  ;; `agent-shell--system-sleep-available-p' return non-nil, so these
  ;; run on Emacs < 31 too, where the library is absent.
  (let ((blocked 0)
        (state (list (cons :buffer (current-buffer))
                     (cons :event-subscriptions nil)
                     (cons :sleep-token nil)))
        (agent-shell-inhibit-system-sleep t))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () state))
              ((symbol-function 'agent-shell-status)
               (lambda (&rest _) 'busy))
              ((symbol-function 'system-sleep-block-sleep)
               (lambda (&rest _) (setq blocked (1+ blocked)) 'token))
              ((symbol-function 'system-sleep-unblock-sleep)
               (lambda (_token) (setq blocked (1- blocked)))))
      (agent-shell--emit-event :event 'input-submitted)
      (should (= blocked 1))
      ;; Status still reports busy, but a terminal event must release.
      (agent-shell--emit-event :event 'error)
      (should-not (map-elt state :sleep-token))
      (should (= blocked 0)))))

(ert-deftest agent-shell--sync-system-sleep-disabled-is-noop-test ()
  "Test no sleep block is acquired when the option is nil."
  (let ((require-calls 0)
        (state (list (cons :buffer (current-buffer))
                     (cons :event-subscriptions nil)
                     (cons :sleep-token nil)))
        (agent-shell--system-sleep-load-attempted nil)
        (agent-shell-inhibit-system-sleep nil))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () state))
              ((symbol-function 'agent-shell-status)
               (lambda (&rest _) 'busy))
              ((symbol-function 'require)
               (lambda (&rest _)
                 (setq require-calls (1+ require-calls))
                 nil))
              ((symbol-function 'system-sleep-block-sleep) nil))
      (agent-shell--emit-event :event 'input-submitted)
      (should-not (map-elt state :sleep-token))
      (should (= 0 require-calls)))))

(ert-deftest agent-shell--sync-system-sleep-unavailable-loads-once-test ()
  "Test unavailable sleep support is loaded at most once."
  (let ((require-calls 0)
        (state (list (cons :buffer (current-buffer))
                     (cons :event-subscriptions nil)
                     (cons :sleep-token nil)))
        (agent-shell--system-sleep-load-attempted nil)
        (agent-shell-inhibit-system-sleep t))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () state))
              ((symbol-function 'agent-shell-status)
               (lambda (&rest _) 'busy))
              ((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (should (eq feature 'system-sleep))
                 (setq require-calls (1+ require-calls))
                 nil))
              ((symbol-function 'system-sleep-block-sleep) nil))
      (agent-shell--emit-event :event 'input-submitted)
      (agent-shell--emit-event :event 'tool-call-update)
      (should (= 1 require-calls))
      (should-not (map-elt state :sleep-token)))))

(ert-deftest agent-shell--sync-system-sleep-single-token-test ()
  "Test repeated busy events don't leak extra blocks."
  ;; The `cl-letf' below is what makes
  ;; `agent-shell--system-sleep-available-p' return non-nil, so these
  ;; run on Emacs < 31 too, where the library is absent.
  (let ((blocked 0)
        (state (list (cons :buffer (current-buffer))
                     (cons :event-subscriptions nil)
                     (cons :sleep-token nil)))
        (agent-shell-inhibit-system-sleep t))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () state))
              ((symbol-function 'agent-shell-status)
               (lambda (&rest _) 'busy))
              ((symbol-function 'system-sleep-block-sleep)
               (lambda (&rest _) (setq blocked (1+ blocked)) 'token))
              ((symbol-function 'system-sleep-unblock-sleep)
               (lambda (_token) (setq blocked (1- blocked)))))
      (agent-shell--emit-event :event 'input-submitted)
      (agent-shell--emit-event :event 'tool-call-update)
      (should (= blocked 1))

      (agent-shell--emit-event :event 'clean-up)
      (should (= blocked 0)))))

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


(ert-deftest agent-shell-send-dwim-with-prefix-appends-context-once-test ()
  "Test `agent-shell-send-dwim' with a prefix arg appends context once.

With \\[universal-argument] \\[universal-argument], the command picks an
existing shell via `agent-shell--dwim' and must append the DWIM context to
the viewport exactly once.  `agent-shell--dwim' already performs the append,
so the command must not append a second time."
  (let ((agent-shell-prefer-viewport-interaction t))
    (with-temp-buffer
      (let ((source-buffer (current-buffer))
            (shell-buffer (generate-new-buffer " *agent-shell shell*"))
            (appends nil))
        (unwind-protect
            (progn
              (with-current-buffer shell-buffer
                (setq-local agent-shell-session-strategy 'reuse)
                (setq-local agent-shell--state
                            `((:buffer . ,shell-buffer)
                              (:session . ((:id . "session-1"))))))
              (cl-letf (((symbol-function 'agent-shell--shell-buffer)
                         (lambda (&rest _) shell-buffer))
                        ((symbol-function 'agent-shell--read-shell-buffer)
                         (lambda (&rest _) shell-buffer))
                        ((symbol-function 'agent-shell--context)
                         (lambda (&key shell-buffer)
                           (ignore shell-buffer)
                           "context from source"))
                        ((symbol-function 'shell-maker-busy)
                         (lambda (&rest _) nil))
                        ((symbol-function 'agent-shell-viewport--show-buffer)
                         (lambda (&rest args)
                           (push (plist-get args :append) appends))))
                (with-current-buffer source-buffer
                  (agent-shell-send-dwim '(16)))
                (should (equal appends '("context from source")))))
          (kill-buffer shell-buffer))))))

(ert-deftest agent-shell-send-dwim-without-prefix-appends-context-once-test ()
  "Test `agent-shell-send-dwim' without a prefix arg appends context once."
  (let ((agent-shell-prefer-viewport-interaction t))
    (with-temp-buffer
      (let ((source-buffer (current-buffer))
            (shell-buffer (generate-new-buffer " *agent-shell shell*"))
            (appends nil))
        (unwind-protect
            (cl-letf (((symbol-function 'agent-shell--shell-buffer)
                       (lambda (&rest _) shell-buffer))
                      ((symbol-function 'agent-shell--context)
                       (lambda (&key shell-buffer)
                         (ignore shell-buffer)
                         "context from source"))
                      ((symbol-function 'shell-maker-busy)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-shell-viewport--show-buffer)
                       (lambda (&rest args)
                         (push (plist-get args :append) appends))))
              (with-current-buffer source-buffer
                (agent-shell-send-dwim))
              (should (equal appends '("context from source"))))
          (kill-buffer shell-buffer))))))
(ert-deftest agent-shell--on-request-emits-permission-request-event-test ()
  "Test `agent-shell--on-request' emits permission-request event."
  (let ((received-events nil)
        (agent-shell--state (list (cons :buffer (current-buffer))
                                  (cons :event-subscriptions nil)
                                  (cons :tool-calls nil)
                                  (cons :last-entry-type nil)
                                  (cons :idle-timer nil))))
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
          (cl-letf (((symbol-function 'shell-maker-start-v2)
                     (lambda (&rest _args)
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

(ert-deftest agent-shell--start-snapshots-dynamic-session-strategy ()
  "Starting a shell should localize the effective session strategy."
  (let ((test-buffer nil)
        (shell-buffer nil)
        (fake-process (start-process "fake-agent" nil "cat"))
        (config (list (cons :buffer-name "test-agent")
                      (cons :client-maker
                            (lambda (_buf)
                              (list (cons :command "cat")))))))
    (unwind-protect
        (cl-letf (((symbol-function 'shell-maker-start-v2)
                   (lambda (&rest _args)
                     (setq test-buffer (get-buffer-create "*test-agent-shell*"))
                     (with-current-buffer test-buffer
                       (setq major-mode 'agent-shell-mode))
                     test-buffer))
                  ((symbol-function 'shell-maker--process) (lambda () fake-process))
                  ((symbol-function 'shell-maker-finish-output) #'ignore)
                  ((symbol-function 'agent-shell--handle) #'ignore)
                  (agent-shell-file-completion-enabled nil))
          (let ((agent-shell-session-strategy 'latest))
            (let ((agent-shell-session-strategy 'prompt))
              (setq shell-buffer (agent-shell--start :config config
                                                    :no-focus t
                                                    :new-session t))))
          (should (local-variable-p 'agent-shell-session-strategy shell-buffer))
          (should (eq (buffer-local-value 'agent-shell-session-strategy shell-buffer)
                      'prompt)))
      (when (process-live-p fake-process)
        (delete-process fake-process))
      (when (and shell-buffer (buffer-live-p shell-buffer))
        (kill-buffer shell-buffer))
      (when (and test-buffer (buffer-live-p test-buffer))
        (kill-buffer test-buffer)))))

(ert-deftest agent-shell--initiate-session-prefers-list-and-load-when-supported ()
  "Test `agent-shell--initiate-session' prefers session/list + session/load."
  (with-temp-buffer
    (let* ((agent-shell-session-strategy 'latest)
           (requests '())
           (session-init-called nil)
           ;; Build the state with `list'/`cons' (not backquote) so the
           ;; alist cells are fresh and `map-put!' can mutate in place.
           ;; Literal cells from a backquoted template signal
           ;; `map-not-inplace' on the first put.  `:pending-restore'
           ;; must be present (initialized to nil) because
           ;; `agent-shell--initiate-session-list-and-load' calls
           ;; `map-put!' on it after a successful list response.
           (state (list (cons :buffer (current-buffer))
                        (cons :client 'test-client)
                        (cons :session (list (cons :id nil)
                                             (cons :mode-id nil)
                                             (cons :modes nil)))
                        (cons :supports-session-list t)
                        (cons :supports-session-load t)
                        (cons :pending-restore nil)
                        (cons :active-requests nil)
                        (cons :event-subscriptions nil))))
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

(ert-deftest agent-shell--apply-session-choices-test ()
  "Test `agent-shell--apply-session-choices'."
  (let ((choices '(("New shell" . :new-shell)
                   ("New Downloads shell" . :downloads-shell)
                   ("New temp shell" . :temp-shell))))
    ;; nil passes choices through unchanged.
    (let ((agent-shell-session-choices-function nil))
      (should (equal (agent-shell--apply-session-choices choices) choices)))
    ;; A filtering function returns the surviving subset.
    (let ((agent-shell-session-choices-function
           (lambda (candidates)
             (seq-remove (lambda (choice) (eq (cdr choice) :temp-shell))
                         candidates))))
      (should (equal (agent-shell--apply-session-choices choices)
                     '(("New shell" . :new-shell)
                       ("New Downloads shell" . :downloads-shell)))))
    ;; Relabeling a choice (same token, new label) is allowed.
    (let ((agent-shell-session-choices-function
           (lambda (candidates)
             (mapcar (lambda (choice)
                       (if (eq (cdr choice) :new-shell)
                           (cons "Start fresh" :new-shell)
                         choice))
                     candidates))))
      (should (equal (agent-shell--apply-session-choices choices)
                     '(("Start fresh" . :new-shell)
                       ("New Downloads shell" . :downloads-shell)
                       ("New temp shell" . :temp-shell)))))
    ;; A non-list return signals an error.
    (let ((agent-shell-session-choices-function (lambda (_candidates) "nope")))
      (should-error (agent-shell--apply-session-choices choices) :type 'user-error))
    ;; An empty return signals an error.
    (let ((agent-shell-session-choices-function (lambda (_candidates) nil)))
      (should-error (agent-shell--apply-session-choices choices) :type 'user-error))
    ;; A choice that was not offered signals an error.
    (let ((agent-shell-session-choices-function (lambda (_candidates) '(("Bogus" . :bogus)))))
      (should-error (agent-shell--apply-session-choices choices) :type 'user-error))))

(ert-deftest agent-shell--prompt-select-session-new-shell-test ()
  "Test `agent-shell--prompt-select-session' returns nil for the new-shell choice."
  (let ((noninteractive nil)
        (other-buffer (get-buffer-create "*other-agent-shell*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-buffers)
                   (lambda () (list other-buffer)))
                  ((symbol-function 'agent-shell--emit-event) #'ignore)
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "New shell")))
          (should-not (agent-shell--prompt-select-session nil)))
      (kill-buffer other-buffer))))

(ert-deftest agent-shell--prompt-select-session-single-choice-test ()
  "Test `agent-shell--prompt-select-session' skips the prompt for a lone choice."
  (let ((noninteractive nil)
        (other-buffer (get-buffer-create "*other-agent-shell*"))
        (agent-shell-session-choices-function
         (lambda (choices)
           (seq-filter (lambda (choice) (eq (cdr choice) :new-shell)) choices))))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-buffers)
                   (lambda () (list other-buffer)))
                  ((symbol-function 'agent-shell--emit-event) #'ignore)
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) (error "Should not have prompted"))))
          (should-not (agent-shell--prompt-select-session nil)))
      (kill-buffer other-buffer))))

(ert-deftest agent-shell--validate-session-strategy-test ()
  "Test `agent-shell--validate-session-strategy' against every input.
It accepts the supported values and rejects `new-deferred' along with
other unknown ones."
  (should-not (agent-shell--validate-session-strategy 'new))
  (should-not (agent-shell--validate-session-strategy 'latest))
  (should-not (agent-shell--validate-session-strategy 'prompt))
  (should-error (agent-shell--validate-session-strategy 'new-deferred)
                :type 'user-error)
  (should-error (agent-shell--validate-session-strategy 'bogus)
                :type 'user-error))

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

  ;; ACP rawInput may be a scalar, such as a patch string.
  (should (null (agent-shell--extract-tool-parameters
                 "*** Begin Patch\n*** Update File: example.el")))
  (should (null (agent-shell--extract-tool-parameters 42)))

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
                 '((plan . "Step 1: do something")))))

  ;; Ignore malformed entries without preventing valid parameters from rendering.
  (should (equal (agent-shell--extract-tool-parameters
                  '((filePath . "/home/user/file.txt") 42))
                 "filePath: /home/user/file.txt")))

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
              'agent-shell-session-directory))
  (should (eq (agent-shell--session-column-face 'title)
              'agent-shell-session-title))
  (should (eq (agent-shell--session-column-face 'date)
              'agent-shell-session-date))
  (should (eq (agent-shell--session-column-face 'session-id)
              'agent-shell-session-id))
  ;; unknown has no face
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

(ert-deftest agent-shell--make-header-graphical-status-fg-test ()
  "Test graphical header honors a propertized `:status' foreground."
  (skip-unless (image-type-available-p 'svg))
  (with-temp-buffer
    (setq-local agent-shell--state
                `((:agent-config . ((:buffer-name . "Test")
                                    (:icon-name . nil)))
                  (:session . ((:id . "abc")
                               (:model-id . nil)
                               (:models . nil)
                               (:mode-id . nil)
                               (:modes . nil)))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'agent-shell--context-usage-indicator)
               (lambda () nil))
              ((symbol-function 'agent-shell--busy-indicator-frame)
               (lambda () nil))
              ((symbol-function 'agent-shell--session-id-indicator)
               (lambda () nil)))
      (let* ((agent-shell-header-style 'graphical)
             (agent-shell--header-cache nil)
             (header (agent-shell--make-header
                      agent-shell--state
                      :position "1/3"
                      :status (propertize "Edit" 'face '(:foreground "#00ff00"))))
             (svg-data (plist-get (cdr (get-text-property 1 'display header))
                                  :data)))
        (should (string-match-p ">1/3</tspan>" svg-data))
        (should (string-match-p "<tspan[^>]*fill=\"#00ff00\"[^>]*>Edit"
                                svg-data))))))

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

(ert-deftest agent-shell--dot-subdir-ensures-gitignore-for-in-repo-directory-test ()
  "Test that `agent-shell--dot-subdir' ensures gitignore for in-repo data."
  (let* ((temp-dir (make-temp-file "agent-shell-test" t))
         (ensure-gitignore-called-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () temp-dir))
                  ((symbol-function 'agent-shell--ensure-gitignore)
                   (lambda (project-root)
                     (setq ensure-gitignore-called-with project-root))))
          (let ((agent-shell-dot-subdir-function #'agent-shell--dot-subdir-in-repo))
            (agent-shell--dot-subdir "screenshots")
            (should (equal ensure-gitignore-called-with temp-dir))))
      (delete-directory temp-dir t))))

(ert-deftest agent-shell--dot-subdir-skips-gitignore-for-external-directory-test ()
  "Test that `agent-shell--dot-subdir' skips gitignore for external data."
  (let ((project-dir (make-temp-file "agent-shell-project" t))
        (data-dir (make-temp-file "agent-shell-data" t))
        (ensure-gitignore-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () project-dir))
                  ((symbol-function 'agent-shell--ensure-gitignore)
                   (lambda (_project-root)
                     (setq ensure-gitignore-called t))))
          (let ((agent-shell-dot-subdir-function
                 (lambda (subdir)
                   (expand-file-name subdir data-dir))))
            (agent-shell--dot-subdir "screenshots")
            (should-not ensure-gitignore-called)))
      (delete-directory project-dir t)
      (delete-directory data-dir t))))

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

(ert-deftest agent-shell--ensure-gitignore-writes-to-git-exclude-test ()
  "Test that `agent-shell--ensure-gitignore' writes /.agent-shell/ to .git/info/exclude."
  (let ((temp-dir (make-temp-file "agent-shell-test" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          (process-file "git" nil nil nil "init")
          (agent-shell--ensure-gitignore temp-dir)
          (should (string-match-p
                   (regexp-quote "/.agent-shell/")
                   (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name ".git/info/exclude" temp-dir))
                     (buffer-string)))))
      (delete-directory temp-dir t))))


(ert-deftest agent-shell--ensure-gitignore-noop-when-already-ignored-test ()
  "Test that `agent-shell--ensure-gitignore' does not add a duplicate entry."
  (let ((temp-dir (make-temp-file "agent-shell-test" t)))
    (unwind-protect
        (let* ((default-directory temp-dir)
               (exclude-file (expand-file-name ".git/info/exclude" temp-dir)))
          (process-file "git" nil nil nil "init")
          (make-directory (expand-file-name ".agent-shell" temp-dir) t)
          (write-region "/.agent-shell/\n" nil exclude-file t 'no-message)
          (agent-shell--ensure-gitignore temp-dir)
          (should (= 1 (with-temp-buffer
                         (insert-file-contents exclude-file)
                         (count-matches (regexp-quote "/.agent-shell/")
                                        (point-min) (point-max))))))
      (delete-directory temp-dir t))))

(ert-deftest agent-shell--on-request-calls-permission-request-handler-test ()
  "Test `agent-shell--on-request' calls handler and :respond auto-approves."
  (with-temp-buffer
    (let* ((responded-option-id nil)
           (received-events nil)
           (handler-received nil)
           (agent-shell-permission-responder-function
            (lambda (request)
              (setq handler-received request)
              (when-let* ((opt (seq-find
                                (lambda (o) (equal (map-elt o :kind) "allow_once"))
                                (map-elt request :options))))
                (funcall (map-elt request :respond)
                         (map-elt opt :option-id)))))
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:tool-calls . nil)
                    (:last-entry-type . nil)
                    (:event-subscriptions . nil)
                    (:idle-timer . nil))))
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
        (agent-shell-subscribe-to
         :shell-buffer (current-buffer)
         :event 'permission-request
         :on-event (lambda (event)
                     (push event received-events)))
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
        (should (equal responded-option-id "opt-allow"))
        (should-not received-events)
        (should-not (map-elt state :idle-timer))))))

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
                    (:event-subscriptions . nil)
                    (:idle-timer . nil))))
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
          (should (eq (get-text-property 0 'face result) 'agent-shell-success)))))))

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
          (should (eq (get-text-property 0 'face result) 'agent-shell-success)))))))

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
          (should (eq (get-text-property 0 'face result) 'agent-shell-warning)))))))

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
            :tool-call
            '((:title . "external_directory")
              (:raw-input . ((filepath . "/home/pmw/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/aws-sdk-s3-1.112.0/src/types/_event.rs")
                             (parentDir . "/home/pmw/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/aws-sdk-s3-1.112.0/src/types")))
              (:kind . "other"))))))

(ert-deftest agent-shell--permission-title-edit-shows-filename-test ()
  "Test `agent-shell--permission-title' includes filename for edit permission.
Based on ACP traffic from https://github.com/xenodium/agent-shell/issues/415."
  (should (equal
           "edit (s3notifications.rs)"
           (agent-shell--permission-title
            :tool-call
            '((:title . "edit")
              (:raw-input . ((filepath . "/home/pmw/Repos/warmup-s3-archives/src/s3notifications.rs")
                             (diff . "Index: /home/pmw/Repos/warmup-s3-archives/src/s3notifications.rs\n")))
              (:kind . "edit"))))))

(ert-deftest agent-shell--permission-title-no-duplicate-filename-test ()
  "Test `agent-shell--permission-title' does not duplicate filename already in title."
  (should (equal
           "Read s3notifications.rs"
           (agent-shell--permission-title
            :tool-call
            '((:title . "Read s3notifications.rs")
              (:raw-input . ((filepath . "/home/user/src/s3notifications.rs")))
              (:kind . "read"))))))

(ert-deftest agent-shell--permission-title-non-string-path-test ()
  "Test `agent-shell--permission-title' ignores a non-string `path'.
Some tools use `path' for a non-filesystem value (e.g. an HTTP
API's path params), so it must not be fed to `file-name-nondirectory'."
  (should (equal
           "some_tool"
           (agent-shell--permission-title
            :tool-call
            '((:title . "some_tool")
              (:raw-input . ((path . ((id . "abc")))
                             (body . ((value . 1)))))
              (:kind . "other"))))))

(ert-deftest agent-shell--permission-title-fetch-shows-url-test ()
  "Test `agent-shell--permission-title' surfaces the full URL for fetch tools.
Based on OpenCode webfetch traffic from
https://github.com/xenodium/agent-shell/issues/745, where a later
`tool_call_update' clobbers the descriptive title back to
\"webfetch\", so the URL must be recovered from `rawInput.url'."
  (should (equal
           "webfetch (https://en.wikipedia.org/wiki/Emacs)"
           (agent-shell--permission-title
            :tool-call
            '((:title . "webfetch")
              (:raw-input . ((url . "https://en.wikipedia.org/wiki/Emacs")
                             (format . "markdown")))
              (:kind . "fetch"))))))

(ert-deftest agent-shell--permission-title-no-duplicate-url-test ()
  "Test `agent-shell--permission-title' does not duplicate a URL already in title.
The `session/request_permission' title is often the URL itself."
  (should (equal
           "https://en.wikipedia.org/wiki/Emacs"
           (agent-shell--permission-title
            :tool-call
            '((:title . "https://en.wikipedia.org/wiki/Emacs")
              (:raw-input . ((url . "https://en.wikipedia.org/wiki/Emacs")
                             (format . "markdown")))
              (:kind . "fetch"))))))

(ert-deftest agent-shell--permission-title-execute-fenced-test ()
  "Test `agent-shell--permission-title' fences execute commands."
  (should (equal
           "```console\nls -la\n```"
           (agent-shell--permission-title
            :tool-call
            '((:title . "Bash")
              (:raw-input . ((command . "ls -la")))
              (:kind . "execute"))))))

(ert-deftest agent-shell--permission-title-content-folded-test ()
  "Append ACP `content' text after the title.
Based on Jane Street AIDE permission requests from
https://github.com/xenodium/agent-shell-js/issues/27 where
structured `content' carries the user-facing detail and there
is no `rawInput'."
  (should (equal
           "Link Feature\n\nAllow linking to this session?"
           (agent-shell--permission-title
            :tool-call
            `((:title . "Link Feature")
              (:kind . "other")
              (:content . [((type . "content")
                            (content (type . "text")
                                     (text . "Allow linking to this session?")))]))))))

(ert-deftest agent-shell--permission-title-content-dedup-against-title-test ()
  "Skip `content' text already mentioned in the title.
Claude populates `content' with the same string as
`rawInput.description'; we should not duplicate the description
when it is already in the rendered text."
  (should (equal
           "```console\nping -c 4 localhost\n```\n\nPing localhost 4 times"
           (agent-shell--permission-title
            :tool-call
            `((:title . "ping -c 4 localhost")
              (:kind . "execute")
              (:raw-input . ((command . "ping -c 4 localhost")
                             (description . "Ping localhost 4 times")))
              (:content . [((type . "content")
                            (content (type . "text")
                                     (text . "Ping localhost 4 times")))]))))))

(ert-deftest agent-shell--permission-title-content-substring-skipped-test ()
  "Skip `content' text that is a substring of the existing title."
  (should (equal
           "Read foo.rs"
           (agent-shell--permission-title
            :tool-call
            `((:title . "Read foo.rs")
              (:kind . "read")
              (:content . [((type . "content")
                            (content (type . "text")
                                     (text . "Read foo.rs")))]))))))

(ert-deftest agent-shell--permission-title-locations-appended-test ()
  "Append `locations' paths not already mentioned in the title."
  (should (equal
           "Search-url Fetch (https://google.com)"
           (agent-shell--permission-title
            :tool-call
            `((:title . "Search-url Fetch")
              (:kind . "other")
              (:locations . [((path . "https://google.com"))]))))))

(ert-deftest agent-shell--permission-title-locations-skipped-when-in-title-test ()
  "Skip `locations' paths already present in the title."
  (should (equal
           "`echo hi`"
           (agent-shell--permission-title
            :tool-call
            `((:title . "`echo hi`")
              (:kind . "execute")
              (:locations . [((path . "echo hi"))]))))))

(ert-deftest agent-shell--permission-title-locations-skipped-when-in-content-test ()
  "Skip `locations' paths already embedded inside `content' text.
AIDE's url_fetch request sends the URL in both `content' (inside
a JSON code block) and `locations'; only one copy should render."
  (should (equal
           "Search-url Fetch\n\nCall url_fetch with {\"url\": \"https://google.com\"}"
           (agent-shell--permission-title
            :tool-call
            `((:title . "Search-url Fetch")
              (:kind . "other")
              (:content . [((type . "content")
                            (content (type . "text")
                                     (text . "Call url_fetch with {\"url\": \"https://google.com\"}")))])
              (:locations . [((path . "https://google.com"))]))))))

(ert-deftest agent-shell--permission-title-locations-basename-skipped-test ()
  "Skip `locations' paths whose basename was already shown via `rawInput'.
Some agents populate both `rawInput.filepath' (which we render as
basename) and `locations' (which has the absolute path); only one
copy should render."
  (should (equal
           "edit (foo.rs)"
           (agent-shell--permission-title
            :tool-call
            `((:title . "edit")
              (:kind . "edit")
              (:raw-input . ((filepath . "/home/user/foo.rs")))
              (:locations . [((path . "/home/user/foo.rs"))]))))))

(ert-deftest agent-shell--permission-title-locations-skipped-after-fenced-command-test ()
  "Skip `locations' paths when the title ends in a fenced command.
OpenCode sends the command in `rawInput' and the working directory in
`locations'.  Appending to the closing fence line would leave the block
unterminated and render the fences verbatim.  See
https://github.com/xenodium/agent-shell/issues/767."
  (should (equal
           "```console\ngh issue list --limit 1\n```"
           (agent-shell--permission-title
            :tool-call
            `((:title . "gh issue list --limit 1")
              (:kind . "execute")
              (:raw-input . ((command . "gh issue list --limit 1")))
              (:locations . [((path . "/home/user/.config/emacs"))]))))))

(ert-deftest agent-shell--permission-title-locations-skipped-after-fenced-raw-input-test ()
  "Skip `locations' paths when the title ends in fenced `rawInput'."
  (should (equal
           "emacs_eval-elisp\n\n```\n(+ 1 2 3)\n```"
           (agent-shell--permission-title
            :tool-call
            `((:title . "emacs_eval-elisp")
              (:kind . "other")
              (:raw-input . ((expression . "(+ 1 2 3)")))
              (:locations . [((path . "/home/user/project"))]))))))

(ert-deftest agent-shell--permission-title-other-kind-single-stringy-raw-input-test ()
  "Surface single stringy `rawInput' value for `other'-kind tools.
OpenCode sends `emacs_eval-elisp' permission requests with the
expression in `rawInput' and no structured `content'."
  (should (equal
           "emacs_eval-elisp\n\n```\n(+ 1 2 3)\n```"
           (agent-shell--permission-title
            :tool-call
            '((:title . "emacs_eval-elisp")
              (:kind . "other")
              (:raw-input . ((expression . "(+ 1 2 3)"))))))))

(ert-deftest agent-shell--permission-title-other-kind-raw-input-dedup-against-content-test ()
  "Skip `rawInput' fence when its value already appears in `content'."
  (should (equal
           "emacs_eval-elisp\n\n(+ 1 2 3)"
           (agent-shell--permission-title
            :tool-call
            `((:title . "emacs_eval-elisp")
              (:kind . "other")
              (:raw-input . ((expression . "(+ 1 2 3)")))
              (:content . [((type . "content")
                            (content (type . "text")
                                     (text . "(+ 1 2 3)")))]))))))

(ert-deftest agent-shell--permission-title-empty-content-and-locations-test ()
  "Empty `content' / `locations' vectors should not affect the title.
Gemini sends these fields as empty arrays."
  (should (equal
           "git log --reverse | head -n 1"
           (agent-shell--permission-title
            :tool-call
            `((:title . "git log --reverse | head -n 1")
              (:kind . "execute")
              (:content . [])
              (:locations . []))))))

(ert-deftest agent-shell-restart-preserves-default-directory ()
  "Restart should use the shell's directory, not the fallback buffer's.

After `kill-buffer' happens during restart, Emacs falls back to another
buffer.  Without the fix, `default-directory' would be inherited from
that fallback buffer, potentially starting the new shell in the wrong project."
  ;; Requires `make-frame', which signals \"Unknown terminal type\" in
  ;; `--batch'.  Skip when no display is available so CI doesn't fail
  ;; on environments that can't host a frame.
  (skip-unless (display-graphic-p))
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
                      ((symbol-function 'shell-maker-set-buffer-name)
                       #'ignore)
                      ((symbol-function 'agent-shell--display-buffer)
                       #'ignore)
                      ((symbol-function 'agent-shell-viewport--show-buffer)
                       #'ignore))
              (agent-shell-restart)))
          (should (equal captured-dir project-a)))
      (when (and frame (frame-live-p frame))
        (delete-frame frame))
      (when (and shell-buffer (buffer-live-p shell-buffer))
        (kill-buffer shell-buffer))
      (when (and other-buffer (buffer-live-p other-buffer))
        (kill-buffer other-buffer))
      (when-let* ((buf (get-buffer "*test-restart-new-shell*")))
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

(ert-deftest agent-shell--display-attached-files-keeps-point-at-end-test ()
  "Attaching files leaves point at the end when it was already there.

The fragment renders while the submit command is still running, so
auto-scroll can read a stale answer for whether the buffer end is on
screen and leave point at the start of what it just inserted.  Point
elsewhere is left alone, so a user reading further up is not dragged
down."
  (let ((shell-buf (generate-new-buffer " *test-shell*")))
    (unwind-protect
        (with-current-buffer shell-buf
          ;; Enough of a shell for the fragment path: `comint-mode' for the
          ;; markers `shell-maker's' auto-scroll sets, and the mode symbol
          ;; the fragment writer checks.
          (comint-mode)
          (setq major-mode 'agent-shell-mode)
          (setq-local agent-shell--state
                      (agent-shell--make-state :buffer shell-buf))
          ;; Displayed, and taller than the window: that is when
          ;; `pos-visible-in-window-p' reports the end off screen and
          ;; auto-scroll leaves point behind.  Undisplayed, the check is
          ;; vacuous and this passes either way.
          (set-window-buffer (selected-window) shell-buf)
          (insert (make-string 200 ?\n))
          (goto-char (point-max))
          (agent-shell--display-attached-files (list "/tmp/one.el"))
          (should (eobp))
          ;; Reading further up: point stays put.
          (goto-char (point-min))
          (agent-shell--display-attached-files (list "/tmp/two.el"))
          (should (equal (point) (point-min))))
      (kill-buffer shell-buf))))

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

(ert-deftest agent-shell-filter-buffer-substring-strips-hidden-markup ()
  "Copying text should exclude markdown syntax hidden by overlays."
  (with-temp-buffer
    (insert "```emacs-lisp\n(defun foo (x)\n  x)\n```\n")
    (markdown-overlays-put)
    (let ((result (agent-shell--filter-buffer-substring (point-min) (point-max))))
      (should (equal result "(defun foo (x)\n  x)\n\n")))))

(ert-deftest agent-shell-filter-buffer-substring-strips-inline-code-backticks ()
  "Copying inline code should exclude the surrounding backticks."
  (with-temp-buffer
    (insert "Use `foo-bar` for that.")
    (markdown-overlays-put)
    (let ((result (agent-shell--filter-buffer-substring (point-min) (point-max))))
      (should (equal result "Use foo-bar for that.")))))

(ert-deftest agent-shell-filter-buffer-substring-handles-reversed-range ()
  "A reversed range yields the same text as the forward one.
START may be greater than END (e.g. a right-to-left mouse selection, or
a kill where mark > point).  Like the stock `buffer-substring', the
result must match the forward range rather than the empty string --
otherwise mouse copy silently yields nothing depending on selection
direction."
  (with-temp-buffer
    (insert "hello world")
    (let ((forward  (agent-shell--filter-buffer-substring (point-min) (point-max)))
          (reversed (agent-shell--filter-buffer-substring (point-max) (point-min))))
      (should (equal forward "hello world"))
      (should (equal reversed forward)))))

(ert-deftest agent-shell-trim-strips-untagged-whitespace ()
  ;; Plain `string-trim'-style behavior when nothing is tagged: outer
  ;; whitespace is removed.
  (should (equal "hello"
                 (agent-shell-trim "\n\n  hello  \n\n"))))

(ert-deftest agent-shell-trim-preserves-tagged-whitespace ()
  ;; A trailing `\\n' tagged with `agent-shell-non-trimmable'
  ;; survives the trim — the renderer's panel padding (top/bottom
  ;; vpad `\\n's around a source block) relies on this so the panel
  ;; doesn't get clipped on the first / last block of a response.
  (let* ((tail (propertize "\n" 'agent-shell-non-trimmable t))
         (s (concat "\n\nhello\n" tail "\n\n")))
    (should (equal "hello\n\n"
                   (substring-no-properties
                    (agent-shell-trim s))))))

(ert-deftest agent-shell-trim-handles-edge-cases ()
  ;; nil input, empty string, and all-whitespace strings.
  (should (null (agent-shell-trim nil)))
  (should (equal "" (agent-shell-trim "")))
  (should (equal "" (agent-shell-trim "\n\n  \t  \n\n"))))

(defun agent-shell-tests--make-session-update (kind text)
  "Build a fake `session/update' notification of KIND with TEXT.
KIND is a sessionUpdate string such as \"user_message_chunk\"."
  `((method . "session/update")
    (params . ((update . ((sessionUpdate . ,kind)
                          (content . ((type . "text")
                                      (text . ,text))))))))
)

(defun agent-shell-tests--pending-restore-prompt-turns (state)
  "Return STATE's pending-restore prompt turns in chronological order.

Each turn is a list of buffered notifications, oldest first.
Empty turns are filtered out."
  (seq-filter #'identity
              (nreverse
               (mapcar #'nreverse
                       (map-elt (map-elt state :pending-restore) :prompt-turns)))))

(ert-deftest agent-shell--pending-restore-groups-notifications-by-prompt-turn ()
  "Test buffered notifications split into per-turn lists.

A new turn begins when a `user_message_chunk' arrives after
agent activity; consecutive user chunks stay in the same turn."
  (let ((state (list (cons :pending-restore (agent-shell--make-pending-restore)))))
    (dolist (notif (list
                    (agent-shell-tests--make-session-update "user_message_chunk" "Hello ")
                    (agent-shell-tests--make-session-update "user_message_chunk" "world")
                    (agent-shell-tests--make-session-update "agent_message_chunk" "Hi ")
                    (agent-shell-tests--make-session-update "agent_message_chunk" "there")
                    (agent-shell-tests--make-session-update "user_message_chunk" "second prompt")
                    (agent-shell-tests--make-session-update "agent_message_chunk" "intermediate")
                    (agent-shell-tests--make-session-update "tool_call" "ignored")
                    (agent-shell-tests--make-session-update "user_message_chunk" "third prompt")
                    (agent-shell-tests--make-session-update "agent_message_chunk" "final answer")))
      (agent-shell--append-restore-notification state notif))
    (let ((prompt-turns (agent-shell-tests--pending-restore-prompt-turns state)))
      (should (= 3 (length prompt-turns)))
      (should (equal (mapcar (lambda (notif)
                               (map-nested-elt notif '(params update sessionUpdate)))
                             (nth 0 prompt-turns))
                     '("user_message_chunk" "user_message_chunk"
                       "agent_message_chunk" "agent_message_chunk")))
      (should (equal (mapcar (lambda (notif)
                               (map-nested-elt notif '(params update sessionUpdate)))
                             (nth 1 prompt-turns))
                     '("user_message_chunk" "agent_message_chunk" "tool_call")))
      (should (equal (mapcar (lambda (notif)
                               (map-nested-elt notif '(params update sessionUpdate)))
                             (nth 2 prompt-turns))
                     '("user_message_chunk" "agent_message_chunk"))))))

(ert-deftest agent-shell--pending-restore-keeps-single-prompt-turn-together ()
  "Test consecutive `user_message_chunk's stay in the same prompt turn."
  (let ((state (list (cons :pending-restore (agent-shell--make-pending-restore)))))
    (dolist (notif (list
                    (agent-shell-tests--make-session-update "user_message_chunk" "Hello ")
                    (agent-shell-tests--make-session-update "user_message_chunk" "world")))
      (agent-shell--append-restore-notification state notif))
    (should (= 1 (length (agent-shell-tests--pending-restore-prompt-turns state))))))

(cl-defun agent-shell-tests--render-pending-restore (&key last-entry-type typed-input undo kill-input)
  "Restore a single replayed turn ending in LAST-ENTRY-TYPE.

Drives `agent-shell--render-pending-restore' as a `session/load' whose
buffered history holds one turn: `agent-shell--replay-turn' is stubbed
to insert that turn's text and leave `:last-entry-type' at
LAST-ENTRY-TYPE, mimicking what the real notification dispatch leaves
behind.

TYPED-INPUT is type-ahead entered at the early prompt before the load
completes.  UNDO undoes once after the replay settles.  KILL-INPUT
runs `comint-kill-input' once it settles, which clears whatever comint
considers unsent input and leaves the rest of the buffer alone.

Returns the resulting buffer string, with the live prompt trailing."
  (let* ((buffer (generate-new-buffer " *agent-shell-restore-test*"))
         (fake-process (start-process "fake-agent" buffer "cat")))
    (set-process-query-on-exit-flag fake-process nil)
    (unwind-protect
        (with-current-buffer buffer
          (comint-mode)
          (setq-local comint-prompt-regexp "^Claude> ")
          (buffer-enable-undo)
          (let ((state (list (cons :buffer (current-buffer))
                             (cons :active-requests nil)
                             (cons :last-entry-type nil)
                             (cons :pending-restore
                                   (agent-shell--make-pending-restore)))))
            (agent-shell--append-restore-notification
             state (agent-shell-tests--make-session-update "user_message_chunk" "Hello"))
            (cl-letf (((symbol-function 'shell-maker--process) (lambda () fake-process))
                      ((symbol-function 'agent-shell--create-bootstrapping-placeholders)
                       #'ignore)
                      ;; Emitting reads shell state, which this bare
                      ;; comint buffer has no business carrying.
                      ((symbol-function 'agent-shell--emit-event) #'ignore)
                      ((symbol-function 'agent-shell--effective-restore-verbosity)
                       (lambda (_state) 'last))
                      ((symbol-function 'agent-shell--replay-turn)
                       (lambda (state _turn)
                         (let ((inhibit-read-only t))
                           (goto-char (point-max))
                           (insert "Claude> replayed"))
                         (map-put! state :last-entry-type last-entry-type))))
              ;; A live prompt awaiting input, emitted the way shell-maker
              ;; emits it before the load completes, so `comint-last-prompt'
              ;; and the process mark start out where a real shell leaves
              ;; them: replayed history lands above the prompt and
              ;; type-ahead after it, with the mark in between.
              (shell-maker--output-filter fake-process "Claude> ")
              (goto-char (point-max))
              (when typed-input
                (insert typed-input)
                (undo-boundary))
              (agent-shell--render-pending-restore state)
              (when undo
                ;; Stands in for the command loop, which boundaries the
                ;; undo list before running the undo command.
                (undo-boundary)
                (undo))
              (when kill-input
                (comint-kill-input)))
            (buffer-substring-no-properties (point-min) (point-max))))
      (when (process-live-p fake-process)
        (delete-process fake-process))
      (kill-buffer buffer))))

(ert-deftest agent-shell--render-pending-restore-closes-trailing-user-prompt-test ()
  "Test a replay ending on a user prompt is closed above the live prompt.
Regression: restoring a session whose last turn is a user message (an
interrupted request, whose final entry is the interruption notice) left
the turn open.  The live prompt then rendered on the same line as the
restored user text, and the next unrelated notification emitted the
end-of-prompt marker unnarrowed, landing it after the live prompt."
  (should (equal (agent-shell-tests--render-pending-restore
                  :last-entry-type "user_message_chunk")
                 (concat "Claude> replayed<shell-maker-end-of-prompt>\n\n"
                         "Claude> ")))
  ;; A replay ending on agent output was already closed while replaying,
  ;; so nothing is appended.
  (should (equal (agent-shell-tests--render-pending-restore
                  :last-entry-type "agent_message_chunk")
                 "Claude> replayedClaude> ")))

(ert-deftest agent-shell--render-pending-restore-undoes-type-ahead-only-test ()
  "Test undo after a restore removes type-ahead and nothing else.
Regression: replay lands above the early prompt, pushing type-ahead
down without adjusting the undo entries recorded for it (Emacs doesn't
adjust the absolute positions they hold).  Undo deleted a stretch of
restored history instead of what was typed."
  (should (equal (agent-shell-tests--render-pending-restore
                  :last-entry-type "agent_message_chunk"
                  :typed-input "hi there")
                 "Claude> replayedClaude> hi there"))
  (should (equal (agent-shell-tests--render-pending-restore
                  :last-entry-type "agent_message_chunk"
                  :typed-input "hi there"
                  :undo t)
                 "Claude> replayedClaude> ")))

(ert-deftest agent-shell--render-pending-restore-keeps-type-ahead-editable-test ()
  "Test type-ahead is still comint's input after a restore.
A replay ending on a user turn closes it with shell-maker's
end-of-prompt marker, emitted under the narrowing that ends before the
live prompt.  The process mark must come back to the prompt's end: at
the prompt's start the `PROMPT> ' text joins the next message, and past
the type-ahead comint reads what was typed as output, so
`comint-kill-input' deletes nothing and submitting sends an empty
message."
  (should (equal (agent-shell-tests--render-pending-restore
                  :last-entry-type "user_message_chunk"
                  :typed-input "hi there"
                  :kill-input t)
                 (concat "Claude> replayed<shell-maker-end-of-prompt>\n\n"
                         "Claude> ")))
  ;; With an empty input area the mark still lands past the `PROMPT> '
  ;; text, so it's never captured as input.
  (should (equal (agent-shell-tests--render-pending-restore
                  :last-entry-type "user_message_chunk"
                  :kill-input t)
                 (concat "Claude> replayed<shell-maker-end-of-prompt>\n\n"
                         "Claude> ")))
  ;; A replay ending on agent output emits no marker, so nothing moves
  ;; the mark off the prompt to begin with.
  (should (equal (agent-shell-tests--render-pending-restore
                  :last-entry-type "agent_message_chunk"
                  :typed-input "hi there"
                  :kill-input t)
                 "Claude> replayedClaude> ")))

(ert-deftest agent-shell--use-session-load-p-modes ()
  "Test `agent-shell--use-session-load-p' across verbosity/protocol combinations."
  ;; `last' mode forces session/load when supported
  (let ((agent-shell-session-restore-verbosity 'last))
    (should (agent-shell--use-session-load-p
             '((:supports-session-load . t)
               (:supports-session-resume . t))))
    ;; `last' falls back to resume when load unsupported
    (should-not (agent-shell--use-session-load-p
                 '((:supports-session-load . nil)
                   (:supports-session-resume . t)))))
  ;; `full' mode forces session/load when supported
  (let ((agent-shell-session-restore-verbosity 'full))
    (should (agent-shell--use-session-load-p
             '((:supports-session-load . t)
               (:supports-session-resume . t)))))
  ;; `minimal' mode prefers resume when available
  (let ((agent-shell-session-restore-verbosity 'minimal))
    (should-not (agent-shell--use-session-load-p
                 '((:supports-session-load . t)
                   (:supports-session-resume . t))))
    ;; `minimal' falls back to load when resume unavailable
    (should (agent-shell--use-session-load-p
             '((:supports-session-load . t)
               (:supports-session-resume . nil))))))

(ert-deftest agent-shell--initiate-session-summary-mode-uses-session-load ()
  "Test that `last' verbosity bypasses `session/resume' in favor of `session/load'."
  (with-temp-buffer
    (let* ((agent-shell-session-strategy 'latest)
           (agent-shell-session-restore-verbosity 'last)
           (requests '())
           (session-init-called nil)
           (state (list (cons :buffer (current-buffer))
                        (cons :client 'test-client)
                        (cons :session (list (cons :id nil)
                                             (cons :mode-id nil)
                                             (cons :modes nil)))
                        (cons :supports-session-list t)
                        (cons :supports-session-load t)
                        (cons :supports-session-resume t)
                        (cons :pending-restore nil)
                        (cons :active-requests nil)
                        (cons :event-subscriptions nil))))
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
                                 '((sessions . [((sessionId . "session-abc")
                                                 (cwd . "/tmp")
                                                 (title . "Some session"))]))))
                       ("session/load"
                        (funcall (plist-get args :on-success) '()))
                       (_ (error "Unexpected method: %s" method)))))))
        (agent-shell--initiate-session
         :shell-buffer (current-buffer)
         :on-session-init (lambda ()
                            (setq session-init-called t)))
        (should (equal (mapcar (lambda (req)
                                 (map-elt (plist-get req :request) :method))
                               (nreverse requests))
                       '("session/list" "session/load")))
        (should session-init-called)
        (should-not (map-elt agent-shell--state :pending-restore))))))

(ert-deftest agent-shell-viewport-next-page-navigates-from-current-prompt-begin-test ()
  "Test `agent-shell-viewport-next-page' navigates from the current prompt.

When the shell point sits mid-interaction (e.g. after switching to the
viewport without repositioning), navigation must start from the current
interaction's prompt begin, otherwise a backward step lands on the
current interaction instead of the previous one."
  (let ((shell-buffer (generate-new-buffer " *agent-shell shell*"))
        (viewport-buffer (generate-new-buffer " *agent-shell shell* [viewport]"))
        (navigated-from nil)
        (prompt-begin nil))
    (unwind-protect
        (progn
          (with-current-buffer shell-buffer
            (insert "line one\nprompt two line\nresponse line three\nmore content")
            (goto-char (point-min))
            (forward-line 1)
            (setq prompt-begin (point))
            ;; Point sits mid/after the interaction, not at the prompt begin.
            (goto-char (point-max)))
          (with-current-buffer viewport-buffer
            (cl-letf (((symbol-function 'agent-shell-viewport--update-header)
                       (lambda () nil)))
              (agent-shell-viewport-view-mode)))
          (with-current-buffer viewport-buffer
            (cl-letf (((symbol-function 'agent-shell-viewport--busy-p)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-shell-viewport--shell-buffer)
                       (lambda (&rest _) shell-buffer))
                      ((symbol-function 'agent-shell-viewport--position)
                       (lambda (&rest _) '((:current . 2) (:total . 2))))
                      ((symbol-function 'shell-maker--prompt-begin-position)
                       (lambda () prompt-begin))
                      ((symbol-function 'comint-previous-prompt)
                       (lambda (&rest _) (forward-line -1)))
                      ((symbol-function 'shell-maker-next-command-and-response)
                       (lambda (_backwards &rest _)
                         (setq navigated-from (point))
                         '("prompt two" . "response")))
                      ((symbol-function 'agent-shell-viewport--initialize)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-shell-viewport--update-header)
                       (lambda () nil)))
              (agent-shell-viewport-next-page :backwards t)
              (should (equal navigated-from prompt-begin)))))
      (kill-buffer viewport-buffer)
      (kill-buffer shell-buffer))))

(ert-deftest agent-shell-viewport-initialize-rerenders-header-position-test ()
  "Test `agent-shell-viewport--initialize' re-renders the header position.

A stale cached position must not survive a content refresh, otherwise
the header shows a position that doesn't match the displayed
interaction (e.g. \"1/2\" after switching to the latest interaction)."
  (let ((viewport-buffer (generate-new-buffer " *agent-shell shell* [viewport]"))
        (shell-buffer (generate-new-buffer " *agent-shell shell*"))
        (rendered-position nil))
    (unwind-protect
        (with-current-buffer viewport-buffer
          (cl-letf (((symbol-function 'agent-shell-viewport--update-header)
                     (lambda () nil)))
            (agent-shell-viewport-view-mode))
          ;; Seed a stale cached position.
          (setq agent-shell-viewport--position-cache '((:current . 1) (:total . 2)))
          (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                     (lambda (&rest _) shell-buffer))
                    ((symbol-function 'shell-maker-history-position)
                     (lambda () '((:current . 2) (:total . 2))))
                    ((symbol-function 'markdown-overlays-put)
                     (lambda (&rest _) nil))
                    ((symbol-function 'agent-shell-viewport--update-header)
                     (lambda ()
                       (setq rendered-position (agent-shell-viewport--position)))))
            (agent-shell-viewport--initialize :prompt "p" :response "r")
            (should (equal rendered-position '((:current . 2) (:total . 2))))))
      (kill-buffer viewport-buffer)
      (kill-buffer shell-buffer))))

(ert-deftest agent-shell--refresh-session-title-skips-when-list-unsupported ()
  "Test `agent-shell--refresh-session-title' sends no request without `list'.

Agents that don't advertise the `list' session capability (e.g. Cline)
would otherwise get a `session/list' request on every turn, failing
with \"Method not found\"."
  (with-temp-buffer
    (let ((request-sent nil)
          (state (list (cons :client 'test-client)
                       (cons :supports-session-list nil)
                       (cons :session (list (cons :id "session-123"))))))
      (setq-local agent-shell--state state)
      (cl-letf (((symbol-function 'acp-send-request)
                 (lambda (&rest _args)
                   (setq request-sent t))))
        (agent-shell--refresh-session-title)
        (should-not request-sent)))))

(ert-deftest agent-shell--refresh-session-title-fetches-when-list-supported ()
  "Test `agent-shell--refresh-session-title' sends `session/list' when supported."
  (with-temp-buffer
    (let ((sent-method nil)
          (state (list (cons :client 'test-client)
                       (cons :supports-session-list t)
                       (cons :session (list (cons :id "session-123"))))))
      (setq-local agent-shell--state state)
      (cl-letf (((symbol-function 'agent-shell--resolve-path)
                 (lambda (path) path))
                ((symbol-function 'acp-send-request)
                 (lambda (&rest args)
                   (setq sent-method (map-elt (plist-get args :request) :method)))))
        (agent-shell--refresh-session-title)
        (should (equal sent-method "session/list"))))))

;;; Tests for agent-shell--activity-group-id

(ert-deftest agent-shell--activity-group-id-groups-consecutive-runs-test ()
  "Consecutive tool calls share a group; an interruption starts a new one.
A tool's group is assigned once and reused, so a later update keeps it even
if a message streamed in between."
  (let ((state (list (cons :last-entry-type nil)
                     (cons :activity-group-count 0)
                     (cons :tool-calls nil))))
    ;; First tool call after a non-tool entry -> group 1.
    (should (equal "activity-1" (agent-shell--activity-group-id state "a")))
    (map-put! state :last-entry-type "tool_call")
    ;; A second consecutive tool call joins the same group.
    (should (equal "activity-1" (agent-shell--activity-group-id state "b")))
    (map-put! state :last-entry-type "tool_call_update")
    ;; A message interrupts the run...
    (map-put! state :last-entry-type "agent_message_chunk")
    ;; ...but an update to an existing tool reuses its stored group.
    (should (equal "activity-1" (agent-shell--activity-group-id state "a")))
    ;; A brand-new tool after the message starts a fresh group.
    (should (equal "activity-2" (agent-shell--activity-group-id state "c")))))

(cl-defun agent-shell-tests--message-chunk-fragments (&key chunks)
  "Drive `agent-shell--on-notification' with CHUNKS and return the fragments.

CHUNKS is a list of (MESSAGE-ID . TEXT) pairs (MESSAGE-ID may be nil).
Between successive chunks `:last-entry-type' is left as-is, reproducing an
interleaved entry that failed to advance it.  Returns a list of
\(BLOCK-ID . CREATE-NEW) as passed to `agent-shell--update-fragment'."
  (let ((state (list (cons :last-entry-type nil)
                     (cons :last-agent-message-id nil)
                     (cons :chunked-group-count 0)
                     (cons :active-requests t)
                     (cons :pending-restore nil)
                     (cons :last-activity-time nil)
                     (cons :buffer nil)))
        (calls '()))
    (cl-letf (((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args)
                 (push (cons (plist-get args :block-id) (plist-get args :create-new)) calls)))
              ((symbol-function 'agent-shell--append-transcript) #'ignore)
              ((symbol-function 'agent-shell--emit-event) #'ignore)
              ((symbol-function 'agent-shell--active-requests-p) (lambda (&rest _) t))
              ((symbol-function 'agent-shell--content-block-to-markdown)
               (lambda (block) (map-elt block 'text)))
              ((symbol-function 'agent-shell--indent-markdown-headers) #'identity))
      (dolist (chunk chunks)
        (agent-shell--on-notification
         :state state
         :acp-notification
         `((method . "session/update")
           (params (update (sessionUpdate . "agent_message_chunk")
                           (messageId . ,(car chunk))
                           (content (type . "text") (text . ,(cdr chunk)))))))))
    (nreverse calls)))

(ert-deftest agent-shell--message-chunk-distinct-message-ids-dont-coalesce-test ()
  "Distinct message ids form separate fragments.
They do so even when an interleaved entry left `:last-entry-type'
unadvanced.  Regression for glued messages like \"Emacs:The GUI Emacs\":
two turns whose text merged into one block."
  (let ((calls (agent-shell-tests--message-chunk-fragments
                :chunks '(("msg_A" . "user's Emacs:")
                          ("msg_B" . "The GUI Emacs")))))
    ;; Different messageIds -> different block-ids, each a new fragment.
    (should (equal "msg_A-agent_message_chunk" (car (nth 0 calls))))
    (should (equal "msg_B-agent_message_chunk" (car (nth 1 calls))))
    (should (cdr (nth 1 calls)))))

(ert-deftest agent-shell--message-chunk-same-message-id-appends-test ()
  "Chunks sharing a `messageId' stream into one fragment (append, not new)."
  (let ((calls (agent-shell-tests--message-chunk-fragments
                :chunks '(("msg_A" . "Hello ") ("msg_A" . "world")))))
    (should (equal "msg_A-agent_message_chunk" (car (nth 0 calls))))
    (should (equal "msg_A-agent_message_chunk" (car (nth 1 calls))))
    ;; First chunk creates the fragment; the second appends to it.
    (should (cdr (nth 0 calls)))
    (should-not (cdr (nth 1 calls)))))

(ert-deftest agent-shell--message-chunk-without-message-id-limitation-test ()
  "Document the `messageId' requirement for `agent_message_chunk' updates.

`messageId' is the only reliable message boundary.  When it is absent
\(older agents omit it) the handler falls back to `:last-entry-type': a
new fragment starts only when the previous rendered entry was not itself
a message chunk.  So two `agent_message_chunk' updates that are NOT
separated by a run-advancing entry -- e.g. only a `usage_update', which
does not touch `:last-entry-type', sits between them -- coalesce into one
fragment even if the agent meant them as distinct messages.  Agents that
emit `messageId' avoid this; those that omit it remain exposed."
  (let ((calls (agent-shell-tests--message-chunk-fragments
                ;; Two chunks the agent could mean as distinct messages;
                ;; with no `messageId' and nothing advancing the run
                ;; between them, they cannot be told apart.
                :chunks '((nil . "Hello ") (nil . "world")))))
    ;; Same block-id, second appends -> one merged fragment (the limitation).
    (should (equal "1-agent_message_chunk" (car (nth 0 calls))))
    (should (equal "1-agent_message_chunk" (car (nth 1 calls))))
    (should (cdr (nth 0 calls)))
    (should-not (cdr (nth 1 calls)))))

(ert-deftest agent-shell--activity-group-current-id-shares-run-with-thoughts-test ()
  "Thoughts and tool calls share one activity run; a message breaks it.
A thought between tool calls keeps the group open (it is part of the same
agent activity), while an `agent_message_chunk' starts a fresh group."
  (let ((state (list (cons :last-entry-type nil)
                     (cons :activity-group-count 0))))
    ;; First entry of a run -> group 1.
    (should (equal "activity-1" (agent-shell--activity-group-current-id state)))
    ;; A thought after a tool call stays in the same group.
    (map-put! state :last-entry-type "tool_call")
    (should (equal "activity-1" (agent-shell--activity-group-current-id state)))
    ;; Streamed thought chunks keep the same group.
    (map-put! state :last-entry-type "agent_thought_chunk")
    (should (equal "activity-1" (agent-shell--activity-group-current-id state)))
    ;; A tool call after a thought still shares the group.
    (should (equal "activity-1" (agent-shell--activity-group-current-id state)))
    ;; A message breaks the run -> fresh group.
    (map-put! state :last-entry-type "agent_message_chunk")
    (should (equal "activity-2" (agent-shell--activity-group-current-id state)))))

(ert-deftest agent-shell--activity-group-initial-expanded-test ()
  "The group expanded initial predicate."
  (dolist (case '((nil . nil)
                  (t . t)
                  ;; `latest' groups are born expanded and folded once done.
                  (latest . t)))
    (let ((agent-shell-activity-group-expand-by-default (car case)))
      (should (eq (cdr case)
                  (agent-shell--activity-group-initial-expanded-p))))))

(ert-deftest agent-shell--sync-activity-group-fold-test ()
  "`latest' keeps only the agent's current activity group expanded."
  (let ((collapsed '())
        (state (list (cons :activity-group-count 1)
                     (cons :request-count 3)
                     (cons :expanded-activity-group nil))))
    (cl-letf (((symbol-function 'agent-shell--collapse-fragment-group)
               (lambda (&rest args)
                 (push (cons (plist-get args :namespace-id)
                             (plist-get args :block-id))
                       collapsed))))
      ;; Other policies never fold anything.
      (dolist (policy '(never always))
        (let ((agent-shell-activity-group-expand-by-default policy))
          (agent-shell--sync-activity-group-fold :state state :group-id "activity-1")
          (should-not collapsed)
          (should-not (map-elt state :expanded-activity-group))))
      (let ((agent-shell-activity-group-expand-by-default 'latest))
        ;; The current run is recorded, with nothing to fold yet.
        (agent-shell--sync-activity-group-fold :state state :group-id "activity-1")
        (should-not collapsed)
        (should (equal '((:namespace-id . 3) (:group-id . "activity-1"))
                       (map-elt state :expanded-activity-group)))
        ;; Further members of the same run leave it expanded.
        (agent-shell--sync-activity-group-fold :state state :group-id "activity-1")
        (should-not collapsed)
        ;; The agent moves on: the previous run folds away.
        (map-put! state :activity-group-count 2)
        (agent-shell--sync-activity-group-fold :state state :group-id "activity-2")
        (should (equal '((3 . "activity-1")) collapsed))
        (should (equal "activity-2"
                       (map-nested-elt state '(:expanded-activity-group :group-id))))
        ;; A late update to the earlier run neither re-expands it nor folds
        ;; the run the agent is currently in.
        (setq collapsed '())
        (agent-shell--sync-activity-group-fold :state state :group-id "activity-1")
        (should-not collapsed)
        (should (equal "activity-2"
                       (map-nested-elt state '(:expanded-activity-group :group-id))))
        ;; Turn end folds the last expanded run and forgets it.
        (agent-shell--collapse-expanded-activity-group state)
        (should (equal '((3 . "activity-2")) collapsed))
        (should-not (map-elt state :expanded-activity-group))
        (setq collapsed '())
        (agent-shell--collapse-expanded-activity-group state)
        (should-not collapsed)))))

(ert-deftest agent-shell--activity-grouping-latest-folds-previous-group-test ()
  "Driving notifications under `latest' folds each group as it is left.
Groups are created expanded, and a group folds as soon as the agent starts
answering rather than waiting for the next run to begin, so only the run
in flight shows its members."
  (let ((collapsed '())
        (expanded '())
        (agent-shell-activity-group-expand-by-default 'latest)
        (state (list (cons :tool-calls nil)
                     (cons :last-entry-type nil)
                     (cons :last-agent-message-id nil)
                     (cons :activity-group-count 0)
                     (cons :chunked-group-count 0)
                     (cons :request-count 1)
                     (cons :expanded-activity-group nil)
                     (cons :active-requests t)
                     (cons :last-activity-time nil)
                     (cons :buffer nil))))
    (cl-letf (((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args)
                 (when-let* ((group-id (plist-get args :group-id)))
                   (push (cons group-id (plist-get args :group-expanded)) expanded))))
              ((symbol-function 'agent-shell--collapse-fragment-group)
               (lambda (&rest args)
                 (push (plist-get args :block-id) collapsed)))
              ((symbol-function 'agent-shell--refresh-activity-group-header) #'ignore)
              ((symbol-function 'agent-shell--append-transcript) #'ignore)
              ((symbol-function 'agent-shell--make-transcript-tool-call-entry)
               (lambda (&rest _) ""))
              ((symbol-function 'agent-shell--delete-fragment) #'ignore)
              ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
              ((symbol-function 'agent-shell--emit-event) #'ignore)
              ((symbol-function 'agent-shell-make-tool-call-label)
               (lambda (&rest _) '((:status . "s") (:title . "t")))))
      (cl-flet ((notify (update)
                  (agent-shell--on-notification
                   :state state
                   :acp-notification `((method . "session/update")
                                       (params (update . ,update))))))
        (notify '((sessionUpdate . "tool_call") (toolCallId . "A")
                  (title . "A") (kind . "other") (status . "pending")))
        (should-not collapsed)
        ;; The agent starts answering: group 1 folds right away, without
        ;; waiting for the next group to open.
        (notify '((sessionUpdate . "agent_message_chunk")
                  (content (type . "text") (text . "msg"))))
        (should (equal '("activity-1") collapsed))
        ;; Further chunks of the same response have nothing left to fold.
        (notify '((sessionUpdate . "agent_message_chunk")
                  (content (type . "text") (text . " more"))))
        (should (equal '("activity-1") collapsed))
        ;; B lands in a fresh group, expanded, leaving group 1 folded.
        (notify '((sessionUpdate . "tool_call") (toolCallId . "B")
                  (title . "B") (kind . "other") (status . "pending")))
        (should (equal '("activity-1") collapsed)))
      ;; Both groups were created expanded.
      (should (equal '(("activity-1" . t) ("activity-2" . t)) (nreverse expanded)))
      ;; Turn end folds the group still in flight.
      (agent-shell--collapse-expanded-activity-group state)
      (should (equal '("activity-2" "activity-1") collapsed)))))

(ert-deftest agent-shell--activity-grouping-late-update-starts-new-group-test ()
  "A message between a tool call and the next starts a fresh group.
Regression for xenodium/agent-shell-js#31: a late in-place completion
update for an earlier tool must not clobber the run boundary an
interleaving message created, or the next tool call joins the earlier
group and renders above the message.  Drives the full notification
dispatch, since the defect is in the `tool_call_update' handler, not the
group-id helper alone."
  (let ((state (agent-shell--make-state)))
    (map-put! state :active-requests t)
    (cl-letf (((symbol-function 'agent-shell--update-fragment) #'ignore)
              ((symbol-function 'agent-shell--refresh-activity-group-header) #'ignore)
              ((symbol-function 'agent-shell--append-transcript) #'ignore)
              ((symbol-function 'agent-shell--make-transcript-tool-call-entry)
               (lambda (&rest _) ""))
              ((symbol-function 'agent-shell--delete-fragment) #'ignore)
              ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
              ((symbol-function 'agent-shell--emit-event) #'ignore)
              ((symbol-function 'agent-shell-make-tool-call-label)
               (lambda (&rest _) '((:status . "s") (:title . "t")))))
      (cl-flet ((notify (update)
                  (agent-shell--on-notification
                   :state state
                   :acp-notification `((method . "session/update")
                                       (params (update . ,update))))))
        (notify '((sessionUpdate . "tool_call") (toolCallId . "A")
                  (title . "A") (kind . "other") (status . "pending")))
        (notify '((sessionUpdate . "agent_message_chunk")
                  (content (type . "text") (text . "msg"))))
        (notify '((sessionUpdate . "tool_call_update") (toolCallId . "A")
                  (status . "completed")
                  (content . [((type . "content")
                               (content (type . "text") (text . "done")))])))
        (notify '((sessionUpdate . "tool_call") (toolCallId . "B")
                  (title . "B") (kind . "other") (status . "pending"))))
      ;; B must land in a fresh group, not A's.
      (should (equal "activity-1" (map-nested-elt state '(:tool-calls "A" :group-id))))
      (should (equal "activity-2" (map-nested-elt state '(:tool-calls "B" :group-id)))))))

(ert-deftest agent-shell--activity-grouping-consecutive-share-group-test ()
  "Consecutive tool calls (no interleaving entry) share one group.
Guards that the #31 fix does not over-split: an in-place completion update
between two tool calls keeps them together."
  ;; Built through `agent-shell--make-state' so the notification handlers
  ;; find every field they write; a hand-rolled alist missing one fails
  ;; `map-put!' with `map-not-inplace'.
  (let ((state (agent-shell--make-state)))
    (map-put! state :active-requests t)
    (cl-letf (((symbol-function 'agent-shell--update-fragment) #'ignore)
              ((symbol-function 'agent-shell--refresh-activity-group-header) #'ignore)
              ((symbol-function 'agent-shell--append-transcript) #'ignore)
              ((symbol-function 'agent-shell--make-transcript-tool-call-entry)
               (lambda (&rest _) ""))
              ((symbol-function 'agent-shell--delete-fragment) #'ignore)
              ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
              ((symbol-function 'agent-shell--emit-event) #'ignore)
              ((symbol-function 'agent-shell-make-tool-call-label)
               (lambda (&rest _) '((:status . "s") (:title . "t")))))
      (cl-flet ((notify (update)
                  (agent-shell--on-notification
                   :state state
                   :acp-notification `((method . "session/update")
                                       (params (update . ,update))))))
        (notify '((sessionUpdate . "tool_call") (toolCallId . "A")
                  (title . "A") (kind . "other") (status . "pending")))
        (notify '((sessionUpdate . "tool_call_update") (toolCallId . "A")
                  (status . "completed")
                  (content . [((type . "content")
                               (content (type . "text") (text . "done")))])))
        (notify '((sessionUpdate . "tool_call") (toolCallId . "B")
                  (title . "B") (kind . "other") (status . "pending"))))
      (should (equal "activity-1" (map-nested-elt state '(:tool-calls "A" :group-id))))
      (should (equal "activity-1" (map-nested-elt state '(:tool-calls "B" :group-id)))))))

(ert-deftest agent-shell--activity-grouping-permission-keeps-group-test ()
  "A tool call that required a permission prompt stays grouped with the next.
Regression: `session/request_permission' set `:last-entry-type', which
advanced the group counter and split the following tool call into its own
group even though the permission dialog is transient (deleted on
completion) and renders no lasting interleaved content."
  (let* ((buffer (generate-new-buffer " *permission-group-test*"))
         (state (agent-shell--make-state :buffer buffer)))
    (map-put! state :active-requests t)
    (unwind-protect
    (cl-letf (((symbol-function 'agent-shell--update-fragment) #'ignore)
              ((symbol-function 'agent-shell--refresh-activity-group-header) #'ignore)
              ((symbol-function 'agent-shell--append-transcript) #'ignore)
              ((symbol-function 'agent-shell--make-transcript-tool-call-entry)
               (lambda (&rest _) ""))
              ((symbol-function 'agent-shell--delete-fragment) #'ignore)
              ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
              ((symbol-function 'agent-shell--start-idle-timer) #'ignore)
              ((symbol-function 'agent-shell--emit-event) #'ignore)
              ((symbol-function 'agent-shell--make-permission-actions) (lambda (&rest _) nil))
              ((symbol-function 'agent-shell--make-tool-call-permission-text)
               (lambda (&rest _) ""))
              ((symbol-function 'agent-shell-make-tool-call-label)
               (lambda (&rest _) '((:status . "s") (:title . "t")))))
      (cl-flet ((notify (update)
                  (agent-shell--on-notification
                   :state state
                   :acp-notification `((method . "session/update")
                                       (params (update . ,update)))))
                (request (obj)
                  (agent-shell--on-request :state state :acp-request obj)))
        (notify '((sessionUpdate . "tool_call") (toolCallId . "A")
                  (title . "A") (kind . "other") (status . "pending")))
        ;; A requires approval; the permission request arrives mid-flight.
        (request '((method . "session/request_permission") (id . 5)
                   (params (options . [])
                           (toolCall (toolCallId . "A") (title . "A") (kind . "other")))))
        (notify '((sessionUpdate . "tool_call_update") (toolCallId . "A")
                  (status . "completed")
                  (content . [((type . "content")
                               (content (type . "text") (text . "done")))])))
        (notify '((sessionUpdate . "tool_call") (toolCallId . "B")
                  (title . "B") (kind . "other") (status . "pending"))))
      ;; A and B are consecutive with no interleaved content — one group.
      (should (equal (map-nested-elt state '(:tool-calls "A" :group-id))
                     (map-nested-elt state '(:tool-calls "B" :group-id)))))
      (kill-buffer buffer))))

(ert-deftest agent-shell--activity-group-header-label-test ()
  "Header glyph is `completed' only when every member is.
Otherwise it shows the worst status present, and the completed/total
count lets a non-completed member lift the total only."
  (cl-flet ((label (statuses)
              (substring-no-properties
               (agent-shell--activity-group-header-label statuses))))
    ;; All completed: check glyph and full count.
    (should (string-prefix-p "✓" (label '("completed" "completed"))))
    (should (string-suffix-p "Activity 5/5"
                             (label '("completed" "completed" "completed"
                                      "completed" "completed"))))
    ;; A failure dominates and drops the numerator.
    (should (string-prefix-p "✗" (label '("failed" "in_progress" "completed"))))
    (should (string-suffix-p "Activity 3/5"
                             (label '("completed" "completed" "completed"
                                      "failed" "failed"))))
    ;; In-progress dominates when nothing failed.
    (should (string-prefix-p "◔" (label '("completed" "pending" "in_progress"))))
    (should (string-suffix-p "Activity 2/5"
                             (label '("completed" "completed" "in_progress"
                                      "pending" "in_progress"))))))

(ert-deftest agent-shell--tool-call-kind-phrase-test ()
  "Verbs conjugate by tense, nouns by count, unknown kinds fall back."
  (should (equal "ran 2 commands"
                 (agent-shell--tool-call-kind-phrase :kind "execute" :count 2)))
  (should (equal "run a command"
                 (agent-shell--tool-call-kind-phrase :kind "execute" :count 1 :pending t)))
  (should (equal "read a file"
                 (agent-shell--tool-call-kind-phrase :kind "read" :count 1)))
  ;; Unknown kinds fall back to the generic phrasing.
  (should (equal "ran 3 tool calls"
                 (agent-shell--tool-call-kind-phrase :kind "mystery" :count 3))))

(ert-deftest agent-shell--activity-group-descriptive-text-test ()
  "Kinds collapse into counted phrases in first-seen order.
Only the first word is capitalized, and the phrase stays in present
tense while any member is still unfinished."
  (cl-flet ((tc (id kind status)
              (cons id (list (cons :kind kind) (cons :status status))))
            (text (members &optional thought)
              (agent-shell--activity-group-descriptive-text
               :members members :thought thought)))
    ;; Multiple kinds, first-seen order, only the first word capitalized.
    (should (equal "Ran 3 commands, read a file"
                   (text (list (tc "a" "execute" "completed")
                               (tc "b" "execute" "completed")
                               (tc "c" "execute" "completed")
                               (tc "d" "read" "completed")))))
    ;; Present tense while pending, past once completed.
    (should (equal "Run a command" (text (list (tc "a" "execute" "pending")))))
    (should (equal "Ran a command" (text (list (tc "a" "execute" "completed")))))
    ;; A single unfinished member lifts the whole kind to present tense.
    (should (equal "Run 2 commands"
                   (text (list (tc "a" "execute" "completed")
                               (tc "b" "execute" "in_progress")))))
    ;; A failed member still reads past tense (it did run).
    (should (equal "Ran a command" (text (list (tc "a" "execute" "failed")))))
    ;; nil kind falls back to the generic phrasing.
    (should (equal "Ran a tool call" (text (list (tc "a" nil "completed")))))
    ;; A thought prefixes a leading (uncounted) "Thought" phrase.
    (should (equal "Thought, ran 2 commands"
                   (text (list (tc "a" "execute" "completed")
                               (tc "b" "execute" "completed"))
                         t)))
    ;; A thought with no tool calls reads just "Thought".
    (should (equal "Thought" (text nil t)))
    ;; A `think'-kind call folds into "Thought" like a chunk, not counted.
    (should (equal "Thought" (text (list (tc "a" "think" "completed")))))
    ;; It folds even alongside counted tool calls.
    (should (equal "Thought, ran a command"
                   (text (list (tc "a" "think" "completed")
                               (tc "b" "execute" "completed")))))
    ;; A `think'-kind call and a thought chunk still read a single "Thought".
    (should (equal "Thought" (text (list (tc "a" "think" "completed")) t)))))

(ert-deftest agent-shell--activity-group-thought-labels-test ()
  "Header labels reflect thoughts recorded on a group.
A thought-only group reads \"Thinking\" (count) / \"Thought\" (descriptive);
a mixed group counts its tools and, in descriptive style, mentions the
thought."
  (cl-flet ((state-with (tool-calls thought-counts)
              (list (cons :tool-calls tool-calls)
                    (cons :activity-thoughts thought-counts)))
            (count (state)
              (substring-no-properties
               (agent-shell-activity-group-count-label
                (list (cons :state state) (cons :group-id "g")))))
            (descriptive (state)
              (substring-no-properties
               (agent-shell-activity-group-descriptive-label
                (list (cons :state state) (cons :group-id "g"))))))
    ;; Counting accumulates and is queryable per group.
    (let ((state (state-with nil nil)))
      (agent-shell--count-group-thought state "g")
      (agent-shell--count-group-thought state "g")
      (should (= 2 (agent-shell--group-thought-count state "g")))
      (should (agent-shell--group-has-thought-p state "g"))
      (should-not (agent-shell--group-has-thought-p state "other"))
      (should (= 0 (agent-shell--group-thought-count state "other"))))
    ;; Thought-only group.
    (let ((state (state-with nil '(("g" . 1)))))
      (should (equal "✓ Activity 1/1" (count state)))
      (should (equal "Thought" (descriptive state))))
    ;; Mixed group: count tallies tools; descriptive mentions the thought.
    (let ((state (state-with '(("t1" (:group-id . "g") (:kind . "execute")
                                (:status . "completed")))
                             '(("g" . 1)))))
      (should (equal "✓ Activity 2/2" (count state)))
      (should (equal "Thought, ran a command" (descriptive state))))))

(ert-deftest agent-shell--activity-group-tally-label-test ()
  "Tally label counts tool calls by category plus thoughts, non-zero only.
Categories render in a fixed order; untyped/MCP calls fold into Other and
think-kind calls into Thinking."
  (cl-flet ((tc (id kind)
              (cons id (list (cons :group-id "g") (cons :kind kind)
                             (cons :status "completed"))))
            (tally (state)
              (when-let* ((s (agent-shell-activity-group-tally-label
                              (list (cons :state state) (cons :group-id "g")))))
                (substring-no-properties s))))
    ;; Full mix, fixed order (Commands, Reads, Edits, Moves, Deletes,
    ;; Thinking), thoughts from the count.
    (should (equal "Commands: 1 Reads: 2 Edits: 1 Thinking: 3"
                   (tally (list (cons :tool-calls
                                      (list (tc "a" "read") (tc "b" "read")
                                            (tc "c" "edit") (tc "d" "execute")))
                                (cons :activity-thoughts '(("g" . 3)))))))
    ;; Edits, moves and deletes are their own categories, in order.
    (should (equal "Edits: 1 Moves: 1 Deletes: 1"
                   (tally (list (cons :tool-calls
                                      (list (tc "a" "edit") (tc "b" "delete") (tc "c" "move")))
                                (cons :activity-thoughts nil)))))
    ;; Searches/Fetches; untyped + \"other\" fold into Other; think-kind
    ;; calls fold into Thinking alongside thought-chunk counts.
    (should (equal "Searches: 1 Fetches: 1 Other: 2 Thinking: 2"
                   (tally (list (cons :tool-calls
                                      (list (tc "a" "search") (tc "b" "fetch")
                                            (tc "c" "other") (tc "d" nil) (tc "e" "think")))
                                (cons :activity-thoughts '(("g" . 1)))))))
    ;; Thought-only group.
    (should (equal "Thinking: 1"
                   (tally (list (cons :tool-calls nil)
                                (cons :activity-thoughts '(("g" . 1)))))))
    ;; Nothing to count -> nil.
    (should-not (tally (list (cons :tool-calls nil) (cons :activity-thoughts nil))))
    ;; Two-tone: label uses the heading face, count uses the default face.
    (let ((s (agent-shell-activity-group-tally-label
              (list (cons :state (list (cons :tool-calls (list (tc "a" "execute")))
                                       (cons :activity-thoughts nil)))
                    (cons :group-id "g")))))
      (should (equal "Commands: 1" (substring-no-properties s)))
      (should (eq 'agent-shell-section-heading (get-text-property 0 'font-lock-face s)))
      (should (eq 'default (get-text-property (1- (length s)) 'font-lock-face s))))))

(ert-deftest agent-shell--on-notification-agent-thought-chunk-face-test ()
  "Test `agent_thought_chunk' rendering hands the body its base face.

Drives an ACP `session/update' notification through
`agent-shell--on-notification' and asserts the body reaching the renderer
carries `agent-shell-thought-body', and that markdown rendered on top of
such a body layers its own faces ahead of the base one."
  (let ((state (list (cons :chunked-group-count 0)
                     (cons :activity-group-count 0)
                     ;; Pre-set so the new-thought branches (transcript
                     ;; header, group relabel) are skipped; the test only
                     ;; exercises content rendering.
                     (cons :last-entry-type "agent_thought_chunk")
                     (cons :last-activity-time nil)))
        (rendered nil))
    (cl-letf (((symbol-function 'agent-shell--active-requests-p)
               (lambda (_state) t))
              ((symbol-function 'agent-shell--append-transcript)
               #'ignore)
              ((symbol-function 'agent-shell--emit-event)
               #'ignore)
              ((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args) (setq rendered (plist-get args :body)))))
      (agent-shell--on-notification
       :state state
       :acp-notification '((method . "session/update")
                           (params
                            (update
                             (sessionUpdate . "agent_thought_chunk")
                             (content (type . "text")
                                      (text . "plain **bold** `code`"))))))
      (should (equal "plain **bold** `code`" (substring-no-properties rendered)))
      (should (eq 'agent-shell-thought-body (get-text-property 0 'face rendered)))
      ;; Also on `font-lock-face', the property that survives
      ;; fontification clearing `face' in a body the renderer never saw.
      (should (eq 'agent-shell-thought-body
                  (get-text-property 0 'font-lock-face rendered)))
      ;; Rendered, unstyled text keeps the base face alone; markup keeps
      ;; its own face ahead of the base one.
      (let ((markdown (agent-shell-markdown-convert rendered)))
        (should (equal "plain bold code" (substring-no-properties markdown)))
        (should (eq 'agent-shell-thought-body (get-text-property 0 'face markdown)))
        (should (equal '(agent-shell-markdown-bold agent-shell-thought-body)
                       (get-text-property 6 'face markdown)))
        (should (equal '(agent-shell-markdown-inline-code agent-shell-thought-body)
                       (get-text-property 11 'face markdown)))))))

(ert-deftest agent-shell--adapt-notification-test ()
  "Test `agent-shell--adapt-notification'."
  (let ((state (agent-shell--make-state
                :agent-config (agent-shell-make-agent-config :identifier 'test))))
    (should (equal (agent-shell--adapt-notification
                    :state state
                    :acp-notification '((some-key . "some-value")))
                  '((some-key . "some-value")))))
  (let* ((adapter (lambda (&key acp-notification)
                    (cons '(adapted . t) acp-notification)))
         (state (agent-shell--make-state
                 :agent-config (agent-shell-make-agent-config
                                :identifier 'test
                                :notification-adapter adapter))))
    (should (equal (agent-shell--adapt-notification
                    :state state
                    :acp-notification '((some-key . "some-value")))
                  '((adapted . t) (some-key . "some-value"))))))

(ert-deftest agent-shell--buffer-name-prefix-test ()
  "Test `agent-shell--buffer-name-prefix' across formats."
  (let ((agent-shell-buffer-name-format 'default))
    (should (equal (agent-shell--buffer-name-prefix "Claude")
                   "Claude Agent @ ")))
  (let ((agent-shell-buffer-name-format 'kebab-case))
    (should (equal (agent-shell--buffer-name-prefix "Claude Code")
                   "claude-code-agent @ ")))
  ;; A custom formatter cannot be decomposed into a prefix.
  (let ((agent-shell-buffer-name-format (lambda (agent-name project-name)
                                          (format "%s: %s" agent-name project-name))))
    (should (equal (agent-shell--buffer-name-prefix "Claude") nil))))

(ert-deftest agent-shell--format-buffer-name-test ()
  "Test `agent-shell--format-buffer-name' across formats."
  (let ((agent-shell-buffer-name-format 'default))
    (should (equal (agent-shell--format-buffer-name "Claude" "agent-shell")
                   "Claude Agent @ agent-shell")))
  (let ((agent-shell-buffer-name-format 'kebab-case))
    (should (equal (agent-shell--format-buffer-name "Claude Code" "agent-shell")
                   "claude-code-agent @ agent-shell")))
  (let ((agent-shell-buffer-name-format (lambda (agent-name project-name)
                                          (format "%s: %s" agent-name project-name))))
    (should (equal (agent-shell--format-buffer-name "Claude" "agent-shell")
                   "Claude: agent-shell"))))

(ert-deftest agent-shell--live-input-prompt-p-test ()
  "Test `agent-shell--live-input-prompt-p' across buffer states."
  (with-temp-buffer
    ;; Prompt at the very end of the buffer with an empty input area is live.
    (insert "output\n")
    (let ((start (copy-marker (point) nil)))
      (insert "> ")
      ;; End marker stays pinned at the prompt end (insertion type nil)
      ;; so text appended after it does not drag it along, mirroring a
      ;; real `comint-last-prompt' cdr.
      (let ((prompt (cons start (copy-marker (point) nil))))
        (should (agent-shell--live-input-prompt-p prompt))
        ;; Unsubmitted typed input (no `field' `output') keeps it live.
        (insert "typed")
        (should (agent-shell--live-input-prompt-p prompt))
        ;; Agent output streaming below a stale prompt makes it not-live.
        (let ((out-start (point)))
          (insert "streamed")
          (put-text-property out-start (point) 'field 'output))
        (should-not (agent-shell--live-input-prompt-p prompt))))))

(ert-deftest agent-shell--live-input-prompt-p-narrowed-above-prompt-test ()
  "Guard against inverted `text-property-any' bounds while narrowed.
Regression for session restore inserting the truncated-history note
above the live prompt, where the buffer is narrowed to end before the
prompt and the prompt end sits past the accessible `point-max'."
  (with-temp-buffer
    (insert "output\n")
    (let ((prompt-start (copy-marker (point) nil)))
      (insert "> ")
      (let ((prompt (cons prompt-start (copy-marker (point) t))))
        (save-restriction
          ;; Narrow above the prompt: `point-max' now precedes the prompt
          ;; end marker, mirroring `agent-shell--render-pending-restore'.
          (narrow-to-region (point-min) (marker-position prompt-start))
          ;; Must not raise `Args out of range' and must report not-live.
          (should-not (agent-shell--live-input-prompt-p prompt)))))))

(ert-deftest agent-shell--realign-on-change-schedules-regardless-of-width ()
  "Schedule a re-align on every window change, not just width changes.
Regression: content rendered while the buffer was off-screen (a table
laid out with `string-width', an image sized against no window) is not
correct for the display.  Bringing the buffer back into a same-width
window must still schedule a re-render; the per-item staleness decision
belongs to `agent-shell-markdown-rerender-tables' /
`agent-shell-markdown-rerender-images', so this hook must not gate on
the window width being unchanged."
  (with-temp-buffer
    (let ((scheduled 0))
      (cl-letf (((symbol-function 'window-live-p) (lambda (_) t))
                ((symbol-function 'window-body-width) (lambda (&rest _) 800))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (&rest _) (setq scheduled (1+ scheduled)) 'timer)))
        (agent-shell--realign-on-change 'window)
        (should (= scheduled 1))
        ;; Same width again: a previously off-screen item may now be
        ;; stale, so this must still schedule.
        (agent-shell--realign-on-change 'window)
        (should (= scheduled 2))))))

(ert-deftest agent-shell--make-permission-actions-orders-allow-before-reject ()
  "Offer allowing before rejecting, whatever order the agent sent.
Regression: Claude Code sends `reject_once' first, which used to render
Deny as the leftmost (and thus default) button."
  (should (equal '("allow_once" "reject_once" "allow_always")
                 (mapcar (lambda (action) (map-elt action :kind))
                         (agent-shell--make-permission-actions
                          '(((kind . "reject_once")
                             (name . "Deny")
                             (optionId . "reject"))
                            ((kind . "allow_once")
                             (name . "Allow Once")
                             (optionId . "allow"))
                            ((kind . "allow_always")
                             (name . "Always Allow")
                             (optionId . "allow-always"))))))))

(ert-deftest agent-shell--make-permission-actions-keeps-same-kind-order ()
  "Keep the agent's order among options sharing a kind.
Only the first of a kind gets a keybinding, so re-ordering them would
move the binding to a different option."
  (let ((actions (agent-shell--make-permission-actions
                  '(((kind . "allow_once")
                     (name . "Allow Once")
                     (optionId . "allow"))
                    ((kind . "allow_once")
                     (name . "Allow this session")
                     (optionId . "allow-session"))
                    ((kind . "reject_once")
                     (name . "Deny")
                     (optionId . "reject"))))))
    (should (equal '("Allow Once" "Allow this session" "Deny")
                   (mapcar (lambda (action) (map-elt action :option)) actions)))
    (should (equal "y" (map-elt (nth 0 actions) :char)))
    (should-not (map-elt (nth 1 actions) :char))))

(ert-deftest agent-shell--render-markdown-runs-external-renderers-by-default ()
  "Render functions see message bodies, the default for every call site."
  (let ((calls 0))
    (let ((agent-shell-markdown-render-functions
           (list (lambda (_context) (setq calls (1+ calls)) nil))))
      (with-temp-buffer
        (insert "Result: \\(x^2\\).")
        (agent-shell--render-markdown)))
    (should (equal 1 calls))))

(ert-deftest agent-shell--render-markdown-suppresses-external-renderers ()
  "Right labels opt out of external Markdown renderers.
An external renderer draws images (e.g. LaTeX math), which single-line
labels already decline via `:render-images'.  Both the buffer-local and
the global hook value must stay quiet."
  (let ((global-calls 0)
        (local-calls 0))
    (let ((global-renderer (lambda (_context)
                             (setq global-calls (1+ global-calls))
                             nil)))
      (add-hook 'agent-shell-markdown-render-functions global-renderer)
      (unwind-protect
          (with-temp-buffer
            (add-hook 'agent-shell-markdown-render-functions
                      (lambda (_context) (setq local-calls (1+ local-calls)) nil)
                      nil t)
            (insert "find . \\( -name '*.nix' \\)")
            (agent-shell--render-markdown :render-images nil
                                          :external-renderers nil))
        (remove-hook 'agent-shell-markdown-render-functions global-renderer)))
    (should (equal 0 global-calls))
    (should (equal 0 local-calls))))

(ert-deftest agent-shell--icon-and-kind-status-kind-label-test ()
  "Kind renders capitalized, unpadded, beside the status icon."
  (let ((label (lambda (status kind)
                 (when-let* ((text (agent-shell--icon-and-kind-status-kind-label
                                    status kind)))
                   (substring-no-properties text)))))
    (should (equal "✓ Command" (funcall label "completed" "execute")))
    (should (equal "◔ Find" (funcall label "in_progress" "search")))
    (should (equal "◔ Command" (funcall label "pending" "execute")))
    (should (equal "✗ Delete" (funcall label "failed" "delete")))
    ;; Underscores read as words.
    (should (equal "✓ Switch Mode" (funcall label "completed" "switch_mode")))
    ;; Kind-less entries (plan steps, group headers) render icon only.
    (should (equal "✓" (funcall label "completed" nil)))
    (should (equal "Read" (funcall label nil "read")))
    (should (equal nil (funcall label nil nil)))))

(ert-deftest agent-shell--icon-and-kind-status-kind-label-faces-test ()
  "The icon tracks status while the kind reads as a section heading."
  ;; Position based: the icon leads, the kind runs to the end.
  (let ((label (agent-shell--icon-and-kind-status-kind-label "completed" "execute")))
    (should (equal 'agent-shell-success
                   (get-text-property 0 'font-lock-face label)))
    (should (equal 'agent-shell-section-heading
                   (get-text-property (1- (length label)) 'font-lock-face label)))))

(ert-deftest agent-shell--thought-process-icon-falls-back-test ()
  "The icon stands aside for the fallback when a display cannot draw it."
  (let ((agent-shell-thought-process-icon "⚹"))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (&rest _) t)))
      (should (equal "⚹" (agent-shell--thought-process-icon))))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (&rest _) nil)))
      (should (equal "◇" (agent-shell--thought-process-icon)))))
  ;; An emptied icon opts out entirely, fallback included.
  (let ((agent-shell-thought-process-icon ""))
    (should (equal nil (agent-shell--thought-process-icon)))))

(ert-deftest agent-shell-make-tool-call-label-titles-render-plain-test ()
  "Titles carry content, so they render plain beside the status label."
  (let ((state '((:tool-calls . (("t1" . ((:kind . "read")
                                          (:status . "completed")
                                          (:title . "file.el"))))))))
    (should (equal 'default
                   (get-text-property
                    0 'font-lock-face
                    (map-elt (agent-shell-make-tool-call-label state "t1") :title))))))

(ert-deftest agent-shell-make-tool-call-label-multiline-command-test ()
  "A multiline command reads as truncated, so its title ends in an ellipsis.

Showing only the first line makes an innocent-looking \"cd somewhere\"
stand in for whatever else the command runs."
  (let ((label (lambda (title)
                 (substring-no-properties
                  (map-elt (agent-shell-make-tool-call-label
                            `((:tool-calls . (("t1" . ((:kind . "execute")
                                                       (:status . "completed")
                                                       (:title . ,title))))))
                            "t1")
                           :title)))))
    (should (equal "cd /tmp…" (funcall label "cd /tmp\ngrep -n foo bar.el")))
    ;; A lone command stays as-is, trailing newline or not.
    (should (equal "cd /tmp" (funcall label "cd /tmp")))
    (should (equal "cd /tmp" (funcall label "cd /tmp\n")))))

(ert-deftest agent-shell--tag-untagged-output-tags-same-chars-test ()
  "Tagging only the untagged tail covers what a whole-range tag would."
  (with-temp-buffer
    (insert "already tagged" "freshly appended")
    (add-text-properties (point-min) 15 '(field output))
    (agent-shell--tag-untagged-output (point-min) (point-max))
    (should (equal nil (text-property-not-all (point-min) (point-max)
                                              'field 'output)))))

(ert-deftest agent-shell--tag-untagged-output-signals-tail-only-test ()
  "Re-tagging a streamed block reports only the newly appended chars.

`add-text-properties' signals a modification spanning the whole range it
is handed, so re-tagging the whole block per chunk would hand
`jit-lock-after-change' the entire block every time (issue #757)."
  (with-temp-buffer
    (insert "already tagged")
    (add-text-properties (point-min) (point-max) '(field output))
    (let ((signalled '()))
      (add-hook 'after-change-functions
                (lambda (beginning end _length)
                  (push (cons beginning end) signalled))
                nil t)
      (insert "appended")
      (setq signalled '())
      (agent-shell--tag-untagged-output (point-min) (point-max))
      (should (equal '((15 . 23)) signalled)))))

(ert-deftest agent-shell--tag-untagged-output-skips-fully-tagged-test ()
  "A block that needs no tagging signals no modification at all."
  (with-temp-buffer
    (insert "already tagged")
    (add-text-properties (point-min) (point-max) '(field output))
    (let ((signalled '()))
      (add-hook 'after-change-functions
                (lambda (beginning end _length)
                  (push (cons beginning end) signalled))
                nil t)
      (agent-shell--tag-untagged-output (point-min) (point-max))
      (should (equal '() signalled)))))

(ert-deftest agent-shell--make-unhandled-notification-body-links-test ()
  "The feature request link is markdown the renderer turns clickable.

The body is written as markdown rather than propertized by hand, so
this guards the assumption that `agent-shell-markdown' picks the link
up: the markup must be gone and the URL recoverable from the text."
  (let ((rendered (agent-shell-markdown-convert
                   (agent-shell--make-unhandled-notification-body
                    '((method . "some/unknown"))))))
    (should (string-match-p "please file a feature request" rendered))
    (should-not (string-match-p "\\[please file a feature request\\]" rendered))
    (should (equal "https://github.com/xenodium/agent-shell/issues/new/choose"
                   (get-text-property (string-match "please file" rendered)
                                      'agent-shell-markdown-url rendered)))))

(ert-deftest agent-shell--make-file-link-uses-label-verbatim-test ()
  "A file link keeps its label exactly.

These go into the prompt, whose buffer text is sent to the agent, so
`agent-shell--make-button' must not pad or bracket them the way it does
a real button."
  (let ((link (agent-shell--make-file-link :label "@/tmp/notes.org"
                                           :file "/tmp/notes.org"
                                           :hint "open")))
    (should (equal "@/tmp/notes.org" (substring-no-properties link)))
    (should (get-text-property 0 'keymap link))
    (should (eq 'hand (get-text-property 0 'pointer link)))))

(ert-deftest agent-shell--make-button-boxed-test ()
  "A boxed button decorates its text; an unboxed one does not."
  (should (string-match-p "@/tmp/x"
                          (substring-no-properties
                           (agent-shell--make-button :text "@/tmp/x" :boxed nil
                                                     :action #'ignore))))
  (should-not (equal "@/tmp/x"
                     (substring-no-properties
                      (agent-shell--make-button :text "@/tmp/x"
                                                :action #'ignore))))
  ;; Unboxed text keeps whatever face it arrived with, rather than
  ;; merging against a nil box face.
  (should (equal 'agent-shell-link
                 (get-text-property 0 'face
                                    (agent-shell--make-button
                                     :text (propertize "x" 'face 'agent-shell-link)
                                     :boxed nil :action #'ignore)))))

(ert-deftest agent-shell--typing-at-prompt-p-test ()
  "A character key typed at an idle prompt is input, not a command."
  (let ((last-command-event ?+)
        (this-command 'agent-shell-image-scale-increase))
    (cl-letf (((symbol-function 'this-command-keys-vector) (lambda () [?+]))
              ((symbol-function 'key-binding)
               (lambda (&rest _) 'agent-shell-image-scale-increase)))
      (cl-letf (((symbol-function 'shell-maker-busy) (lambda (&rest _) nil))
                ((symbol-function 'shell-maker-point-at-last-prompt-p)
                 (lambda (&rest _) t)))
        (should (agent-shell--typing-at-prompt-p)))
      ;; Away from the prompt (reading output), it's a command.
      (cl-letf (((symbol-function 'shell-maker-busy) (lambda (&rest _) nil))
                ((symbol-function 'shell-maker-point-at-last-prompt-p)
                 (lambda (&rest _) nil)))
        (should-not (agent-shell--typing-at-prompt-p)))
      ;; Busy shell: the prompt isn't taking input.
      (cl-letf (((symbol-function 'shell-maker-busy) (lambda (&rest _) t))
                ((symbol-function 'shell-maker-point-at-last-prompt-p)
                 (lambda (&rest _) t)))
        (should-not (agent-shell--typing-at-prompt-p)))))
  ;; Invoked as M-x rather than by its key: a command, even at the prompt.
  (let ((last-command-event nil)
        (this-command 'agent-shell-image-scale-increase))
    (cl-letf (((symbol-function 'shell-maker-busy) (lambda (&rest _) nil))
              ((symbol-function 'shell-maker-point-at-last-prompt-p)
               (lambda (&rest _) t)))
      (should-not (agent-shell--typing-at-prompt-p)))))

(ert-deftest agent-shell--render-deferred-images-test ()
  "A body ending in image markup renders once the turn is over.

Streaming holds that markup back in case a `{width=...}\' block is
still coming, so nothing else would ever render it."
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image) (lambda (&rest _) '(image :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil)))
          (with-temp-buffer
            (insert (format "plot\n\n![alt](%s)" image-file))
            (put-text-property (point-min) (point-max) 'agent-shell-ui-section 'body)
            (agent-shell--render-deferred-images)
            (should (equal "plot\n\nalt" (buffer-substring-no-properties
                                          (point-min) (point-max))))
            (should (eq 'image (car-safe (get-text-property (1- (point-max))
                                                           'display)))))
          ;; A collapsed body renders too, staying hidden: the render on
          ;; expand isn't marked complete, so skipping it here would leave
          ;; the image raw for good.
          (with-temp-buffer
            (insert (format "plot\n\n![alt](%s)" image-file))
            (put-text-property (point-min) (point-max) 'agent-shell-ui-section 'body)
            (put-text-property (point-min) (point-max) 'invisible t)
            (agent-shell--render-deferred-images)
            (should (equal "plot\n\nalt" (buffer-substring-no-properties
                                          (point-min) (point-max))))
            (should (eq 'image (car-safe (get-text-property (1- (point-max))
                                                           'display))))
            (should (eq t (get-text-property (1- (point-max)) 'invisible))))
          ;; Text outside a fragment body is not a shell rendering target.
          (with-temp-buffer
            (insert (format "plot\n\n![alt](%s)" image-file))
            (agent-shell--render-deferred-images)
            (should (string-suffix-p (format "![alt](%s)" image-file)
                                     (buffer-substring-no-properties
                                      (point-min) (point-max))))))
      (delete-file image-file))))

(ert-deftest agent-shell-file-display-action-test ()
  "Files open per `agent-shell-file-display-action\'.

The default takes over the current window, as before the setting
existed; an action opening elsewhere leaves the conversation in view,
with the link\'s line range still selected in whichever window the
file landed in."
  (let ((file (make-temp-file "agent-shell-tests" nil nil "one\ntwo\nthree\nfour\n")))
    (unwind-protect
        (with-selected-window (frame-first-window)
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (let ((agent-shell-file-display-action
                   '((display-buffer-reuse-window display-buffer-same-window)))
                  (shell-window (selected-window)))
              (agent-shell-markdown-visit-file :file file :line-start 2 :line-end 3)
              ;; Took over the window the link was followed from.
              (should (eq shell-window (selected-window)))
              (should (equal (file-truename file)
                             (file-truename (buffer-file-name))))
              (should (equal "two\nthree"
                             (buffer-substring-no-properties (point) (mark)))))
            (when (get-file-buffer file)
              (kill-buffer (get-file-buffer file)))
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (let ((agent-shell-file-display-action '(display-buffer-pop-up-window))
                  (shell-window (selected-window)))
              (agent-shell-markdown-visit-file :file file :line-start 2 :line-end 3)
              ;; The conversation stays on screen, the file lands elsewhere.
              (should-not (eq shell-window (selected-window)))
              (should (window-live-p shell-window))
              (should (equal "*scratch*" (buffer-name (window-buffer shell-window))))
              (should (equal (file-truename file)
                             (file-truename (buffer-file-name))))
              (should (equal "two\nthree"
                             (buffer-substring-no-properties (point) (mark)))))))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest agent-shell-file-display-action-showing-nothing-test ()
  "An action showing no window is legal rather than an error.

`display-buffer-no-window\' with `allow-no-window\' returns nil, which
`select-window\' would otherwise choke on."
  (let ((file (make-temp-file "agent-shell-tests" nil nil "one\ntwo\n")))
    (unwind-protect
        (with-selected-window (frame-first-window)
          (save-window-excursion
            (let ((agent-shell-file-display-action
                   '(display-buffer-no-window . ((allow-no-window . t)))))
              (should-not (agent-shell-markdown-visit-file :file file :line-start 2)))))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (delete-file file))))

;;; Tests for scheduled directory cleanup

(ert-deftest agent-shell--clean-up-deletes-pending-directory-test ()
  "Test killing a shell deletes the directory it scheduled."
  (let ((temp-dir (make-temp-file "temp-" t))
        ;; Trashing can outlive the kill, so assert on an outright delete.
        (delete-by-moving-to-trash nil))
    (unwind-protect
        (progn
          (with-current-buffer (generate-new-buffer " *agent-shell-cleanup-test*")
            (setq major-mode 'agent-shell-mode)
            (setq-local agent-shell--state (agent-shell--make-state))
            (setq-local agent-shell--pending-directory-cleanup temp-dir)
            (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t)
            (kill-buffer))
          (should-not (file-directory-p temp-dir)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest agent-shell--clean-up-ignores-default-directory-test ()
  "Test cleanup spares the shell's `default-directory'.

A temp shell's `default-directory' can end up outside the directory it
created (project detection resolves \"/tmp\" as the root when
\"/tmp/.git\" exists).  Cleanup must delete what was scheduled, never
where the buffer happens to point."
  (let ((temp-dir (make-temp-file "temp-" t))
        (sibling-file (make-temp-file "agent-shell-bystander"))
        (delete-by-moving-to-trash nil))
    (unwind-protect
        (progn
          (with-current-buffer (generate-new-buffer " *agent-shell-cleanup-test*")
            (setq major-mode 'agent-shell-mode)
            (setq-local agent-shell--state (agent-shell--make-state))
            (setq-local agent-shell--pending-directory-cleanup temp-dir)
            (setq-local default-directory temporary-file-directory)
            (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t)
            (kill-buffer))
          (should-not (file-directory-p temp-dir))
          (should (file-directory-p temporary-file-directory))
          (should (file-exists-p sibling-file)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t))
      (delete-file sibling-file))))

(ert-deftest agent-shell--clean-up-without-pending-directory-test ()
  "Test cleanup deletes nothing when no directory was scheduled.

Shells working in the user's own directories schedule nothing, so the
cleanup every shell runs has nothing to delete."
  (let ((project-dir (make-temp-file "agent-shell-project" t))
        (delete-by-moving-to-trash nil))
    (unwind-protect
        (progn
          (with-current-buffer (generate-new-buffer " *agent-shell-cleanup-test*")
            (setq major-mode 'agent-shell-mode)
            (setq-local agent-shell--state (agent-shell--make-state))
            (setq-local default-directory project-dir)
            (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t)
            (kill-buffer))
          (should (file-directory-p project-dir)))
      (delete-directory project-dir t))))

(ert-deftest agent-shell-restart-inherits-pending-directory-cleanup-test ()
  "Test restarting a temp shell hands its directory to the new shell.

Restart kills the shell buffer, so the outgoing buffer must unschedule
the directory and the incoming one take it over.  Otherwise the restarted
shell is left working in a directory that was just deleted."
  (let ((temp-dir (make-temp-file "temp-" t))
        (delete-by-moving-to-trash nil)
        (new-shell-buffer nil)
        (shell-buffer (generate-new-buffer " *agent-shell-restart-test*")))
    (unwind-protect
        (progn
          (with-current-buffer shell-buffer
            (setq major-mode 'agent-shell-mode)
            (setq-local agent-shell--state (agent-shell--make-state))
            (setq-local agent-shell--pending-directory-cleanup temp-dir)
            (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t)
            (cl-letf (((symbol-function 'agent-shell--start)
                       (lambda (&rest _args)
                         (setq new-shell-buffer
                               (generate-new-buffer " *agent-shell-restart-test-new*"))))
                      ((symbol-function 'shell-maker-set-buffer-name) #'ignore)
                      ((symbol-function 'agent-shell--display-buffer) #'ignore)
                      ((symbol-function 'agent-shell-viewport--show-buffer) #'ignore))
              (agent-shell-restart)))
          (should (file-directory-p temp-dir))
          (should (equal (buffer-local-value 'agent-shell--pending-directory-cleanup
                                             new-shell-buffer)
                         temp-dir)))
      (when (buffer-live-p shell-buffer)
        (kill-buffer shell-buffer))
      (when (buffer-live-p new-shell-buffer)
        (kill-buffer new-shell-buffer))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))
(ert-deftest agent-shell-experimental--make-session-steering-request-test ()
  "Test `agent-shell-experimental--make-session-steering-request'."
  (let ((request (agent-shell-experimental--make-session-steering-request
                  :session-id "sess-1"
                  :prompt '(((type . "text") (text . "actually, just the filenames"))))))
    (should (equal (map-elt request :method) "_session/steering"))
    (should (equal (map-nested-elt request '(:params sessionId)) "sess-1"))
    ;; Sent as a vector so it serializes to a JSON array, like session/prompt.
    (should (vectorp (map-nested-elt request '(:params prompt))))
    (should (equal (map-nested-elt request '(:params prompt))
                   [((type . "text") (text . "actually, just the filenames"))]))
    ;; Opts out of the agent starting a turn of its own when none is running.
    (should (equal (map-nested-elt request '(:params _meta steering idleBehavior))
                   "promptRequired")))
  (should-error (agent-shell-experimental--make-session-steering-request
                 :prompt '(((type . "text") (text . "hi")))))
  (should-error (agent-shell-experimental--make-session-steering-request
                 :session-id "sess-1")))

(cl-defun agent-shell-tests--steer-guard (&key busy supported status confirm)
  "Invoke `agent-shell-prompt-steer' and report what its guards decided.

BUSY, SUPPORTED and STATUS are what `shell-maker-busy',
`agent-shell-steering-supported-p' and `agent-shell-status' report.
CONFIRM is how the user answers, should a guard ask.

Returns `steered' when the prompt reached the agent, or the `user-error'
message explaining why it did not."
  (cl-letf (((symbol-function 'agent-shell--shell-buffer)
             (lambda (&rest _) (current-buffer)))
            ((symbol-function 'shell-maker-busy) (lambda (&rest _) busy))
            ((symbol-function 'agent-shell-steering-supported-p)
             (lambda (&rest _) supported))
            ((symbol-function 'agent-shell-status) (lambda (&rest _) status))
            ((symbol-function 'y-or-n-p) (lambda (&rest _) confirm))
            ((symbol-function 'agent-shell--state) (lambda (&rest _) nil))
            ((symbol-function 'agent-shell-experimental--send-steering)
             (lambda (&rest _) 'steered)))
    (condition-case error
        (agent-shell-prompt-steer "actually, just the filenames")
      (user-error (error-message-string error)))))

(ert-deftest agent-shell-prompt-steer-test ()
  "Test `agent-shell-prompt-steer' refuses when there is no turn to steer."
  (should (eq (agent-shell-tests--steer-guard :busy t :supported t :status 'busy)
              'steered))
  (should (equal (agent-shell-tests--steer-guard :busy nil :supported t :status 'ready)
                 "No turn to steer; the agent is idle"))
  ;; An agent that never advertised steering is only ever queued to.
  (should (equal (agent-shell-tests--steer-guard :busy t :supported nil :status 'busy)
                 "This agent does not support steering"))
  ;; Steering a shell awaiting a permission answer can interrupt the turn,
  ;; which rejects that permission too, so it is the user's call to make.
  (should (equal (agent-shell-tests--steer-guard :busy t :supported t :status 'blocked
                                                 :confirm nil)
                 "Steering cancelled"))
  (should (eq (agent-shell-tests--steer-guard :busy t :supported t :status 'blocked
                                              :confirm t)
              'steered)))

(cl-defun agent-shell-tests--steer-outcome (&key outcome busy request-failed)
  "Steer a prompt, answer with OUTCOME, and return what became of it.

OUTCOME is the agent's answer verbatim, as it arrives on the wire.  BUSY
is what `shell-maker-busy' reports while the answer is handled: a shell
that has not yet processed its own `session/prompt' response is still
busy even though the agent says the turn ended.  REQUEST-FAILED answers
the request itself with an error instead.

Returns the things that happened, in order: `rendered' (shown as a user
prompt in the running turn), `submitted' (sent as an ordinary prompt),
`queued' (added to the pending queue), `reported' (explained in a shell
fragment) and `interrupted' (the running turn cancelled)."
  (let ((happened)
        ;; A turn is running when the guards check, which is what lets the
        ;; steer go out at all.  BUSY is what the shell reports later, when
        ;; the agent's answer comes back.
        (running t))
    (cl-letf (((symbol-function 'agent-shell--shell-buffer)
               (lambda (&rest _) (current-buffer)))
              ((symbol-function 'agent-shell--state)
               (lambda (&rest _) (list (cons :buffer (current-buffer))
                                       (cons :session (list (cons :id "sess-1")))
                                       (cons :request-count 0)
                                       (cons :last-entry-type nil))))
              ((symbol-function 'agent-shell-steering-supported-p)
               (lambda (&rest _) t))
              ((symbol-function 'agent-shell-status) (lambda (&rest _) 'busy))
              ((symbol-function 'agent-shell-interrupt)
               (lambda (&rest _) (push 'interrupted happened)))
              ((symbol-function 'shell-maker-busy) (lambda (&rest _) running))
              ((symbol-function 'agent-shell--expand-truncated-regions)
               (lambda (text) text))
              ((symbol-function 'agent-shell--prompt-content-blocks)
               (lambda (text) (vector (list (cons 'type "text")
                                            (cons 'text text)))))
              ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
              ((symbol-function 'agent-shell--send-request)
               (lambda (&rest args)
                 (setq running busy)
                 (if request-failed
                     (funcall (plist-get args :on-failure)
                              '((message . "agent said no")) "raw")
                   (funcall (plist-get args :on-success)
                            (list (cons 'outcome outcome))))))
              ((symbol-function 'agent-shell-experimental--render-steered-prompt)
               (lambda (&rest _) (push 'rendered happened)))
              ((symbol-function 'agent-shell--insert-to-shell-buffer)
               (lambda (&rest _) (push 'submitted happened)))
              ((symbol-function 'agent-shell--prompt-queue-enqueue)
               (lambda (&rest _) (push 'queued happened)))
              ((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest _) (push 'reported happened))))
      (agent-shell-prompt-steer "actually, just the filenames"))
    (nreverse happened)))

(ert-deftest agent-shell-prompt-steer-outcome-test ()
  "Test `agent-shell-prompt-steer' acts on the agent's own answer."
  (should (equal (agent-shell-tests--steer-outcome :outcome "injected") '(rendered)))
  ;; The turn had ended and the agent handed the prompt back, so send it as
  ;; an ordinary one.  Claude only, and only because the request opts in.
  (should (equal (agent-shell-tests--steer-outcome :outcome "promptRequired")
                 '(submitted)))
  (should (equal (agent-shell-tests--steer-outcome :outcome "startedNewTurn")
                 '(reported)))
  ;; Steering never queues: that is `agent-shell-prompt-queue''s job, and a
  ;; queued prompt would reach the agent long after it was asked for.  The
  ;; turn is interrupted so the agent does not carry on in a direction the
  ;; user believes they already corrected.
  (should (equal (agent-shell-tests--steer-outcome :outcome "failed")
                 '(reported interrupted)))
  ;; An outcome no agent answers with today must not read as delivered.
  (should (equal (agent-shell-tests--steer-outcome :outcome "somethingNew")
                 '(reported interrupted)))
  (should (equal (agent-shell-tests--steer-outcome :outcome nil)
                 '(reported interrupted)))
  ;; A request that never got an answer is handled the same way.
  (should (equal (agent-shell-tests--steer-outcome :request-failed t)
                 '(reported interrupted)))
  ;; Submitting into a still-busy shell would error, so it reports too.
  (should (equal (agent-shell-tests--steer-outcome :outcome "promptRequired" :busy t)
                 '(reported interrupted))))

(cl-defun agent-shell-tests--render-steered-prompt (prompt &key idle)
  "Render PROMPT into a bare shell buffer mid-turn.

IDLE renders as though the turn ended while the steer was in flight, so
a live input prompt already sits at the buffer end.

Returns an alist of the resulting buffer text and the `:last-entry-type'
left behind."
  (let* ((buffer (generate-new-buffer " *agent-shell-steer-render-test*"))
         (fake-process (start-process "fake-agent" buffer "cat")))
    (set-process-query-on-exit-flag fake-process nil)
    (unwind-protect
        (with-current-buffer buffer
          (comint-mode)
          (setq-local comint-prompt-regexp "^Claude> ")
          (let ((state (list (cons :buffer (current-buffer))
                             (cons :chunked-group-count 0)
                             (cons :last-entry-type "agent_message_chunk")
                             (cons :agent-config '((:shell-prompt . "Claude> "))))))
            (cl-letf (((symbol-function 'shell-maker--process) (lambda () fake-process))
                      ((symbol-function 'shell-maker-busy) (lambda (&rest _) (not idle))))
              ;; The turn so far: a prompt the user submitted and the
              ;; agent's answer streaming under it.
              (shell-maker--output-filter fake-process "Claude> ")
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "list the files<shell-maker-end-of-prompt>\nListing "))
              ;; The turn ended while the steer was in flight, so the shell
              ;; already printed the next prompt and is waiting on input.
              (when idle
                (shell-maker--output-filter fake-process "\nClaude> "))
              (agent-shell-experimental--render-steered-prompt :state state :prompt prompt)
              (list (cons :text (buffer-substring-no-properties (point-min) (point-max)))
                    (cons :last-entry-type (map-elt state :last-entry-type))))))
      (when (process-live-p fake-process)
        (delete-process fake-process))
      (kill-buffer buffer))))

(ert-deftest agent-shell-experimental--render-steered-prompt-test ()
  "Test a steered prompt renders as a closed user prompt.
Neither adapter echoes a steered prompt back, so it is rendered here or
it is nowhere.  The end-of-prompt marker closes it: chat mode reads the
last prompt with no marker after it as the live one, and the prompt bar
hides that."
  (let ((rendered (agent-shell-tests--render-steered-prompt "just the filenames")))
    (should (equal (map-elt rendered :text)
                   (concat "Claude> list the files<shell-maker-end-of-prompt>\n"
                           "Listing \n\n"
                           "Claude> [steer] just the filenames"
                           "<shell-maker-end-of-prompt>")))
    ;; Not "user_message_chunk": that asks the notification dispatch to
    ;; insert an end-of-prompt marker of its own on the next update.
    (should-not (equal (map-elt rendered :last-entry-type) "user_message_chunk")))
  ;; The turn can end while the steer is in flight, leaving a live input
  ;; prompt at the buffer end.  Rendering there would put the prompt in
  ;; comint's input area, where submitting sends it as input.
  (let ((rendered (agent-shell-tests--render-steered-prompt "just the filenames" :idle t)))
    (should (equal (map-elt rendered :text)
                   (concat "Claude> list the files<shell-maker-end-of-prompt>\n"
                           "Listing \n\n"
                           "Claude> [steer] just the filenames"
                           "<shell-maker-end-of-prompt>\n"
                           "Claude> ")))))

(defmacro agent-shell-tests--with-rendered-shell (markdown &rest body)
  "Render MARKDOWN in a temporary shell buffer and run BODY with point at start.

Only the markdown items (links, images, source blocks and tables) are
left navigable: prompts, blocks and permission buttons need a live
shell, so item navigation finds none of them here."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (setq major-mode 'agent-shell-mode)
     (insert ,markdown)
     (agent-shell-markdown-replace-markup)
     (goto-char (point-min))
     (cl-letf (((symbol-function 'agent-shell--typing-at-prompt-p) #'ignore)
               ((symbol-function 'comint-next-prompt) #'ignore)
               ((symbol-function 'agent-shell-ui-forward-block) #'ignore)
               ((symbol-function 'agent-shell-ui-backward-block) #'ignore)
               ((symbol-function 'agent-shell-next-permission-button) #'ignore)
               ((symbol-function 'agent-shell-previous-permission-button) #'ignore))
       ,@body)))

(ert-deftest agent-shell-next-item-walks-into-and-out-of-a-table ()
  "Next item enters a table at its first cell, walks it, then moves on."
  (agent-shell-tests--with-rendered-shell
      "Intro [before](https://before.com/x)

| A | B |
|---|---|
| 1 | 2 |

Then [after](https://after.com/y)
"
    (agent-shell-next-item)
    (should (looking-at-p "before"))
    ;; The table is entered at its first cell, not at whatever part of it
    ;; comes first.
    (agent-shell-next-item)
    (should (eq (char-after) ?A))
    (agent-shell-next-item)
    (should (eq (char-after) ?B))
    (agent-shell-next-item)
    (should (eq (char-after) ?1))
    (agent-shell-next-item)
    (should (eq (char-after) ?2))
    ;; Past the last cell, navigation leaves the table.
    (agent-shell-next-item)
    (should (looking-at-p "after"))))

(ert-deftest agent-shell-next-item-reaches-a-link-sitting-mid-cell ()
  "Inside a table, a cell's link is an item of its own.

Otherwise a link that doesn't start its cell is unreachable: navigation
would step over it to the next cell, leaving nothing to press RET on."
  (agent-shell-tests--with-rendered-shell
      "| A | B |
|---|---|
| 1 | see [docs](https://docs.example.com) |

Then [after](https://after.example.com)
"
    (should (search-forward "see"))
    (goto-char (match-beginning 0))
    (agent-shell-next-item)
    (should (looking-at-p "docs"))
    ;; Past the table's last item, navigation leaves it.
    (agent-shell-next-item)
    (should (looking-at-p "after"))
    (agent-shell-previous-item)
    (should (eq (char-after) ?A))))

(ert-deftest agent-shell-next-item-leaves-a-table-with-a-prefix ()
  "A prefix carries navigation past the table point is in.

Only from inside one: outside, a table is entered deliberately, on its
first cell, so there's nothing for the prefix to leave."
  (agent-shell-tests--with-rendered-shell
      "Intro [before](https://before.example.com)

| A | B |
|---|---|
| 1 | 2 |

Then [after](https://after.example.com)
"
    ;; From the first cell, rather than walking B, 1 and 2.
    (should (search-forward "A"))
    (goto-char (match-beginning 0))
    (agent-shell-next-item t)
    (should (looking-at-p "after"))
    (agent-shell-previous-item)
    (should (eq (char-after) ?A))
    (agent-shell-previous-item t)
    (should (looking-at-p "before"))
    ;; Outside a table the prefix changes nothing.
    (agent-shell-next-item t)
    (should (eq (char-after) ?A))))

(ert-deftest agent-shell-next-item-enters-a-table-ahead-of-its-links ()
  "A link in a cell is reached by walking the table, not entered at directly."
  (agent-shell-tests--with-rendered-shell
      "| A | B |
|---|---|
| 1 | [two](https://two.com/x) |
"
    (agent-shell-next-item)
    (should (eq (char-after) ?A))
    (agent-shell-next-item)
    (should (eq (char-after) ?B))
    (agent-shell-next-item)
    (should (eq (char-after) ?1))
    (agent-shell-next-item)
    (should (looking-at-p "two"))))

(ert-deftest agent-shell-table-cells-claim-no-keys-of-their-own ()
  "A rendered table leaves the buffer's keys alone.

Cell navigation used to come from a keymap text property, which took
precedence over `agent-shell-mode-map' and so kept TAB from ever
reaching item navigation."
  (agent-shell-tests--with-rendered-shell
      "| A | [docs](https://example.com) |
|---|---|
| 1 | 2 |
"
    (use-local-map agent-shell-mode-map)
    (should (search-forward "A"))
    (goto-char (match-beginning 0))
    (should-not (get-text-property (point) 'keymap))
    (should (eq #'agent-shell-next-item (key-binding (kbd "TAB"))))
    ;; A link in a cell still keeps its own.
    (should (search-forward "docs"))
    (goto-char (match-beginning 0))
    (should (get-text-property (point) 'keymap))))

(ert-deftest agent-shell-backward-up-item-returns-to-a-table-s-first-cell ()
  (agent-shell-tests--with-rendered-shell
      "| A | B |
|---|---|
| 1 | 2 |
"
    (should (search-forward "A"))
    (let ((first-cell (match-beginning 0)))
      (should (search-forward "2"))
      (goto-char (match-beginning 0))
      (agent-shell-backward-up-item)
      (should (eq first-cell (point))))
    ;; Outside a table the key keeps its usual meaning, which at top
    ;; level is `backward-up-list' reporting there's nothing to go up to.
    (goto-char (point-max))
    (should-error (agent-shell-backward-up-item) :type 'user-error)))

(ert-deftest agent-shell-previous-item-enters-a-table-at-its-first-cell ()
  "Previous item enters the table above at its first cell, then leaves it."
  (agent-shell-tests--with-rendered-shell
      "Intro [before](https://before.com/x)

| A | B |
|---|---|
| 1 | 2 |

Then [after](https://after.com/y)
"
    (should (search-forward "after"))
    (goto-char (match-beginning 0))
    (agent-shell-previous-item)
    (should (eq (char-after) ?A))
    ;; Ahead of the first cell there's nowhere left in the table to go,
    ;; so navigation leaves it.
    (agent-shell-previous-item)
    (should (looking-at-p "before"))))

(ert-deftest agent-shell-previous-item-walks-cells-in-reverse ()
  "Previous item walks back through the cells of the table point is in."
  (agent-shell-tests--with-rendered-shell
      "| A | B |
|---|---|
| 1 | 2 |
"
    (should (search-forward "2"))
    (goto-char (match-beginning 0))
    (agent-shell-previous-item)
    (should (eq (char-after) ?1))
    (agent-shell-previous-item)
    (should (eq (char-after) ?B))
    (agent-shell-previous-item)
    (should (eq (char-after) ?A))))

(provide 'agent-shell-tests)
;;; agent-shell-tests.el ends here
