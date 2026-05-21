;;; agent-shell-markdown-c.el -*- lexical-binding: t -*-

(require 'org-faces)
(require 'url-parse)
(require 'url-util)

(defgroup agent-shell-markdown-c nil
  "Render Markdown text into propertized form (rust backend)."
  :group 'text)

(defface agent-shell-markdown-bold
  '((t :inherit bold))
  "Face for bold text."
  :group 'agent-shell-markdown-c)

(defface agent-shell-markdown-italic
  '((t :inherit italic))
  "Face for italic text."
  :group 'agent-shell-markdown-c)

(defface agent-shell-markdown-strikethrough
  '((t :strike-through t))
  "Face for strikethrough text."
  :group 'agent-shell-markdown-c)

(defface agent-shell-markdown-inline-code
  '((t :inherit font-lock-doc-markup-face))
  "Face for inline code."
  :group 'agent-shell-markdown-c)

(defface agent-shell-markdown-link
  '((t :inherit link))
  "Face for link titles."
  :group 'agent-shell-markdown-c)

(defface agent-shell-markdown-header-1
  '((t :inherit org-level-1))
  "Face for level-1 headers."
  :group 'agent-shell-markdown-c)

(defface agent-shell-markdown-header-2
  '((t :inherit org-level-2))
  "Face for level-2 headers."
  :group 'agent-shell-markdown-c)

(defface agent-shell-markdown-header-3
  '((t :inherit org-level-3))
  "Face for level-3 headers."
  :group 'agent-shell-markdown-c)

(defface agent-shell-markdown-header-4
  '((t :inherit org-level-4))
  "Face for level-4 headers."
  :group 'agent-shell-markdown-c)

(defface agent-shell-markdown-header-5
  '((t :inherit org-level-5))
  "Face for level-5 headers."
  :group 'agent-shell-markdown-c)

(defface agent-shell-markdown-header-6
  '((t :inherit org-level-6))
  "Face for level-6 headers."
  :group 'agent-shell-markdown-c)

(defface agent-shell-markdown-source-block
  '((t :inherit lazy-highlight :extend t))
  "Background face for fenced source-block bodies."
  :group 'agent-shell-markdown-c)

(defvar agent-shell-markdown-image-max-width 0.4
  "Maximum width for inline images.")

(defun agent-shell-markdown-c-convert (markdown)
  "Convert MARKDOWN string into propertized text."
  (with-temp-buffer
    (insert markdown)
    (agent-shell-markdown-c-replace-markup)
    (buffer-string)))

(defvar drop-point 0) (setq drop-point 0)

(defun agent-shell-markdown-c-replace-markup ()
  "Apply propertization to current buffer using aucmd4cw.
Formatting delimiters (*, _, ~~, etc.) are left in place."
  (save-excursion
    (let* ((markdown (buffer-string))
           (result (pulldown-cmark-emacs-parse markdown))
           (spans (drop drop-point result))
           (inhibit-modification-hooks t))
      (setq drop-point (1- (length result)))
      (dolist (span spans) (agent-shell-markdown-c--apply-span span markdown))
      (agent-shell-markdown--mirror-face-to-font-lock-face
       (point-min) (point-max)))))

(defun agent-shell-markdown-c--apply-span (span markdown)
  "Apply properties for SPAN to the current buffer.
MARKDOWN is the source string used for URL extraction."
  (let ((type (plist-get span :type))
        (start (plist-get span :start))
        (end (plist-get span :end)))
    (when (< start end)
      (pcase type
        (:strong (agent-shell-markdown-c--apply-face start end 'agent-shell-markdown-bold))
        (:emphasis (agent-shell-markdown-c--apply-face start end 'agent-shell-markdown-italic))
        (:strikethrough (agent-shell-markdown-c--apply-face start end 'agent-shell-markdown-strikethrough))
        (:code (agent-shell-markdown-c--apply-face start end 'agent-shell-markdown-inline-code))
        (:code-block (agent-shell-markdown-c--apply-face start end 'agent-shell-markdown-inline-code))
        (:heading (agent-shell-markdown-c--apply-heading start end))
        (:link (agent-shell-markdown-c--apply-link start end markdown))
        (:image (agent-shell-markdown-c--apply-image start end markdown))
        (:hr (agent-shell-markdown-c--apply-divider start end))
        ;; (:code-block (agent-shell-markdown-c--apply-source-block start end))
        ((or :paragraph :blockquote :unordered-list :ordered-list
             :list-item :table :table-head :table-body :table-row
             :table-header :table-cell)
         nil)))))

(defun agent-shell-markdown-c--buf-pos (pos)
  "Convert 0-indexed POS to buffer position, clamped to accessible region."
  (max (point-min) (min (point-max) (+ (point-min) pos))))

(defun agent-shell-markdown-c--apply-face (start end face)
  "Apply FACE to buffer range [START, END).
START and END are 0-indexed positions from aucmd4cw-parse."
  (let ((buf-start (agent-shell-markdown-c--buf-pos start))
        (buf-end (agent-shell-markdown-c--buf-pos end)))
    (when (< buf-start buf-end)
      (add-face-text-property buf-start buf-end face t (current-buffer))
      (put-text-property buf-start buf-end 'font-lock-face face))))

(defun agent-shell-markdown-c--apply-heading (start end)
  "Apply heading face using positions from aucmd4cw-parse."
  (let ((buf-start (agent-shell-markdown-c--buf-pos start))
        (buf-end (agent-shell-markdown-c--buf-pos end))
        (level (save-excursion
                 (goto-char (agent-shell-markdown-c--buf-pos start))
                 (skip-chars-forward "#"))))
    (add-face-text-property
     buf-start buf-end
     (intern (format "agent-shell-markdown-header-%d"
                     (max 1 (min level 6))))
     nil (current-buffer))))

(defun agent-shell-markdown-c--apply-link (start end markdown)
  "Apply link face and keymap at [START, END)."
  (let ((buf-start (agent-shell-markdown-c--buf-pos start))
        (buf-end (agent-shell-markdown-c--buf-pos end))
        (url (agent-shell-markdown-c--extract-url markdown start)))
    (add-face-text-property buf-start buf-end
                            'agent-shell-markdown-link nil (current-buffer))
    (when url
      (let ((map (agent-shell-markdown-c--make-ret-binding-map
                  (lambda () (interactive)
                    (agent-shell-markdown-c--open-link url)))))
        (put-text-property buf-start buf-end 'keymap map (current-buffer))
        (put-text-property buf-start buf-end
                           'mouse-face 'highlight (current-buffer))))))

(defun agent-shell-markdown-c--extract-url (markdown title-start)
  "Extract URL from markdown link surrounding TITLE-START."
  (save-match-data
    (let* ((i (1- title-start))
           (open-bracket (when (>= i 0)
                           (if (eq (aref markdown i) ?\[)
                               i
                             (when (and (> i 0)
                                        (eq (aref markdown (1- i)) ?\[))
                               (1- i))))))
      (when open-bracket
        (let ((close-bracket (cl-loop for j from (1+ open-bracket)
                                      while (< j (length markdown))
                                      when (eq (aref markdown j) ?\])
                                      return j)))
          (when (and close-bracket
                     (< (1+ close-bracket) (length markdown))
                     (eq (aref markdown (1+ close-bracket)) ?\())
            (let ((paren-start (1+ close-bracket)))
              (cl-loop for j from (1+ paren-start)
                       while (< j (length markdown))
                       when (eq (aref markdown j) ?\))
                       return (substring markdown paren-start j)))))))))

(defun agent-shell-markdown-c--apply-image (start end markdown)
  "Render image at [START, END) using display property."
  (let ((url (agent-shell-markdown-c--extract-url markdown start)))
    (when-let ((path (and url
                          (agent-shell-markdown-c--resolve-image-url url)))
               ((image-supported-file-p path))
               ((display-graphic-p)))
      (let ((buf-start (agent-shell-markdown-c--buf-pos start))
            (buf-end (agent-shell-markdown-c--buf-pos end))
            (placeholder (let ((alt (substring markdown start end)))
                            (if (string-empty-p alt) " " alt)))
            (image (create-image path nil nil
                                 :max-width
                                 (agent-shell-markdown-c--image-max-width))))
        (image-flush image)
        (delete-region buf-start buf-end)
        (goto-char buf-start)
        (insert placeholder)
        (let ((insert-end (point)))
          (put-text-property buf-start insert-end 'display image)
          (put-text-property buf-start insert-end 'keymap
                             (agent-shell-markdown-c--make-ret-binding-map
                              (lambda () (interactive) (find-file path))))
          (put-text-property buf-start insert-end
                             'mouse-face 'highlight))))))

(defun agent-shell-markdown-c--apply-divider (start end)
  "Render horizontal rule at [START, END)."
  (let ((buf-start (agent-shell-markdown-c--buf-pos start))
        (buf-end (agent-shell-markdown-c--buf-pos end)))
    (when (< buf-start buf-end)
      (add-text-properties
       buf-start buf-end
       (list 'display
             (concat (propertize (make-string
                                  (agent-shell-markdown-c--display-width) ?\s)
                                 'face '(:underline t))
                     "\n")
             'agent-shell-markdown-frozen t
             'rear-nonsticky '(display agent-shell-markdown-frozen))))))

(defun agent-shell-markdown-c--display-width ()
  "Return usable display width, falling back to 80."
  (or (ignore-errors (window-body-width)) 80))

(defun agent-shell-markdown--mirror-face-to-font-lock-face (start end)
  "Copy each `face' run across [START, END) to `font-lock-face'."
  (let ((pos start))
    (while (< pos end)
      (let ((face (get-text-property pos 'face))
            (next (or (next-single-property-change pos 'face nil end) end)))
        (when face
          (put-text-property pos next 'font-lock-face face))
        (setq pos next)))))

(defun agent-shell-markdown-c--make-ret-binding-map (fun)
  "Return a sparse keymap binding RET and mouse-1 to FUN."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") fun)
    (define-key map [mouse-1] fun)
    (define-key map [remap self-insert-command] 'ignore)
    map))

(defun agent-shell-markdown-c--open-link (url)
  "Open URL.  Use local navigation for file links, browse-url otherwise."
  (unless (agent-shell-markdown-c--open-local-link url)
    (browse-url url)))

(defun agent-shell-markdown-c--open-local-link (url)
  "Open URL as a local file link if possible."
  (when-let ((parsed (agent-shell-markdown-c--parse-local-link url)))
    (find-file (car parsed))
    (when (cdr parsed)
      (goto-char (point-min))
      (forward-line (1- (cdr parsed))))
    t))

(defun agent-shell-markdown-c--parse-local-link (url)
  "Parse URL as a local file link.
Return (FILE . LINE) when URL points to an existing local file,
or nil otherwise."
  (when-let ((match
              (cond
               ((string-match
                 (rx bos "file://"
                     (group (+? anything))
                     (optional (or (seq "#L" (group (one-or-more digit)))
                                   (seq ":" (group (one-or-more digit)))))
                     eos)
                 url)
                (cons (match-string 1 url)
                      (or (match-string 2 url) (match-string 3 url))))
               ((string-match
                 (rx bos "file:"
                     (group (not (any "/")) (+? anything))
                     (optional (or (seq "#L" (group (one-or-more digit)))
                                   (seq ":" (group (one-or-more digit)))))
                     eos)
                 url)
                (cons (match-string 1 url)
                      (or (match-string 2 url) (match-string 3 url))))
               ((string-match
                 (rx bos
                     (group (? (optional "/") alpha ":/")
                            (one-or-more (not (any ":#"))))
                     "#L" (group (one-or-more digit))
                     eos)
                 url)
                (cons (match-string 1 url) (match-string 2 url)))
               ((string-match
                 (rx bos
                     (group (? (optional "/") alpha ":/")
                            (one-or-more (not (any ":#"))))
                     ":" (group (one-or-more digit))
                     eos)
                 url)
                (cons (match-string 1 url) (match-string 2 url)))
               ((not (string-empty-p url))
                (cons url nil))))
              (filepath (expand-file-name (car match))))
    (when (file-exists-p filepath)
      (cons filepath
            (when (cdr match)
              (string-to-number (cdr match)))))))

(defun agent-shell-markdown-c--resolve-image-url (url)
  "Resolve image URL to an absolute local file path, or nil."
  (when-let* ((path (cond
                      ((string-prefix-p "file://" url)
                       (url-unhex-string
                        (url-filename (url-generic-parse-url url))))
                      ((string-prefix-p "file:" url)
                       (substring url (length "file:")))
                      ((or (file-name-absolute-p url)
                           (string-prefix-p "~" url)
                           (string-prefix-p "./" url)
                           (string-prefix-p "../" url))
                       url)))
               (expanded (expand-file-name path))
               ((file-exists-p expanded)))
    expanded))

(defun agent-shell-markdown-c--image-max-width ()
  "Return the maximum image width in pixels."
  (if (floatp agent-shell-markdown-image-max-width)
      (let ((window (or (get-buffer-window (current-buffer))
                        (frame-first-window))))
        (round (* agent-shell-markdown-image-max-width
                  (window-body-width window t))))
    agent-shell-markdown-image-max-width))

(provide 'agent-shell-markdown-c)

;;; agent-shell-markdown-c.el ends here
