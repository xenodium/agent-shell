;;; agent-shell-inline-images.el --- Inline image rendering for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Alvaro Ramirez

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
;; Inline image rendering for agent-shell responses.
;;
;; Support the work https://github.com/sponsors/xenodium

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)

(declare-function agent-shell--load-image "agent-shell")
(declare-function agent-shell--resolve-path "agent-shell")
(declare-function agent-shell-cwd "agent-shell")
(declare-function agent-shell-ui-make-action-keymap "agent-shell-ui")

(defcustom agent-shell-render-inline-images t
  "Whether to render inline images in agent responses."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-inline-image-max-width 400
  "Maximum width in pixels for inline images rendered in responses."
  :type 'integer
  :group 'agent-shell)

(defcustom agent-shell-inline-image-fetch-remote nil
  "Whether to fetch and render remote images linked in responses."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-inline-image-cache-size 64
  "Maximum number of inline images to keep in cache."
  :type 'integer
  :group 'agent-shell)

(defcustom agent-shell-inline-image-download-directory nil
  "Default directory used by `agent-shell-inline-images-copy-to-downloads'.
When nil, prompt for a destination."
  :type 'directory
  :group 'agent-shell)

(defconst agent-shell-inline-images--extensions
  '("png" "jpg" "jpeg" "gif" "webp" "svg")
  "Supported inline image file extensions.")

(defconst agent-shell-inline-images--markdown-image-regexp
  (rx "!" "[" (*? (not (any "]"))) "]"
      "(" (group (+ (not (any "\n)")))) ")")
  "Regexp matching markdown image syntax with a captured URL.")

(defconst agent-shell-inline-images--url-regexp
  (concat "\\(\\(?:https?\\|file\\)://[^][()<>\"' \t\n]+\\."
          (regexp-opt agent-shell-inline-images--extensions)
          "\\(?:\\?[^][()<>\"' \t\n]*\\)?\\)"))

(defconst agent-shell-inline-images--file-regexp
  (concat "\\(\\(?:\\./\\|\\.\\./\\|~/\\|/\\)?[^][()<>\"' \t\n]+\\."
          (regexp-opt agent-shell-inline-images--extensions)
          "\\)"))

(defvar agent-shell-inline-images--cache (make-hash-table :test #'equal)
  "Cache of inline images keyed by URI and size.")

(defvar agent-shell-inline-images--cache-order nil
  "LRU list of cache keys in most-recent-first order.")

(defvar agent-shell-inline-images--pending (make-hash-table :test #'equal)
  "Pending remote image fetch callbacks keyed by cache key.")

(defun agent-shell-inline-images--cache-key (uri max-width)
  "Return a cache key for URI and MAX-WIDTH."
  (list uri max-width))

(defun agent-shell-inline-images--cache-get (key)
  "Return cached image for KEY and refresh LRU order."
  (when-let ((image (gethash key agent-shell-inline-images--cache)))
    (setq agent-shell-inline-images--cache-order
          (cons key (remove key agent-shell-inline-images--cache-order)))
    image))

(defun agent-shell-inline-images--cache-put (key image)
  "Store IMAGE in cache for KEY and prune LRU."
  (puthash key image agent-shell-inline-images--cache)
  (setq agent-shell-inline-images--cache-order
        (cons key (remove key agent-shell-inline-images--cache-order)))
  (agent-shell-inline-images--cache-prune))

(defun agent-shell-inline-images--cache-prune ()
  "Prune cache to `agent-shell-inline-image-cache-size'."
  (while (> (length agent-shell-inline-images--cache-order)
            agent-shell-inline-image-cache-size)
    (let ((key (car (last agent-shell-inline-images--cache-order))))
      (setq agent-shell-inline-images--cache-order
            (butlast agent-shell-inline-images--cache-order))
      (remhash key agent-shell-inline-images--cache))))

(defun agent-shell-inline-images--pending-add (key callback)
  "Register CALLBACK for KEY."
  (puthash key (cons callback (gethash key agent-shell-inline-images--pending))
           agent-shell-inline-images--pending))

(defun agent-shell-inline-images--pending-pop (key)
  "Return and clear callbacks for KEY."
  (let ((callbacks (gethash key agent-shell-inline-images--pending)))
    (remhash key agent-shell-inline-images--pending)
    callbacks))

(defun agent-shell-inline-images--mime-to-image-type (mime-type)
  "Convert MIME-TYPE string to an Emacs image type symbol."
  (pcase (downcase (or mime-type ""))
    ("image/png" 'png)
    ("image/jpeg" 'jpeg)
    ("image/jpg" 'jpeg)
    ("image/gif" 'gif)
    ("image/webp" 'webp)
    ("image/svg+xml" 'svg)
    (_ nil)))

(defun agent-shell-inline-images--mime-to-extension (mime-type)
  "Convert MIME-TYPE to a file extension."
  (pcase (downcase (or mime-type ""))
    ("image/png" "png")
    ("image/jpeg" "jpg")
    ("image/jpg" "jpg")
    ("image/gif" "gif")
    ("image/webp" "webp")
    ("image/svg+xml" "svg")
    (_ "img")))

(defun agent-shell-inline-images--create-image-from-data (data mime-type max-width)
  "Create an image from DATA and MIME-TYPE with MAX-WIDTH."
  (when (and data (display-graphic-p))
    (let ((type (agent-shell-inline-images--mime-to-image-type mime-type)))
      (create-image data type t :max-width max-width))))

(defun agent-shell-inline-images--extract-markdown-target (raw)
  "Extract URL from RAW markdown image target text."
  (let* ((trimmed (string-trim raw))
         (unwrapped (if (and (string-prefix-p "<" trimmed)
                             (string-suffix-p ">" trimmed))
                        (substring trimmed 1 -1)
                      trimmed)))
    (car (split-string unwrapped "[ \t\n]+" t))))

(defun agent-shell-inline-images--sanitize-target (target)
  "Normalize TARGET by trimming punctuation and wrappers."
  (let ((trimmed (string-trim target)))
    (when (and (string-prefix-p "`" trimmed)
               (string-suffix-p "`" trimmed)
               (> (length trimmed) 1))
      (setq trimmed (substring trimmed 1 -1)))
    (when (and (string-prefix-p "<" trimmed)
               (string-suffix-p ">" trimmed)
               (> (length trimmed) 1))
      (setq trimmed (substring trimmed 1 -1)))
    (string-trim-right trimmed "[.,;:!?]+")))

(defun agent-shell-inline-images--file-url-to-path (uri)
  "Convert file URI to a local path."
  (let* ((parsed (url-generic-parse-url uri))
         (path (url-filename parsed)))
    (when (and path (not (string-empty-p path)))
      (url-unhex-string path))))

(defun agent-shell-inline-images--resolve-inline-image-path (path)
  "Resolve PATH as a local file path, returning nil if not found."
  (let* ((expanded (cond
                    ((string-prefix-p "file://" path)
                     (agent-shell-inline-images--file-url-to-path path))
                    ((string-prefix-p "file:" path)
                     (substring path (length "file:")))
                    ((file-name-absolute-p path)
                     path)
                    ((string-prefix-p "~" path)
                     (expand-file-name path))
                    (t
                     (expand-file-name path (agent-shell-cwd)))))
         (resolved (and expanded (agent-shell--resolve-path expanded))))
    (cond
     ((and expanded (file-exists-p expanded)) expanded)
     ((and resolved (file-exists-p resolved)) resolved)
     (t nil))))

(defun agent-shell-inline-images--inline-image-action (uri)
  "Return an interactive action for URI or nil."
  (cond
   ((or (string-prefix-p "http://" uri)
        (string-prefix-p "https://" uri))
    (lambda ()
      (interactive)
      (browse-url uri)))
   ((string-prefix-p "data:image/" uri) nil)
   (t
    (when-let ((path (agent-shell-inline-images--resolve-inline-image-path uri)))
      (lambda ()
        (interactive)
        (find-file path))))))

(defun agent-shell-inline-images--apply-inline-image (start end image uri &optional action on-entered object)
  "Apply IMAGE display properties between START and END.
URI is stored as a text property. If OBJECT is non-nil, apply to that string."
  (when image
    (add-text-properties
     start end
     `(display ,image
               agent-shell-inline-image t
               agent-shell-inline-image-uri ,uri
               pointer hand
               rear-nonsticky t)
     object)
    (when action
      (add-text-properties start end
                           `(keymap ,(agent-shell-ui-make-action-keymap action))
                           object))
    (when on-entered
      (add-text-properties
       start end
       (list 'cursor-sensor-functions
             (list (lambda (_window _old-pos sensor-action)
                     (when (eq sensor-action 'entered)
                       (funcall on-entered)))))
       object))))

(defun agent-shell-inline-images--deliver-remote (key image)
  "Deliver IMAGE to all pending callbacks for KEY."
  (when image
    (agent-shell-inline-images--cache-put key image))
  (dolist (callback (agent-shell-inline-images--pending-pop key))
    (ignore-errors (funcall callback image))))

(defun agent-shell-inline-images--fetch-remote-image (uri max-width callback)
  "Fetch remote image URI asynchronously and call CALLBACK."
  (let* ((key (agent-shell-inline-images--cache-key uri max-width))
         (cached (agent-shell-inline-images--cache-get key)))
    (cond
     (cached (funcall callback cached))
     ((gethash key agent-shell-inline-images--pending)
      (agent-shell-inline-images--pending-add key callback))
     (t
      (agent-shell-inline-images--pending-add key callback)
      (url-retrieve
       uri
       (lambda (status)
         (let ((response (current-buffer)))
           (unwind-protect
               (unless (plist-get status :error)
                 (goto-char (point-min))
                 (when (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
                   (let ((content-type (when (re-search-forward "^Content-Type: \\([^;\r\n]+\\)" nil t)
                                         (downcase (match-string 1)))))
                     (when (re-search-forward "\r?\n\r?\n" nil t)
                       (set-buffer-multibyte nil)
                       (let ((data (buffer-substring-no-properties (point) (point-max))))
                         (when (and content-type (string-prefix-p "image/" content-type))
                           (agent-shell-inline-images--deliver-remote
                            key
                            (agent-shell-inline-images--create-image-from-data
                             data content-type max-width))))))))
             (when (buffer-live-p response)
               (kill-buffer response)))))
       nil t)))))

(defun agent-shell-inline-images--render-uri (start end uri &optional action)
  "Render URI as an image between START and END."
  (let* ((max-width agent-shell-inline-image-max-width)
         (action (or action (agent-shell-inline-images--inline-image-action uri)))
         (buffer (current-buffer))
         (start-marker (copy-marker start))
         (end-marker (copy-marker end)))
    (cond
     ((string-prefix-p "data:image/" uri)
      (when (string-match (rx "data:" (group (+ (not (any ";"))))
                              ";base64," (group (+ any)))
                          uri)
        (let ((mime-type (match-string 1 uri))
              (data (match-string 2 uri)))
          (when-let ((image (agent-shell-inline-images--create-image-from-data
                             (base64-decode-string data) mime-type max-width)))
            (agent-shell-inline-images--apply-inline-image
             start end image uri action (lambda () (message "Press RET to open")))))))
     ((or (string-prefix-p "http://" uri)
          (string-prefix-p "https://" uri))
      (when agent-shell-inline-image-fetch-remote
        (agent-shell-inline-images--fetch-remote-image
         uri max-width
         (lambda (image)
           (when (and image (buffer-live-p buffer))
             (with-current-buffer buffer
               (let ((inhibit-read-only t)
                     (start-pos (marker-position start-marker))
                     (end-pos (marker-position end-marker)))
                 (when (and start-pos end-pos)
                   (agent-shell-inline-images--apply-inline-image
                    start-pos end-pos image uri action
                    (lambda () (message "Press RET to open")))))))))))
     (t
      (when-let* ((path (agent-shell-inline-images--resolve-inline-image-path uri))
                  (image (agent-shell--load-image :file-path path :max-width max-width)))
        (agent-shell-inline-images--apply-inline-image
         start end image uri action (lambda () (message "Press RET to open"))))))))

(defun agent-shell-inline-images--render-markdown-images (start end)
  "Render markdown image syntax between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward agent-shell-inline-images--markdown-image-regexp end t)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0)))
        (unless (or (get-text-property match-start 'agent-shell-inline-image)
                    (get-text-property match-start 'display))
          (when-let* ((target (agent-shell-inline-images--extract-markdown-target
                               (match-string-no-properties 1)))
                      (uri (agent-shell-inline-images--sanitize-target target)))
            (agent-shell-inline-images--render-uri match-start match-end uri)))))))

(defun agent-shell-inline-images--render-image-urls (start end)
  "Render inline image URLs between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward agent-shell-inline-images--url-regexp end t)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (uri (agent-shell-inline-images--sanitize-target
                   (match-string-no-properties 1))))
        (unless (or (get-text-property match-start 'agent-shell-inline-image)
                    (get-text-property match-start 'display))
          (agent-shell-inline-images--render-uri match-start match-end uri))))))

(defun agent-shell-inline-images--render-image-files (start end)
  "Render inline image file paths between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward agent-shell-inline-images--file-regexp end t)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (uri (agent-shell-inline-images--sanitize-target
                   (match-string-no-properties 1))))
        (unless (or (get-text-property match-start 'agent-shell-inline-image)
                    (get-text-property match-start 'display)
                    (string-prefix-p "http://" uri)
                    (string-prefix-p "https://" uri))
          (agent-shell-inline-images--render-uri match-start match-end uri))))))

(defun agent-shell-inline-images--render (range)
  "Render inline images for fragment RANGE."
  (when (and agent-shell-render-inline-images (display-graphic-p))
    (when-let* ((start (map-nested-elt range '(:body :start)))
                (end (map-nested-elt range '(:body :end))))
      (let ((inhibit-read-only t))
        (agent-shell-inline-images--render-markdown-images start end)
        (agent-shell-inline-images--render-image-urls start end)
        (agent-shell-inline-images--render-image-files start end)))))

(defun agent-shell-inline-images--content-block-to-plain-text (block)
  "Return plain text for content BLOCK."
  (pcase (map-elt block 'type)
    ("text" (or (map-elt block 'text) ""))
    ("image" (if-let ((uri (map-elt block 'uri)))
                 (format "\n[image: %s]\n" uri)
               "\n[image]\n"))
    (_ (or (map-elt block 'text) ""))))

(defun agent-shell-inline-images--content-block-to-display (block)
  "Return displayable text for content BLOCK."
  (pcase (map-elt block 'type)
    ("text" (or (map-elt block 'text) ""))
    ("image"
     (let* ((mime-type (map-elt block 'mimeType))
            (data (map-elt block 'data))
            (uri (map-elt block 'uri))
            (text (propertize (if uri uri "[image]")
                              'rear-nonsticky t
                              'agent-shell-inline-image-uri uri)))
       (cond
        ((and agent-shell-render-inline-images data mime-type)
         (when-let ((image (ignore-errors
                             (agent-shell-inline-images--create-image-from-data
                              (base64-decode-string data)
                              mime-type
                              agent-shell-inline-image-max-width))))
           (agent-shell-inline-images--apply-inline-image
            0 (length text) image (or uri "[image]")
            (agent-shell-inline-images--inline-image-action (or uri ""))
            (lambda () (message "Press RET to open"))
            text))
         (concat "\n" text "\n"))
        (uri (concat "\n" uri "\n"))
        (t "\n[image]\n"))))
    (_ (or (map-elt block 'text) ""))))

(defun agent-shell-inline-images--content-to-plain-text (content)
  "Normalize CONTENT to plain text."
  (cond
   ((stringp content) content)
   ((and (listp content)
         (or (assq 'type content) (assq 'text content)))
    (agent-shell-inline-images--content-block-to-plain-text content))
   ((vectorp content)
    (apply #'concat (mapcar #'agent-shell-inline-images--content-block-to-plain-text
                            (append content nil))))
   ((listp content)
    (apply #'concat (mapcar #'agent-shell-inline-images--content-block-to-plain-text content)))
   ((map-elt content 'text) (map-elt content 'text))
   (t "")))

(defun agent-shell-inline-images--content-to-display (content)
  "Normalize CONTENT to displayable text."
  (cond
   ((stringp content) content)
   ((and (listp content)
         (or (assq 'type content) (assq 'text content)))
    (agent-shell-inline-images--content-block-to-display content))
   ((vectorp content)
    (apply #'concat (mapcar #'agent-shell-inline-images--content-block-to-display
                            (append content nil))))
   ((listp content)
    (apply #'concat (mapcar #'agent-shell-inline-images--content-block-to-display content)))
   ((map-elt content 'text) (map-elt content 'text))
   (t "")))

(defun agent-shell-inline-images--match-at-point (regexp)
  "Return match group 1 for REGEXP covering point, or nil."
  (let ((pos (point))
        (result nil))
    (save-excursion
      (beginning-of-line)
      (while (and (not result)
                  (re-search-forward regexp (line-end-position) t))
        (when (and (<= (match-beginning 0) pos)
                   (<= pos (match-end 0)))
          (setq result (match-string-no-properties 1)))))
    result))

(defun agent-shell-inline-images--org-link-at-point ()
  "Return org link target at point, or nil."
  (when (and (fboundp 'org-in-regexp)
             (boundp 'org-link-any-re)
             (org-in-regexp org-link-any-re 1)
             (fboundp 'org-element-context))
    (let* ((link (org-element-context))
           (type (org-element-property :type link))
           (path (org-element-property :path link)))
      (when (and (eq (org-element-type link) 'link) path)
        (if (and type (not (string= type "file")))
            (concat type ":" path)
          path)))))

(defun agent-shell-inline-images--uri-at-point ()
  "Return an image URI at point, if any."
  (or (get-text-property (point) 'agent-shell-inline-image-uri)
      (let ((overlay-uri (cl-find-if
                          (lambda (ov) (overlay-get ov 'agent-shell-inline-image-uri))
                          (overlays-at (point)))))
        (when overlay-uri
          (overlay-get overlay-uri 'agent-shell-inline-image-uri)))
      (agent-shell-inline-images--org-link-at-point)
      (when-let ((raw (agent-shell-inline-images--match-at-point
                       agent-shell-inline-images--markdown-image-regexp)))
        (agent-shell-inline-images--sanitize-target
         (agent-shell-inline-images--extract-markdown-target raw)))
      (when-let ((raw (agent-shell-inline-images--match-at-point
                       agent-shell-inline-images--url-regexp)))
        (agent-shell-inline-images--sanitize-target raw))
      (when-let ((raw (agent-shell-inline-images--match-at-point
                       agent-shell-inline-images--file-regexp)))
        (agent-shell-inline-images--sanitize-target raw))))

(defun agent-shell-inline-images--filename-from-uri (uri)
  "Derive a filename from URI."
  (cond
   ((string-prefix-p "data:image/" uri)
    (format "image-%s.%s"
            (format-time-string "%Y%m%d-%H%M%S")
            (agent-shell-inline-images--mime-to-extension
             (when (string-match (rx "data:" (group (+ (not (any ";")))) ";base64,") uri)
               (match-string 1 uri)))))
   ((or (string-prefix-p "http://" uri)
        (string-prefix-p "https://" uri)
        (string-prefix-p "file://" uri))
    (let* ((parsed (url-generic-parse-url uri))
           (path (or (url-filename parsed) "")))
      (setq path (car (split-string path "?" t)))
      (let ((name (file-name-nondirectory path)))
        (if (string-empty-p name)
            (format "image-%s" (format-time-string "%Y%m%d-%H%M%S"))
          name))))
   (t
    (let ((name (file-name-nondirectory uri)))
      (if (string-empty-p name)
          (format "image-%s" (format-time-string "%Y%m%d-%H%M%S"))
        name)))))

(defun agent-shell-inline-images--copy-uri-to-downloads (uri dest-dir)
  "Copy image at URI to DEST-DIR."
  (let* ((dest-dir (expand-file-name dest-dir))
         (file-name (agent-shell-inline-images--filename-from-uri uri))
         (dest-path (expand-file-name file-name dest-dir)))
    (make-directory dest-dir t)
    (cond
     ((string-prefix-p "data:image/" uri)
      (when (string-match (rx "data:" (group (+ (not (any ";"))))
                              ";base64," (group (+ any)))
                          uri)
        (let ((data (match-string 2 uri)))
          (with-temp-file dest-path
            (set-buffer-multibyte nil)
            (insert (base64-decode-string data))))))
     ((or (string-prefix-p "http://" uri)
          (string-prefix-p "https://" uri))
      (url-copy-file uri dest-path t))
     (t
      (when-let ((path (agent-shell-inline-images--resolve-inline-image-path uri)))
        (copy-file path dest-path t))))
    (message "Copied image to: %s" dest-path)))

;;;###autoload
(defun agent-shell-inline-images-copy-to-downloads ()
  "Copy the image at point to a destination directory."
  (interactive)
  (if-let ((uri (agent-shell-inline-images--uri-at-point)))
      (let ((dest-dir (or agent-shell-inline-image-download-directory
                          (read-directory-name "Save image to directory: "
                                               (expand-file-name "~/Downloads/") nil t))))
        (agent-shell-inline-images--copy-uri-to-downloads uri dest-dir))
    (user-error "No image reference found at point")))

(with-eval-after-load 'agent-shell
  (add-hook 'agent-shell-section-functions #'agent-shell-inline-images--render))

(provide 'agent-shell-inline-images)

;;; agent-shell-inline-images.el ends here
