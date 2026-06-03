;;; agent-shell-markdown.el --- Replace Markdown markup with propertized text -*- lexical-binding: t -*-

;; Copyright (C) 2026 Alvaro Ramirez

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
;; Convert a Markdown string into propertized text:
;;
;;   (agent-shell-markdown-convert "hello **world**")
;;
;; Or rewrite the current buffer in place:
;;
;;   (agent-shell-markdown-replace-markup)
;;
;; Both remove the markup characters and leave behind face text
;; properties.  Supported markup:
;;
;;   bold        `**X**' / `__X__'        face `agent-shell-markdown-bold'
;;   italic      `*X*'   / `_X_'          face `agent-shell-markdown-italic'
;;   strike      `~~X~~'                  face `agent-shell-markdown-strikethrough'
;;   header      `# X' .. `###### X'      face `agent-shell-markdown-header-1' .. `-6'
;;   inline code `` `X` ``                face `agent-shell-markdown-inline-code'
;;   link        `[title](url)'           face `agent-shell-markdown-link', keymap opens URL
;;   image       `![alt](url)'            `display' property carries image
;;   image path  bare image path on a line  same as `![alt](url)' (no markup)
;;   divider     `---' / `***' / `___'    rendered as an underlined rule line
;;   fenced code ```LANG\nX\n```          body syntax-highlighted via LANG mode
;;   tables      `| A | B |' grid rows    rendered with aligned columns,
;;                                         unicode borders, header/zebra rows
;;                                         and wrap-to-window-width support
;;
;; All agent-shell-markdown-* faces inherit from the conventional faces
;; (`bold', `italic', `org-level-N', etc.) so default rendering is
;; unchanged, while still letting users customize markdown output
;; without disturbing the source faces elsewhere.
;;
;; Open / streaming fenced blocks (no closing fence yet) are
;; left alone so their contents stay protected as the buffer
;; grows.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)
(require 'org-faces)
(require 'url-parse)
(require 'url-util)

(defgroup agent-shell-markdown nil
  "Render Markdown text into propertized form."
  :group 'text)

(defface agent-shell-markdown-bold
  '((t :inherit bold))
  "Face for bold text rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-italic
  '((t :inherit italic))
  "Face for italic text rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-strikethrough
  '((t :strike-through t))
  "Face for strikethrough text rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-inline-code
  '((t :inherit font-lock-doc-markup-face))
  "Face for inline code rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-link
  '((t :inherit link))
  "Face for link titles rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-blockquote
  '((t :inherit font-lock-comment-face))
  "Face for blockquoted text rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-header-1
  '((t :inherit org-level-1))
  "Face for level-1 headers rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-header-2
  '((t :inherit org-level-2))
  "Face for level-2 headers rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-header-3
  '((t :inherit org-level-3))
  "Face for level-3 headers rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-header-4
  '((t :inherit org-level-4))
  "Face for level-4 headers rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-header-5
  '((t :inherit org-level-5))
  "Face for level-5 headers rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-header-6
  '((t :inherit org-level-6))
  "Face for level-6 headers rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-table-header
  '((t :inherit bold))
  "Face for table header row content."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-table-border
  '((t :inherit font-lock-comment-face))
  "Face for table borders (pipes and dashes)."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-table-zebra
  '((t :inherit lazy-highlight))
  "Face for alternating (zebra) data rows in tables."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-source-block
  '((t :inherit lazy-highlight :extend t))
  "Background face applied to rendered fenced source-block bodies.
`:extend t' makes the background color reach the right edge of
the window, so the block reads as a contiguous panel rather than
a per-char highlight."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-source-block-language
  '((t :inherit (italic font-lock-type-face agent-shell-markdown-source-block)))
  "Face for the language label shown above a fenced source block."
  :group 'agent-shell-markdown)

(defvar agent-shell-markdown-image-max-width 0.4
  "Maximum width for inline images rendered from `![alt](url)'.
An integer is taken as pixels.  A float between 0 and 1 is a
ratio of the window body width.")

(defvar agent-shell-markdown-prettify-tables t
  "When non-nil, render markdown tables with aligned columns.")

(defvar agent-shell-markdown-table-use-unicode-borders t
  "When non-nil, use Unicode box-drawing chars (│ ─ ┼ ├ ┤) for borders.
When nil, fall back to ASCII pipes and dashes.")

(defvar agent-shell-markdown-table-wrap-columns t
  "When non-nil, wrap table columns to fit within window width.")

(defvar agent-shell-markdown-table-max-width-fraction 0.9
  "Fraction of window width to use as max table width when wrapping.")

(defvar agent-shell-markdown-table-zebra-stripe t
  "When non-nil, alternate row backgrounds in tables for readability.")

(defvar agent-shell-markdown-language-mapping
  '(("elisp" . "emacs-lisp")
    ("objective-c" . "objc")
    ("objectivec" . "objc")
    ("cpp" . "c++"))
  "Map of fenced-block language aliases to Emacs major mode prefixes.
Keys are lower-case language names as written after the opening
backticks; values are the corresponding Emacs mode prefix (the
`-mode' suffix is appended internally).  Example:

  (\"elisp\" . \"emacs-lisp\")  ; ```elisp -> emacs-lisp-mode")

(cl-defun agent-shell-markdown-convert (markdown)
  "Convert MARKDOWN string into propertized text.

Bold, italic, strikethrough, headers, and inline code are
rendered as text properties on the inner text; the markup
characters are removed.  See `agent-shell-markdown-replace-markup' for
the in-buffer equivalent.

For example:

  (agent-shell-markdown-convert \"_my_ **text**\")
  => #(\"my text\" 0 2 (face italic) 3 7 (face bold))"
  (with-temp-buffer
    (insert markdown)
    (agent-shell-markdown-replace-markup)
    (buffer-string)))

(cl-defun agent-shell-markdown-replace-markup (&key force
                                                    (render-images t)
                                                    (highlight-blocks t))
  "Replace Markdown markup in current buffer with propertized text.

Rewrites the buffer in place: markup characters are removed and
the remaining text carries face properties.  Faces compose, so a
span nested inside another type ends up with all applicable
faces.

Markup inside fenced code blocks and inline code spans is left
alone.  Streaming-friendly: an unclosed fence protects the rest
of the buffer, an unclosed inline backtick protects the rest of
its line, and incomplete bold/italic/strike spans are skipped
until their closing delimiter arrives.

Italic, bold, and strike passes loop until a full round makes no
changes, so adjacent delimiters peel one layer per round
(e.g. `**_X_**' resolves in two rounds).  Headers, inline code,
links, images, bare image-path lines, dividers, source-block
styling, and table styling run once after the loop.

The buffer is narrowed to the streaming watermark for the
duration of the passes — content before the watermark is already
rendered and stable, so every regex / property scan starts there
instead of `point-min'.  The watermark is read off the
`agent-shell-markdown-watermark' text property on the first
character and re-stamped at the end of the call.  Pass FORCE
non-nil to drop the watermark and re-render the whole buffer
(useful after mid-buffer edits, or for tests).

RENDER-IMAGES, when non-nil (the default), replaces `![alt](url)'
markup with displayed images where the URL resolves to an image
file; nil leaves the markup as-is.  HIGHLIGHT-BLOCKS, when non-nil
(the default), runs the fenced-block body through the language's
major-mode font-lock to colour keywords / strings / etc.; nil
strips the fences and inserts the action label but leaves the
body un-fontified."
  (save-excursion
    (when force
      (with-silent-modifications
        (remove-text-properties (point-min) (point-max)
                                '(agent-shell-markdown-watermark nil))))
    (let ((watermark (agent-shell-markdown--watermark-start)))
      (save-restriction
        (narrow-to-region watermark (point-max))
        (let* ((source-ranges (agent-shell-markdown--sort-ranges
                               (agent-shell-markdown--make-markers
                                (agent-shell-markdown--source-block-ranges))))
               (rendered-ranges (agent-shell-markdown--make-markers
                                 (agent-shell-markdown--frozen-ranges)))
               (inline-ranges (agent-shell-markdown--make-markers
                               (agent-shell-markdown--inline-code-ranges
                                :avoid-ranges (agent-shell-markdown--sort-ranges
                                               source-ranges rendered-ranges))))
               (avoid-ranges (agent-shell-markdown--sort-ranges
                              source-ranges rendered-ranges inline-ranges)))
          (while (let ((italic-changed (agent-shell-markdown--replace-italics
                                        :avoid-ranges avoid-ranges))
                       (bold-changed (agent-shell-markdown--replace-bolds
                                      :avoid-ranges avoid-ranges))
                       (strike-changed (agent-shell-markdown--replace-strikethroughs
                                        :avoid-ranges avoid-ranges)))
                   (or italic-changed bold-changed strike-changed)))
          (agent-shell-markdown--replace-headers :avoid-ranges avoid-ranges)
          (agent-shell-markdown--style-inline-code :avoid-ranges source-ranges)
          (agent-shell-markdown--replace-links :avoid-ranges avoid-ranges)
          (when render-images
            (agent-shell-markdown--replace-images :avoid-ranges avoid-ranges)
            (agent-shell-markdown--replace-image-file-paths
             :avoid-ranges avoid-ranges))
          (agent-shell-markdown--style-dividers :avoid-ranges avoid-ranges)
          (agent-shell-markdown--style-blockquotes :avoid-ranges avoid-ranges)
          (agent-shell-markdown--style-source-blocks
           :highlight-blocks highlight-blocks)
          ;; Tables run last so cell content has already been processed by
          ;; every other pass (bold, italic, links, inline code, etc.).
          ;; The cell parser respects face and `agent-shell-markdown-frozen'
          ;; so it doesn't mis-split on pipes that got swallowed by other
          ;; markup.  AVOID-RANGES protects content inside still-open
          ;; fenced blocks (where the closing fence hasn't streamed in
          ;; yet) — without it a table inside a code block would render
          ;; eagerly and the fences would then strip out, leaving a
          ;; rendered table.  Watermark backs off past any rendered
          ;; table whose extension is still possible (see
          ;; `--set-watermark'), so `--find-tables' under the narrow
          ;; always sees the existing `agent-shell-markdown-table-source'
          ;; needed to fold new rows in.
          (agent-shell-markdown--style-tables :avoid-ranges source-ranges)
          ;; Mirror every `face' we composed onto `font-lock-face' so our
          ;; styling survives `font-lock-mode' re-fontification — comint
          ;; / shell-maker / agent-shell buffers fontify on every output
          ;; chunk and would otherwise clear our `face' properties.
          (agent-shell-markdown--mirror-face-to-font-lock-face
           (point-min) (point-max))
          ;; Tag rendered chars so a yank into another buffer drops the
          ;; styling, display overrides, internal markers, and keymaps
          ;; we layered on — paste should give plain chars, not our
          ;; implementation cruft.
          (put-text-property (point-min) (point-max)
                             'yank-handler
                             (list (lambda (s)
                                     (insert (substring-no-properties s))))))))
    (agent-shell-markdown--set-watermark)))

(cl-defun agent-shell-markdown--replace-bolds (&key avoid-ranges)
  "Replace `**X**' / `__X__' spans in current buffer with bold X.

Markup characters are deleted; remaining inner text carries face
`agent-shell-markdown-bold' layered on top of any existing face
properties.  Spans that fall inside any of AVOID-RANGES are left
untouched.  Returns non-nil if at least one replacement was made.

For example, the buffer \"hello **world**.\" becomes \"hello
world.\" with face `agent-shell-markdown-bold' on \"world\"."
  (let ((case-fold-search nil)
        (changed nil))
    (goto-char (point-min))
    (while (re-search-forward
            (rx (or line-start (syntax whitespace))
                (group
                 (or (seq "**" (group (one-or-more (not (any "\n*")))) "**")
                     (seq "__" (group (one-or-more (not (any "\n_")))) "__")))
                (or (syntax punctuation) (syntax whitespace) line-end))
            nil t)
      (let* ((markup-start (match-beginning 1))
             (markup-end (match-end 1))
             (avoid (agent-shell-markdown--in-avoid-range-p
                     markup-start markup-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (let ((text (buffer-substring
                       (or (match-beginning 2) (match-beginning 3))
                       (or (match-end 2) (match-end 3)))))
            (delete-region markup-start markup-end)
            (goto-char markup-start)
            (insert text)
            (add-face-text-property markup-start
                                    (+ markup-start (length text))
                                    'agent-shell-markdown-bold)
            (setq changed t)))))
    changed))

(cl-defun agent-shell-markdown--replace-italics (&key avoid-ranges)
  "Replace `*X*' / `_X_' spans in current buffer with italic X.

Markup characters are deleted; remaining inner text carries face
`agent-shell-markdown-italic' layered on top of any existing face
properties.  Spans that fall inside any of AVOID-RANGES are left
untouched.  Returns non-nil if at least one replacement was made.

For example, the buffer \"hello *world*.\" becomes \"hello
world.\" with face `agent-shell-markdown-italic' on \"world\"."
  (let ((case-fold-search nil)
        (changed nil))
    (goto-char (point-min))
    (while (re-search-forward
            (rx (or (group (or bol (one-or-more (any "\n \t")))
                           (group "*")
                           (group (one-or-more (not (any "\n*")))) "*")
                    (group (or bol (one-or-more (any "\n \t")))
                           (group "_")
                           (group (one-or-more (not (any "\n_")))) "_")))
            nil t)
      (let* ((markup-start (or (match-beginning 2) (match-beginning 5)))
             (markup-end (match-end 0))
             (avoid (agent-shell-markdown--in-avoid-range-p
                     markup-start markup-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (let ((text (buffer-substring
                       (or (match-beginning 3) (match-beginning 6))
                       (or (match-end 3) (match-end 6)))))
            (delete-region markup-start markup-end)
            (goto-char markup-start)
            (insert text)
            (add-face-text-property markup-start
                                    (+ markup-start (length text))
                                    'agent-shell-markdown-italic)
            (setq changed t)))))
    changed))

(cl-defun agent-shell-markdown--replace-strikethroughs (&key avoid-ranges)
  "Replace `~~X~~' spans in current buffer with strike-through-faced X.

Markup characters are deleted; remaining inner text carries face
`agent-shell-markdown-strikethrough' layered on top of any existing face
properties.  Spans inside any of AVOID-RANGES are left untouched.
Returns non-nil if at least one replacement was made.

For example, the buffer \"a ~~b~~ c\" becomes \"a b c\" with face
`agent-shell-markdown-strikethrough' on \"b\"."
  (let ((case-fold-search nil)
        (changed nil))
    (goto-char (point-min))
    (while (re-search-forward
            (rx "~~" (group (one-or-more (not (any "\n~")))) "~~")
            nil t)
      (let* ((markup-start (match-beginning 0))
             (markup-end (match-end 0))
             (avoid (agent-shell-markdown--in-avoid-range-p
                     markup-start markup-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (let ((text (buffer-substring (match-beginning 1) (match-end 1))))
            (delete-region markup-start markup-end)
            (goto-char markup-start)
            (insert text)
            (add-face-text-property markup-start
                                    (+ markup-start (length text))
                                    'agent-shell-markdown-strikethrough)
            (setq changed t)))))
    changed))

(cl-defun agent-shell-markdown--replace-headers (&key avoid-ranges)
  "Replace `# X' / `## X' / ... headers with X faced as `org-level-N'.

The `#' prefix and one or more separator spaces are stripped; the
title text is left with face `agent-shell-markdown-header-N' where N is
the number of `#' characters clamped to 1..6.  Headers inside any
of AVOID-RANGES are left untouched.

Requires an explicit trailing newline — a header at end-of-buffer
without `\\n' is treated as still streaming and left raw, so a
chunk that lands `# He' followed later by `llo World\\n' renders
the full `Hello World' on the second call rather than eagerly
facing `He' and leaving `llo World' plain.

For example, the buffer \"## My title\\n\" becomes \"My title\\n\"
with face `agent-shell-markdown-header-2' on \"My title\"."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward
            (rx bol (zero-or-more blank) (group (one-or-more "#"))
                (one-or-more blank)
                (group (one-or-more (not (any "\n")))) "\n")
            nil t)
      (let* ((markup-start (match-beginning 0))
             (markup-end (match-end 0))
             (avoid (agent-shell-markdown--in-avoid-range-p
                     markup-start markup-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (let* ((level (- (match-end 1) (match-beginning 1)))
                 (text (buffer-substring (match-beginning 2) (match-end 2)))
                 ;; The trailing `\\n' we re-insert below would otherwise
                 ;; punch a hole in the caller's contiguous block range
                 ;; (eg. `invisible'/`agent-shell-ui-section') and break
                 ;; toggle/replace operations — same hazard called out in
                 ;; `--style-source-blocks'.  Carry over the original
                 ;; newline's caller props.
                 (carried (agent-shell-markdown--carry-properties
                           (1- markup-end))))
            (delete-region markup-start markup-end)
            (goto-char markup-start)
            (insert text "\n")
            (when carried
              (add-text-properties markup-start (point) carried))
            (add-face-text-property markup-start
                                    (+ markup-start (length text))
                                    (intern (format "agent-shell-markdown-header-%d"
                                                    (min (max level 1) 6))))))))))

(cl-defun agent-shell-markdown--style-inline-code (&key avoid-ranges)
  "Strip backticks from complete inline `X` spans and face the body.

The body of each well-formed `` `X` `` is left in place with
face `agent-shell-markdown-inline-code' and tagged with the text
property `agent-shell-markdown-frozen t' so it is never re-processed
on subsequent calls (the body can legitimately contain
markdown-looking chars like `**' once the surrounding backticks
are gone).  Spans inside any of AVOID-RANGES (typically fenced
code blocks) are left untouched.

For example, the buffer \"a `code` b\" becomes \"a code b\" with
face `agent-shell-markdown-inline-code' on \"code\"."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
      (let* ((markup-start (match-beginning 0))
             (markup-end (match-end 0))
             (avoid (agent-shell-markdown--in-avoid-range-p
                     markup-start markup-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (let ((text (buffer-substring (match-beginning 1) (match-end 1))))
            (delete-region markup-start markup-end)
            (goto-char markup-start)
            (insert text)
            (let ((end (+ markup-start (length text))))
              (add-face-text-property markup-start end 'agent-shell-markdown-inline-code)
              (add-text-properties markup-start end
                                   '(agent-shell-markdown-frozen t
                                     rear-nonsticky (agent-shell-markdown-frozen))))))))))

(cl-defun agent-shell-markdown--replace-links (&key avoid-ranges)
  "Replace `[title](url)' markup with title faced as link.

The bracket/parenthesis markup is stripped; the title is left
with face `agent-shell-markdown-link' and a keymap text property that
opens the URL on RET or mouse-1.  Matches preceded by `!' (the
image syntax) are skipped, as are links inside any of
AVOID-RANGES.

For example, the buffer \"see [docs](https://example.com)\"
becomes \"see docs\" with face `agent-shell-markdown-link' on \"docs\"
and a keymap that opens the URL."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward
            (rx "["
                (group (one-or-more (not (any "]"))))
                "]"
                "("
                (group (one-or-more (not (any ")"))))
                ")")
            nil t)
      (let* ((markup-start (match-beginning 0))
             (markup-end (match-end 0))
             (is-image (eq (char-before markup-start) ?!))
             (avoid (unless is-image
                      (agent-shell-markdown--in-avoid-range-p
                       markup-start markup-end avoid-ranges))))
        (cond
         (avoid (goto-char (cdr avoid)))
         (is-image nil)
         (t
          (let ((title (buffer-substring (match-beginning 1) (match-end 1)))
                (url (buffer-substring-no-properties
                      (match-beginning 2) (match-end 2))))
            (delete-region markup-start markup-end)
            (goto-char markup-start)
            (insert title)
            (let ((end (+ markup-start (length title))))
              (add-face-text-property markup-start end 'agent-shell-markdown-link)
              (put-text-property markup-start end 'keymap
                                 (agent-shell-markdown--make-ret-binding-map
                                  (lambda () (interactive)
                                    (agent-shell-markdown--open-link url))))
              (put-text-property markup-start end 'mouse-face 'highlight)))))))))

(cl-defun agent-shell-markdown--replace-images (&key avoid-ranges)
  "Replace `![alt](url)' image markup with displayed images.

If URL resolves to an existing local file that is image-supported
and a graphical display is available, the full markup is replaced
by the alt text (or a single space if alt is empty) carrying a
`display' property with the image and a keymap that opens the
file on RET or mouse-1.  Otherwise the markup is left untouched.
Images inside any of AVOID-RANGES are left alone.

For example, the buffer \"see ![logo](logo.png)\" becomes
\"see logo\" with the image shown in place of \"logo\"."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward
            (rx "!"
                "["
                (group (zero-or-more (not (any "]"))))
                "]"
                "("
                (group (one-or-more (not (any ")"))))
                ")")
            nil t)
      (let* ((markup-start (match-beginning 0))
             (markup-end (match-end 0))
             (avoid (agent-shell-markdown--in-avoid-range-p
                     markup-start markup-end avoid-ranges)))
        (cond
         (avoid (goto-char (cdr avoid)))
         (t
          (let* ((alt (buffer-substring-no-properties
                       (match-beginning 1) (match-end 1)))
                 (url (buffer-substring-no-properties
                       (match-beginning 2) (match-end 2)))
                 (path (agent-shell-markdown--resolve-image-url url)))
            (when (and path
                       (image-supported-file-p path)
                       (display-graphic-p))
              (let ((image (create-image
                            path nil nil
                            :max-width (agent-shell-markdown--image-max-width)))
                    (placeholder (if (string-empty-p alt) " " alt)))
                (image-flush image)
                (delete-region markup-start markup-end)
                (goto-char markup-start)
                (insert placeholder)
                (let ((end (+ markup-start (length placeholder))))
                  (put-text-property markup-start end 'display image)
                  (put-text-property markup-start end 'keymap
                                     (agent-shell-markdown--make-ret-binding-map
                                      (lambda () (interactive)
                                        (find-file path))))
                  (put-text-property markup-start end 'mouse-face 'highlight)))))))))))

(cl-defun agent-shell-markdown--replace-image-file-paths (&key avoid-ranges)
  "Render bare image-path lines as displayed images.

A line that is solely a local path or `file://' URI ending in a
supported image extension is treated like an `![alt](url)' image:
when the path resolves to an existing image-supported file and a
graphical display is available, the line text is left in place
carrying a `display' property with the image and a keymap that
opens the file.  Lines inside any of AVOID-RANGES are left
untouched, as are unresolvable paths.

For example, a buffer line containing just `/abs/path/img.png'
renders the image in place of that text."
  (let* ((case-fold-search t)
         (ext-re (regexp-opt image-file-name-extensions))
         (regex (concat "^[ \t]*\\(\\(?:file://\\|[/~.]\\)[^ \t\n]*\\."
                        ext-re
                        "\\)[ \t]*$")))
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (let* ((line-start (match-beginning 0))
             (line-end (match-end 0))
             (avoid (agent-shell-markdown--in-avoid-range-p
                     line-start line-end avoid-ranges)))
        (cond
         (avoid (goto-char (cdr avoid)))
         (t
          (let* ((path-start (match-beginning 1))
                 (path-end (match-end 1))
                 (raw (buffer-substring-no-properties path-start path-end))
                 (resolved (agent-shell-markdown--resolve-image-url raw)))
            (when (and resolved
                       (image-supported-file-p resolved)
                       (display-graphic-p))
              (let ((image (create-image
                            resolved nil nil
                            :max-width (agent-shell-markdown--image-max-width))))
                (image-flush image)
                (put-text-property path-start path-end 'display image)
                (put-text-property path-start path-end 'keymap
                                   (agent-shell-markdown--make-ret-binding-map
                                    (lambda () (interactive)
                                      (find-file resolved))))
                (put-text-property path-start path-end 'mouse-face 'highlight)
                (add-text-properties path-start path-end
                                     '(agent-shell-markdown-frozen t
                                       rear-nonsticky (agent-shell-markdown-frozen))))))))))))

(cl-defun agent-shell-markdown--style-dividers (&key avoid-ranges)
  "Render `---' / `***' / `___' horizontal-rule lines as styled rules.

Each line consisting of 3+ matching dash/star/underscore chars
(optionally surrounded by spaces or tabs) gets a `display' text
property that draws an underlined rule across the window, plus a
`agent-shell-markdown-frozen' tag so subsequent calls don't re-process
it.  Dividers inside any of AVOID-RANGES are left untouched.

The chars themselves remain in the buffer beneath the display
property, so the source markdown round-trips through copy/save."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward
            (rx bol (zero-or-more blank)
                (or (seq "***" (zero-or-more "*"))
                    (seq "---" (zero-or-more "-"))
                    (seq "___" (zero-or-more "_")))
                (zero-or-more blank) eol)
            nil t)
      (let* ((rule-start (match-beginning 0))
             (rule-end (match-end 0))
             (avoid (agent-shell-markdown--in-avoid-range-p
                     rule-start rule-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (add-text-properties
           rule-start rule-end
           (list 'display
                 (concat (propertize (make-string 12 ?\s)
                                     'face '(:underline t))
                         "\n")
                 'agent-shell-markdown-frozen t
                 'rear-nonsticky '(display agent-shell-markdown-frozen))))))))

(cl-defun agent-shell-markdown--style-blockquotes (&key avoid-ranges)
  "Render `>'-prefixed lines as blockquotes with vertical bars.

Each leading `>' character on the line is shown as `▌' via a
`display' text property; the underlying `>' chars stay in the
buffer so the source markdown round-trips through copy/save and
re-rendering remains idempotent.  Remaining content on the line
gets face `agent-shell-markdown-blockquote' (composes with any
face already applied by an earlier pass — bold/italic/inline-code
inside a blockquote still render).

Multiple nesting levels are supported: each leading `>' renders
as its own bar, so `>> text' shows two bars and `>>> text' three.
Whitespace between `>'s is preserved literally.

Requires an explicit trailing newline — a blockquote line at
end-of-buffer without `\\n' is treated as still streaming and
left raw, matching the header behaviour.

Lines inside any of AVOID-RANGES (e.g. fenced code blocks) are
left untouched."
  (let ((case-fold-search nil)
        (bar (propertize "▌" 'face 'agent-shell-markdown-blockquote)))
    (goto-char (point-min))
    (while (re-search-forward
            (rx bol (zero-or-more blank)
                ">" (zero-or-more (any " \t>"))
                (zero-or-more (not (any "\n"))) "\n")
            nil t)
      (let* ((line-start (match-beginning 0))
             (line-end (match-end 0))
             (avoid (agent-shell-markdown--in-avoid-range-p
                     line-start line-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (save-excursion
            (goto-char line-start)
            (skip-chars-forward " \t" line-end)
            (while (eq (char-after) ?>)
              (put-text-property (point) (1+ (point)) 'display bar)
              (forward-char 1)
              (skip-chars-forward " \t" line-end)))
          (add-face-text-property line-start (1- line-end)
                                  'agent-shell-markdown-blockquote)
          (add-text-properties line-start line-end
                               '(agent-shell-markdown-frozen t
                                 rear-nonsticky (agent-shell-markdown-frozen))))))))

(defun agent-shell-markdown--display-width ()
  "Return a usable display width for divider rendering.
Tries the selected window's body width and falls back to 80
characters when no usable window is available (e.g. batch)."
  (or (ignore-errors (window-body-width))
      80))

(cl-defun agent-shell-markdown--style-source-blocks (&key (highlight-blocks t))
  "Strip fenced code block markup and syntax-highlight the body.

For each complete `\\`\\`\\`LANG' / `\\`\\`\\`' fenced block,
the opening and closing fence lines are deleted from the buffer.
The body text stays in place with face properties from LANG's
major mode (when loadable) and a `agent-shell-markdown-frozen t' text
property tagging it as rendered output.  That tag is read back
as an avoid-range on subsequent calls, so the body is never
re-processed as inline markup even though its surrounding
fences are gone.

Open / streaming fences (no closing line yet) are left alone.

When HIGHLIGHT-BLOCKS is nil, fences are still stripped and the
action label inserted, but the body is left un-fontified (no
language-mode keyword colours).  Useful when the caller wants the
panel layout without paying the syntax-highlighting cost.

For example, the buffer:

  ```elisp
  (message \"hi\")
  ```

becomes:

  (message \"hi\")

with `emacs-lisp-mode' face properties on the body and a
`agent-shell-markdown-frozen' tag covering those same chars."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    ;; Group 2 captures the opening backtick run; `backref' on the
    ;; closer matches the same literal run, so a 4-backtick outer
    ;; fence requires a 4-backtick close — a 3-backtick line inside
    ;; is just body.  Note this is slightly tighter than CommonMark
    ;; (which permits close > open), but every-LLM-I've-seen emits
    ;; matched counts, so the simplification is worth it.
    (while (re-search-forward
            (rx (group bol (zero-or-more blank)
                       (group (>= 3 "`"))
                       (zero-or-more blank)
                       (group (zero-or-more (or alphanumeric "-" "+" "#")))
                       (zero-or-more blank) "\n")
                (group (*? anychar))
                "\n"
                (group bol (zero-or-more blank)
                       (backref 2)
                       (zero-or-more blank) (or "\n" eol)))
            nil t)
      (let* ((open-start (match-beginning 1))
             (open-end (match-end 1))
             (lang (buffer-substring-no-properties (match-beginning 3)
                                                   (match-end 3)))
             (body-start (copy-marker (match-beginning 4)))
             (body-end (copy-marker (match-end 4)))
             (close-start (match-beginning 5))
             (close-end (match-end 5))
             (highlighted (when highlight-blocks
                            (agent-shell-markdown--highlight-code
                             (buffer-substring-no-properties body-start body-end)
                             lang))))
        ;; Delete in reverse position order so earlier offsets stay
        ;; valid; body markers adjust automatically.
        (delete-region close-start close-end)
        (delete-region open-start open-end)
        ;; Seed the bg panel on body chars first, then layer language
        ;; font-lock faces on top — the foreground colors take priority
        ;; per glyph while the `:extend t' background fills the gaps
        ;; and reaches the right edge of the window.  Include the
        ;; trailing `\\n' (the one that sat between body and close
        ;; fence, preserved by the deletes above): `:extend t' only
        ;; extends the background when the face is in effect at
        ;; end-of-line, so without the `\\n' carrying the face the
        ;; last body line's bg would stop at the last content char.
        (let ((body-bg-end (min (1+ (marker-position body-end))
                                (point-max)))
              ;; `line-prefix' / `wrap-prefix' visually inset each
              ;; rendered line: 2 plain cols then 2 bg-tinted cols.
              ;; Copying chars out of the block yanks raw source with
              ;; no leading indentation.  `wrap-prefix' handles long
              ;; lines that wrap.  Splitting the prefix this way keeps
              ;; the panel from running hard to the window's left edge
              ;; while still drawing a clear tinted gutter.
              (prefix (concat "  "
                              (propertize
                               "  " 'face
                               'agent-shell-markdown-source-block))))
          (put-text-property (marker-position body-start) body-bg-end
                             'face 'agent-shell-markdown-source-block)
          (agent-shell-markdown--apply-faces-from highlighted
                                                  (marker-position body-start))
          (add-text-properties (marker-position body-start) body-bg-end
                               `(agent-shell-markdown-frozen t
                                                             agent-shell-non-trimmable t
                                                             rear-nonsticky (agent-shell-markdown-frozen
                                                                             agent-shell-non-trimmable)
                                                             line-prefix ,prefix
                                                             wrap-prefix ,prefix))
          ;; Insert an actionable "LANG ⧉" / "snippet ⧉" label and the
          ;; surrounding panel padding as REAL BUFFER TEXT — no
          ;; `display' properties (which previously caused the body's
          ;; first char to be hidden / clipped, see #597 "Make code
          ;; block label actual buffer text"), no overlays.  Layout
          ;; relative to the original body: `<vpad>\\n<label>\\n\\n
          ;; <body>\\n<vpad>\\n', where each padding `\\n' carries the
          ;; panel bg face so its line renders as a tinted blank line.
          ;; RET or mouse-1 on the label kills the body to the kill
          ;; ring.  `content-start' uses insertion-type t so it stays
          ;; AFTER the inserted prefix, giving the kill-action a
          ;; stable pointer to body content even though `body-start'
          ;; itself collapses to the leading vpad's first char.
          ;; After insertion we carry the body's caller-set properties
          ;; (`invisible', agent-shell-ui block/section markers,
          ;; `read-only', etc.) onto the inserted chars — propertize'd
          ;; inserts ignore stickiness, and without this the inserted
          ;; prefix punches a hole in the caller's contiguous block
          ;; range and breaks toggle/replace operations.
          (let* ((label-text (concat (if (string-empty-p lang) "snippet" lang)
                                     " ⧉"))
                 (content-start (copy-marker (marker-position body-start) t))
                 (kill-action (lambda ()
                                (interactive)
                                ;; Locate the body by text property in
                                ;; the current buffer so copy works in
                                ;; any buffer that received a propertized
                                ;; copy of the rendered block (e.g. the
                                ;; viewport).
                                (when-let* ((start (next-single-property-change
                                                    (point)
                                                    'agent-shell-markdown-source-block-body))
                                            ((get-text-property
                                              start
                                              'agent-shell-markdown-source-block-body))
                                            (end (next-single-property-change
                                                  start
                                                  'agent-shell-markdown-source-block-body)))
                                  (kill-new (buffer-substring-no-properties start end))
                                  (message "Copied"))))
                 (vpad-line (propertize "\n"
                                        'face 'agent-shell-markdown-source-block
                                        'line-prefix prefix
                                        'wrap-prefix prefix
                                        'agent-shell-non-trimmable t
                                        'rear-nonsticky
                                        '(agent-shell-non-trimmable)))
                 (label (propertize
                         label-text
                         'face 'agent-shell-markdown-source-block-language
                         'mouse-face 'highlight
                         'pointer 'hand
                         'keymap (agent-shell-markdown--make-ret-binding-map
                                  kill-action)
                         'cursor-sensor-functions
                         (list (lambda (_window _old-pos sensor-action)
                                 (when (eq sensor-action 'entered)
                                   (message "Press RET to copy"))))
                         'agent-shell-markdown-frozen t
                         'rear-nonsticky '(agent-shell-markdown-frozen)
                         'line-prefix prefix
                         'wrap-prefix prefix))
                 ;; Top vpad `\\n' + label + middle vpad `\\n' + a
                 ;; second `\\n' that becomes the first column of the
                 ;; line carrying body content.
                 (header (concat vpad-line label vpad-line vpad-line))
                 (carried (agent-shell-markdown--carry-properties body-start)))
            (goto-char body-start)
            (insert header)
            (when carried
              (add-text-properties (marker-position body-start)
                                   (marker-position content-start)
                                   carried))
            ;; Tag body content so the label's copy action can locate
            ;; it by text property, survives a propertized copy into
            ;; another buffer (e.g. viewport).
            (put-text-property (marker-position content-start)
                               (marker-position body-end)
                               'agent-shell-markdown-source-block-body t)
            ;; Bottom vpad: insert a single tinted `\\n' AFTER the
            ;; body's trailing newline so the panel ends on a blank
            ;; tinted line below the last body line.  body-end
            ;; (insertion-type nil) stays put across this insert; the
            ;; vpad lives at [body-end, body-end+1) within the buffer.
            (save-excursion
              (when (and (< (marker-position body-end) (point-max))
                         (eq (char-after (marker-position body-end)) ?\n))
                (goto-char (1+ (marker-position body-end)))
                (let ((vpad-start (point)))
                  (insert vpad-line)
                  (when carried
                    (add-text-properties vpad-start (point) carried)))))
            ;; Move point past the body so the outer `re-search-forward'
            ;; loop doesn't backtrack into body content (e.g. shorter
            ;; inner fences inside a wider outer fence).
            (goto-char (marker-position body-end))))))))

(defconst agent-shell-markdown--table-line-regexp
  (rx line-start
      (zero-or-more (any " \t"))
      "|"
      (one-or-more (not (any "\n")))
      "|"
      (zero-or-more (any " \t"))
      line-end)
  "Regexp matching a single line of a markdown table.")

(defconst agent-shell-markdown--table-pending-line-regexp
  (rx line-start (zero-or-more (any " \t")) "|")
  "Lenient regexp matching a line that might still be streaming into
a table row — anything starting with `|' (after optional leading
whitespace).  Used by `--extending-table-start' so the watermark
can back off past a partial separator like `|---|---|----' that
hasn't grown its closing `|' yet.")

(defconst agent-shell-markdown--table-separator-regexp
  (rx line-start
      (zero-or-more (any " \t"))
      "|"
      (one-or-more (or "-" ":" "|" " " "\t"))
      "|"
      (zero-or-more (any " \t"))
      line-end)
  "Regexp matching a table separator row (e.g. `|---|---|').")

(cl-defun agent-shell-markdown--find-tables (&key avoid-ranges)
  "Return tables to (re-)render in current buffer.

Each element is an alist with keys :start, :end (the region to
replace), and :source (the markdown table source — a propertized
string — that should be rendered into that region).

Two flavours of region are collected:

  - Pure ASCII tables: 2 or more consecutive `|...|' lines, not
    in a frozen region.  A `|---|...' separator row is optional
    — when present it splits header from data; when absent all
    rows are rendered as data.

  - Rendered table + extension: a previously-rendered table
    carries its original source on each char via the
    `agent-shell-markdown-table-source' property.  Chars immediately
    after the rendered region are folded back in: characters up
    to the next `\\n' are continuation of the rendered table's
    last source row (i.e. a chunk boundary that split a row mid-
    cell), and any complete `|...|' lines that follow extend the
    table with new rows.  The combined source is stashed and the
    region is re-rendered.

A rendered table with no extension is skipped — re-rendering
unchanged source is a no-op."
  ;; agent-shell tags its body chars with `field output' while the
  ;; `\\n's between rows may not carry the same field value; without
  ;; this binding, `forward-line' / `line-end-position' would stop at
  ;; those field boundaries and silently truncate table rows.
  (let ((inhibit-field-text-motion t)
        (tables '())
        (pos (point-min)))
    (save-excursion
      (while (< pos (point-max))
        (goto-char pos)
        (cond
         ;; Skip past any avoid-range containing POS in one hop —
         ;; otherwise multi-line ranges (open fences, big rendered
         ;; spans) make us walk every line just to fall through.
         ;; Query with `[pos, pos+1)' so a range whose half-open
         ;; exclusive END equals POS doesn't match (would otherwise
         ;; setq POS back to itself → infinite loop).
         ((let ((avoid (agent-shell-markdown--in-avoid-range-p
                        pos (1+ pos) avoid-ranges)))
            (when avoid (setq pos (cdr avoid)) t)))
         ((get-text-property pos 'agent-shell-markdown-table-source)
          (let* ((stashed (get-text-property pos 'agent-shell-markdown-table-source))
                 (rendered-end (or (next-single-property-change
                                    pos 'agent-shell-markdown-table-source
                                    nil (point-max))
                                   (point-max)))
                 (trailing-end rendered-end))
            ;; Scan forward from rendered-end accumulating chars that
            ;; extend the rendered table: first any continuation chars
            ;; on the same physical line (a chunk boundary that split
            ;; a row mid-cell), then complete table rows after the
            ;; next `\n'.  Both kinds end up in one substring that
            ;; `concat'-ing onto STASHED yields valid markdown,
            ;; because the trailing substring's own `\n's handle the
            ;; row boundaries.
            (save-excursion
              (goto-char rendered-end)
              (when (and (< (point) (point-max))
                         (not (eq (char-after) ?\n)))
                (end-of-line)
                (setq trailing-end (point)))
              (when (and (< (point) (point-max))
                         (eq (char-after) ?\n))
                (forward-char 1)
                (while (and (not (eobp))
                            (looking-at agent-shell-markdown--table-line-regexp)
                            (not (get-text-property (point)
                                                    'agent-shell-markdown-frozen))
                            (not (agent-shell-markdown--in-avoid-range-p
                                  (point) (line-end-position) avoid-ranges)))
                  (setq trailing-end (line-end-position))
                  (forward-line 1))))
            (if (> trailing-end rendered-end)
                (let ((combined (concat stashed
                                        (buffer-substring rendered-end
                                                          trailing-end))))
                  (push `((:start . ,pos)
                          (:end . ,trailing-end)
                          (:source . ,combined))
                        tables)
                  (setq pos trailing-end))
              ;; Nothing to fold — re-rendering unchanged source would
              ;; be a no-op, so skip past the rendered region.
              (setq pos rendered-end))))
         ((and (looking-at agent-shell-markdown--table-line-regexp)
               (not (get-text-property pos 'agent-shell-markdown-frozen)))
          (let ((table-start pos)
                (table-end nil)
                (row-count 0))
            ;; Greedily consume rows that match the table regex.  Mid-
            ;; stream chunk boundaries that split a row are handled by
            ;; the streaming-extension branch above, which folds
            ;; continuation chars back into the rendered table's last
            ;; row on the next render.  AVOID-RANGES (e.g. an open
            ;; fenced block whose closing fence hasn't streamed in
            ;; yet) keeps the contained rows raw.
            (while (and (not (eobp))
                        (looking-at agent-shell-markdown--table-line-regexp)
                        (not (get-text-property (point)
                                                'agent-shell-markdown-frozen))
                        (not (agent-shell-markdown--in-avoid-range-p
                              (point) (line-end-position) avoid-ranges)))
              (setq table-end (line-end-position))
              (setq row-count (1+ row-count))
              (forward-line 1))
            ;; >=2 pipe rows is enough to render; a separator
            ;; (`|---|...') is not required.  When present it splits
            ;; header from data (and styles the header).  When absent
            ;; all rows are data.
            (when (>= row-count 2)
              (push `((:start . ,table-start)
                      (:end . ,table-end)
                      (:source . ,(buffer-substring table-start table-end)))
                    tables))
            ;; If we matched table rows, `table-end' is past them.
            ;; Otherwise advance to the next line — the table regex
            ;; needs `bol' to match, so scanning the rest of this line
            ;; char-by-char can never produce a hit.
            (setq pos (or table-end
                          (progn (forward-line 1) (point))))))
         (t
          ;; No table-source here and no table starts at this position.
          ;; The table regex requires `bol', so jump straight to the
          ;; next line start rather than crawling each char.
          (forward-line 1)
          (setq pos (point))))))
    (nreverse tables)))

(defun agent-shell-markdown--parse-table-row (start end)
  "Parse table row from START to END into cells.

Returns a list of alists with :start, :end, :content for each
cell, where :content carries any text properties applied by the
earlier passes (bold, italic, inline-code, link, etc.).

A `|' is treated as a cell separator unless it (a) is preceded by
a `\\' escape, or (b) carries `agent-shell-markdown-frozen' — in which
case it lives inside a region one of our passes has already
rendered (e.g. inline-code body containing a literal `|') and
isn't a real delimiter.  We deliberately don't check `face' so
that pipes faced by external font-lock (markdown-mode, etc.)
are still parsed as cell separators."
  (let ((cells '()))
    (save-excursion
      (goto-char start)
      (when (looking-at (rx (zero-or-more (any " \t")) "|"))
        (goto-char (match-end 0)))
      (let ((cell-start (point)))
        (while (< (point) end)
          (if (re-search-forward (rx (any "|\\")) end t)
              (let ((ch (char-before))
                    (pipe-pos (1- (point))))
                (cond
                 ((and (eq ch ?|)
                       (not (get-text-property pipe-pos
                                               'agent-shell-markdown-frozen)))
                  (let ((cell-end pipe-pos))
                    (push `((:start . ,cell-start)
                            (:end . ,cell-end)
                            (:content . ,(string-trim
                                          (buffer-substring
                                           cell-start cell-end))))
                          cells)
                    (setq cell-start (point))))
                 ((eq ch ?\\)
                  (when (< (point) end) (forward-char 1)))))
            (goto-char end)))))
    (nreverse cells)))

(defvar-local agent-shell-markdown--table-char-pixel-cache nil
  "Cons cell (FONT-WIDTH . SPACE-PIXELS).
Caches the rendered pixel width of a single space in the buffer;
invalidated when the font width changes (e.g. text scaling).
Stored in the destination buffer (the one displayed in the
window passed to the measurement helpers), so cache lookups are
per-destination.")

(defun agent-shell-markdown--table-measure-string (str window)
  "Return real pixel width of STR rendered at point-max of WINDOW's buffer.

Briefly inserts STR, measures with `window-text-pixel-size', and
deletes; `inhibit-modification-hooks' and the modified flag are
preserved so callers never observe the mutation."
  (with-current-buffer (window-buffer window)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (modified (buffer-modified-p))
          real)
      (save-excursion
        (goto-char (point-max))
        (let ((m (point-marker)))
          (set-marker-insertion-type m nil)
          (insert str)
          (setq real (car (window-text-pixel-size window m (point))))
          (delete-region m (point))
          (set-marker m nil)))
      (set-buffer-modified-p modified)
      real)))

(defun agent-shell-markdown--table-char-pixel-width (window)
  "Return real pixel width of a single space in WINDOW, cached.
Cache lives in the destination buffer and is invalidated when
its font width changes."
  (with-current-buffer (window-buffer window)
    (let ((fw (window-font-width window)))
      (if (and agent-shell-markdown--table-char-pixel-cache
               (= fw (car agent-shell-markdown--table-char-pixel-cache)))
          (cdr agent-shell-markdown--table-char-pixel-cache)
        (let ((sw (agent-shell-markdown--table-measure-string " " window)))
          (setq agent-shell-markdown--table-char-pixel-cache (cons fw sw))
          sw)))))

(defvar agent-shell-markdown--table-default-line-height nil
  "Cached default line height in pixels.
Computed once per session by `agent-shell-markdown--table-char-height-scale'.")

(defconst agent-shell-markdown--table-min-height-scale 0.75
  "Minimum height scale factor.
Characters needing more aggressive scaling than this are left
unscaled — shrinking text below 75% makes it unreadable.  This
allows emoji (~0.77) and CJK (~0.90) through while skipping
scripts with tall ascenders/descenders like Arabic (~0.63).")

(defvar agent-shell-markdown--table-height-scale-cache (make-hash-table :test 'eq)
  "Cache of height scale factors keyed by character.")

(defun agent-shell-markdown--table-measure-line-height (win str)
  "Return the rendered pixel height of STR as a single line in WIN."
  (with-temp-buffer
    (set-window-buffer win (current-buffer))
    (insert str "\n")
    (cdr (window-text-pixel-size win 1 3))))

(defun agent-shell-markdown--table-char-height-scale (char)
  "Return the display height scale needed for CHAR, or nil if none.

Color emoji and CJK glyphs typically render taller than the default
line height, which makes cells containing them taller than ASCII-only
cells in the same row.  When a table has rows of mixed glyph types,
the vertical borders end up at different y-positions and the
column lines look broken.  Scaling tall glyphs down via the
`display' `height' property forces a uniform line height across
all rows so borders connect cleanly.

The needed scale is just `default-h / char-h' — the factor that
brings the glyph back to the default height.  Results are cached."
  (let ((cached (gethash char agent-shell-markdown--table-height-scale-cache
                         'miss)))
    (if (eq cached 'miss)
        (let ((scale
               (let ((win (selected-window))
                     (orig-buf (window-buffer)))
                 (unwind-protect
                     (let* ((default-h
                             (or agent-shell-markdown--table-default-line-height
                                 (setq agent-shell-markdown--table-default-line-height
                                       (agent-shell-markdown--table-measure-line-height
                                        win "A"))))
                            (char-h (agent-shell-markdown--table-measure-line-height
                                     win (string char))))
                       (when (> char-h default-h)
                         (let ((ratio (/ (float default-h) char-h)))
                           (and (>= ratio
                                    agent-shell-markdown--table-min-height-scale)
                                ratio))))
                   (set-window-buffer win orig-buf)))))
          (puthash char scale agent-shell-markdown--table-height-scale-cache)
          scale)
      cached)))

(defun agent-shell-markdown--table-apply-height-scaling (str)
  "Add display height scaling to tall characters in STR.
Returns a new string with `display' `(height N)' on glyphs that
would otherwise cause uneven row heights — emoji, CJK, etc.
ASCII-only strings short-circuit and are returned unchanged."
  (if (or (not (display-graphic-p))
          (string-match-p (rx bos (* ascii) eos) str))
      str
    (let ((result (copy-sequence str))
          (len (length str)))
      (dotimes (i len)
        (let* ((ch (seq-elt result i))
               (scale (agent-shell-markdown--table-char-height-scale ch)))
          ;; Also scale a base char that's about to be widened by VS-16
          ;; (forces emoji presentation, which is what makes ⚠ become ⚠️).
          (unless scale
            (when (and (< (1+ i) len)
                       (= (seq-elt result (1+ i)) #xFE0F))
              (setq scale (agent-shell-markdown--table-char-height-scale
                           #xFE0F))))
          (when scale
            (put-text-property i (1+ i) 'display
                               `(height ,scale)
                               result))))
      result)))

(cl-defun agent-shell-markdown--table-display-width (&key str window)
  "Return display width of STR in character units.

ASCII content with no face properties uses the cheap
`string-width'.  Non-ASCII content, or ASCII content carrying a
`face' property (whose font may render at a different pixel
width — e.g. a theme styling inline-code with a wider family),
routes through `window-text-pixel-size' so column widths reflect
the actual rendered pixel width rather than a `string-width'
approximation.  Mixing the two paths within a column (some rows
ASCII-padded, some pixel-padded) accumulates fractional drift on
the right edge of the column and visibly misaligns the vertical
pipes between rows."
  (if (and window
           (window-live-p window)
           (fboundp 'window-text-pixel-size)
           (display-graphic-p)
           (or (not (string-match-p (rx bos (* ascii) eos) str))
               (agent-shell-markdown--text-has-face-p str)))
      (condition-case nil
          (let ((char-px (agent-shell-markdown--table-char-pixel-width window))
                (real-px (agent-shell-markdown--table-measure-string str window)))
            (ceiling (/ (float real-px) char-px)))
        (error (string-width str)))
    (string-width str)))

(cl-defun agent-shell-markdown--table-longest-word (&key str window)
  "Return display width of longest word in STR.
Uses `agent-shell-markdown--table-display-width' so non-ASCII words
get accurate measurement when WINDOW is given."
  (if (or (null str) (string-empty-p str))
      0
    (let ((words (split-string str "[ \t\n]+" t)))
      (if words
          (apply #'max
                 (mapcar (lambda (w)
                           (agent-shell-markdown--table-display-width
                            :str w :window window))
                         words))
        0))))

(defun agent-shell-markdown--table-total-width (widths)
  "Return total rendered width for a table with column WIDTHS.
Accounts for borders and padding (`| X | Y |' = 2 padding +
1 pipe per column, plus one leading pipe)."
  (+ 1 (seq-reduce (lambda (acc w) (+ acc w 3)) widths 0)))

(defun agent-shell-markdown--table-allocate-widths (natural-widths min-widths target)
  "Shrink NATURAL-WIDTHS proportionally to fit TARGET, respecting MIN-WIDTHS."
  (let* ((total (agent-shell-markdown--table-total-width natural-widths))
         (excess (- total target)))
    (if (<= excess 0)
        natural-widths
      (let* ((shrinkable (seq-mapn (lambda (w m) (max 0 (- w m)))
                                   natural-widths min-widths))
             (total-shrinkable (seq-reduce #'+ shrinkable 0)))
        (if (<= total-shrinkable 0)
            min-widths
          (let ((ratio (min 1.0 (/ (float excess) total-shrinkable))))
            (seq-mapn (lambda (w m s)
                        (max m (floor (- w (* s ratio)))))
                      natural-widths min-widths shrinkable)))))))

(defun agent-shell-markdown--text-has-face-p (text)
  "Return non-nil if TEXT carries any `face' text property.
Used to decide whether table cell measurement / wrap must take the
pixel-accurate path: a face like `agent-shell-markdown-inline-code'
that pulls in a different font family or weight can render at a
different pixel width than `string-width' reports."
  (or (get-text-property 0 'face text)
      (next-single-property-change 0 'face text)))

(defvar-local agent-shell-markdown--table-face-width-cache nil
  "Hash table mapping face value → pixel-width ratio vs unfaced text.
Cache lives in the destination buffer so per-buffer font settings
(text scaling, face remapping) get their own ratios.  Lazily
initialized.")

(defun agent-shell-markdown--table-face-width-ratio (face window)
  "Return pixel-width ratio of FACE-styled text vs unfaced text in WINDOW.
A ratio of 1.0 means FACE doesn't affect rendered char width.
Cached per face in the destination buffer.

Ratios are always positive floats, so `nil' from `gethash' reliably
means \"not cached yet\" — no sentinel needed."
  (with-current-buffer (window-buffer window)
    (unless agent-shell-markdown--table-face-width-cache
      (setq agent-shell-markdown--table-face-width-cache
            (make-hash-table :test 'equal)))
    (or (gethash face agent-shell-markdown--table-face-width-cache)
        (let* ((sample "MMMMMMMMMM")
               (plain-px (agent-shell-markdown--table-measure-string
                          sample window)))
          (puthash face
                   (if (zerop plain-px) 1.0
                     (/ (float (agent-shell-markdown--table-measure-string
                                (propertize sample 'face face) window))
                        plain-px))
                   agent-shell-markdown--table-face-width-cache)))))

(cl-defun agent-shell-markdown--table-wrap-char-width (text pos &optional window)
  "Return the display width contribution of the char at POS in TEXT.

Mostly `char-width', but with one correction: U+FE0F VARIATION
SELECTOR-16 forces emoji presentation on the preceding char,
widening that glyph to 2 cells (e.g. `⚠' alone renders 1 col,
`⚠\\uFE0F' / `⚠️' renders 2).  `char-width' reports 1 for `⚠' and
0 for VS-16 — summing to 1 — even though the combined grapheme
takes 2 cells.  We compensate by attributing width 1 to VS-16
itself so the running total over the grapheme equals 2.

When WINDOW is a live graphic window and the char carries a `face'
property, the result is scaled by the face's measured pixel-width
ratio (see `agent-shell-markdown--table-face-width-ratio') so wrap
decisions match the rendered width.  This catches themes where
inline-code or bold faces pull in a wider/narrower font and the
unscaled `char-width' undercounts — letting an N-char wrap line
overflow an N-cell column and push the right pipe out of line."
  (let* ((ch (seq-elt text pos))
         (base (if (= ch #xFE0F) 1 (char-width ch))))
    (if-let* ((face (and window
                         (window-live-p window)
                         (display-graphic-p)
                         (fboundp 'window-text-pixel-size)
                         (get-text-property pos 'face text))))
        (condition-case nil
            (* base (agent-shell-markdown--table-face-width-ratio
                     face window))
          (error base))
      base)))

(defun agent-shell-markdown--table-wrap-string-width (text window)
  "Return face-aware display width of TEXT in cells.
Like `string-width' but, when WINDOW is graphic, scales each char
by its face's measured pixel-width ratio so the result tracks the
rendered width rather than the unstyled char count."
  (let ((sum 0))
    (dotimes (i (length text))
      (setq sum (+ sum
                   (agent-shell-markdown--table-wrap-char-width
                    text i window))))
    sum))

(cl-defun agent-shell-markdown--table-wrap-text (text width &optional window)
  "Wrap TEXT to fit within WIDTH, returning a list of lines.
Preserves text properties across wrapped lines.

Uses the VS-16-aware width helper so that emoji presentation
sequences (`⚠️') count as their actual rendered width (2 cells)
rather than the `string-width' approximation (1 cell), which
would otherwise let a 9-rendered-col cell fit inside a 8-col
column and overflow the table border on render.

When WINDOW is a live graphic window, char widths also factor in
any `face' property's pixel-width ratio so wrap lines fit the
column in pixel terms — themes that style inline-code with a
different font would otherwise produce wrap lines whose pixel
width exceeds the column budget, drifting the right pipe."
  (cond
   ((or (null text) (string-empty-p text)) (list ""))
   ((<= (agent-shell-markdown--table-wrap-string-width text window)
        ;; Subtract VS-16 occurrences from WIDTH for the fit check —
        ;; each VS-16 widens its base char by 1 cell beyond what
        ;; `string-width' reports, so the effective budget shrinks
        ;; by one per VS-16 present.
        (- width
           (seq-count (lambda (c) (= c #xFE0F)) text)))
    (list text))
   (t
    (let ((lines '())
          (pos 0)
          (len (length text)))
      (while (< pos len)
        ;; Greedily consume chars until adding the next one would
        ;; exceed WIDTH (using VS-16-aware widths).
        (let ((end-pos pos)
              (line-width 0))
          (while (and (< end-pos len)
                      (<= (+ line-width
                             (agent-shell-markdown--table-wrap-char-width
                              text end-pos window))
                          width))
            (setq line-width
                  (+ line-width
                     (agent-shell-markdown--table-wrap-char-width
                      text end-pos window)))
            (setq end-pos (1+ end-pos)))
          ;; Make sure at least one char advances even when the very
          ;; first char already exceeds WIDTH (e.g. wide glyph).
          (when (= end-pos pos)
            (setq end-pos (1+ pos)))
          ;; Try to break at the last whitespace within [pos, end-pos).
          (let ((break-pos end-pos))
            (when (< end-pos len)
              (let ((scan (1- end-pos)))
                (while (and (> scan pos)
                            (not (memq (seq-elt text scan) '(?\s ?\t))))
                  (setq scan (1- scan)))
                (when (> scan pos)
                  (setq break-pos (1+ scan)))))
            (push (string-trim-right (substring text pos break-pos)) lines)
            (setq pos break-pos)
            (while (and (< pos len)
                        (memq (seq-elt text pos) '(?\s ?\t)))
              (setq pos (1+ pos))))))
      (nreverse lines)))))

(cl-defun agent-shell-markdown--pad-table-string (&key str width window force-pixel)
  "Pad STR with spaces to reach WIDTH columns.

ASCII-only strings take the cheap `string-width' + spaces path.
Any non-ASCII content (single-codepoint emoji, CJK, ZWJ
sequences, regional-indicator flags, VS-16 emoji) routes through
pixel-accurate measurement.  Mixing the two paths within a
column accumulates fractional drift between rows and visibly
misaligns the right-edge pipes.

When FORCE-PIXEL is non-nil, the pixel path is taken regardless of
STR's content.  Callers use this to keep all wrapped lines of one
multi-line cell on the same path — otherwise a wrapped cell that
splits non-ASCII content (e.g. an em dash) onto one line and pure
ASCII content onto another would render those continuation lines
via different paths and drift sub-pixel on their right edge."
  (if (and window
           (window-live-p window)
           (fboundp 'window-text-pixel-size)
           (display-graphic-p)
           (or force-pixel
               (not (string-match-p (rx bos (* ascii) eos) str))
               (agent-shell-markdown--text-has-face-p str)))
      (condition-case nil
          (let* ((char-px (agent-shell-markdown--table-char-pixel-width window))
                 (target-px (* width char-px))
                 (content-px (agent-shell-markdown--table-measure-string str window))
                 (pad-px (- target-px content-px)))
            (if (<= pad-px 0)
                str
              (let* ((full-spaces (floor (/ (float pad-px) char-px)))
                     (remaining-px (- pad-px (* full-spaces char-px))))
                (concat str
                        (make-string full-spaces ?\s)
                        (if (> remaining-px 0)
                            (propertize " " 'display
                                        `(space :width (,remaining-px)))
                          "")))))
        (error (agent-shell-markdown--pad-table-string-ascii :str str :width width)))
    (agent-shell-markdown--pad-table-string-ascii :str str :width width)))

(cl-defun agent-shell-markdown--pad-table-string-ascii (&key str width)
  "ASCII / fallback padding: append plain spaces to reach WIDTH columns."
  (let ((current (string-width str)))
    (if (>= current width)
        str
      (concat str (make-string (- width current) ?\s)))))

(defun agent-shell-markdown--make-table-separator-cell (width)
  "Return a separator-cell string of WIDTH dashes."
  (make-string width
               (if agent-shell-markdown-table-use-unicode-borders ?─ ?-)))

(defun agent-shell-markdown--render-table-separator-row (col-widths)
  "Build the rendered separator line for COL-WIDTHS."
  (let ((pipe (if agent-shell-markdown-table-use-unicode-borders "┼" "|"))
        (left (if agent-shell-markdown-table-use-unicode-borders "├" "|"))
        (right (if agent-shell-markdown-table-use-unicode-borders "┤" "|")))
    (concat
     (propertize left 'face 'agent-shell-markdown-table-border)
     (mapconcat
      (lambda (w)
        (propertize (agent-shell-markdown--make-table-separator-cell (+ w 2))
                    'face 'agent-shell-markdown-table-border))
      col-widths
      (propertize pipe 'face 'agent-shell-markdown-table-border))
     (propertize right 'face 'agent-shell-markdown-table-border))))

(cl-defun agent-shell-markdown--render-table-data-row (&key processed-cells col-widths row-face window)
  "Build the rendered string for a data row, possibly multi-line.

PROCESSED-CELLS is the list of propertized cell strings.
COL-WIDTHS is the list of column widths.  ROW-FACE, when non-nil,
is layered on top of the row content (preserving inline faces).
WINDOW, when given, is forwarded to `agent-shell-markdown--pad-table-string'
for pixel-accurate padding of non-ASCII content.

Each cell on the first physical line of a wrapped row carries
`agent-shell-markdown-table-cell-start' on its leading padding char so
`agent-shell-markdown-table-next-cell' / `-previous-cell' can navigate
logical rows (skipping the visual continuation lines)."
  (let* ((pipe (if agent-shell-markdown-table-use-unicode-borders "│" "|"))
         (styled-pipe (propertize pipe 'face 'agent-shell-markdown-table-border))
         (wrapped (seq-mapn
                   (lambda (cell width)
                     (agent-shell-markdown--table-wrap-text
                      cell width window))
                   processed-cells col-widths))
         ;; Per-cell "force pixel padding" flag, decided once from the
         ;; un-wrapped cell content and applied to every wrapped line
         ;; of that cell.  Without this, a cell whose wrap splits
         ;; non-ASCII content (e.g. an em dash) onto one line and pure
         ;; ASCII onto another would render those lines via different
         ;; padding paths and drift sub-pixel apart on their right edge.
         ;; Face-styled cells (e.g. inline-code) also need the pixel
         ;; path so padding pins the right edge to the column's pixel
         ;; budget — when a theme styles inline-code with a wider font
         ;; the ASCII path's `string-width' undercounts and the right
         ;; pipe drifts past the column boundary.
         (force-pixel-flags
          (mapcar (lambda (cell)
                    (or (not (string-match-p (rx bos (* ascii) eos) cell))
                        (agent-shell-markdown--text-has-face-p cell)))
                  processed-cells))
         (max-lines (apply #'max 1 (mapcar #'length wrapped)))
         (lines '()))
    (dotimes (line-idx max-lines)
      (let ((parts '()))
        (seq-mapn
         (lambda (cell-lines width force-pixel)
           (let* ((line (if (< line-idx (length cell-lines))
                            (nth line-idx cell-lines)
                          ""))
                  (padded (concat " "
                                  (agent-shell-markdown--pad-table-string
                                   :str line :width width :window window
                                   ;; Empty continuation lines have no
                                   ;; content to measure — leaving them
                                   ;; on the ASCII path avoids a wasted
                                   ;; pixel measurement that some Emacs
                                   ;; builds appear to mishandle for an
                                   ;; empty range.
                                   :force-pixel (and force-pixel
                                                     (not (string-empty-p
                                                           line))))
                                  " ")))
             (when row-face
               (add-face-text-property 0 (length padded) row-face t padded))
             ;; Mark first physical line of each cell as navigable —
             ;; continuation lines of a wrapped row aren't standalone
             ;; cells.  Tag the first content char (index 1, past the
             ;; leading padding space) so navigation lands cursor on
             ;; the content rather than the border-adjacent space.
             (when (and (zerop line-idx) (> (length padded) 1))
               (put-text-property 1 2 'agent-shell-markdown-table-cell-start t padded))
             (push padded parts)))
         wrapped col-widths force-pixel-flags)
        (push (concat styled-pipe
                      (string-join (nreverse parts) styled-pipe)
                      styled-pipe)
              lines)))
    (mapconcat #'identity (nreverse lines) "\n")))

(cl-defun agent-shell-markdown--preprocess-table (&key rows window)
  "Parse cells in ROWS and compute natural column widths.
Returns an alist with `:natural-widths' and `:processed-rows'.

`:min-widths' (wrap-allocation widths from longest words) is no
longer computed here — it's only needed when the table has to be
allocated narrower than its natural total, and computing it for
every cell on every render is a substantial cost.  Callers that
need it should use `agent-shell-markdown--table-min-widths'.

When WINDOW is given, cell widths are measured with
pixel-accurate `agent-shell-markdown--table-display-width' so columns
containing emoji/CJK line up with the column's right border."
  (let ((widths nil)
        (processed-rows nil))
    (dolist (row rows)
      (if (map-elt row :separator)
          (push (cons row nil) processed-rows)
        (let ((cells (agent-shell-markdown--parse-table-row
                      (map-elt row :start) (map-elt row :end)))
              (col 0)
              (processed-cells nil))
          (dolist (cell cells)
            (let* ((processed (agent-shell-markdown--table-apply-height-scaling
                               (map-elt cell :content)))
                   (dw (agent-shell-markdown--table-display-width
                        :str processed :window window)))
              (push processed processed-cells)
              (if (nth col widths)
                  (setf (nth col widths) (max (nth col widths) dw))
                (setq widths (append widths (list dw))))
              (setq col (1+ col))))
          (push (cons row (nreverse processed-cells)) processed-rows))))
    (list (cons :natural-widths widths)
          (cons :processed-rows (nreverse processed-rows)))))

(cl-defun agent-shell-markdown--table-min-widths (&key processed-rows window)
  "Return the minimum (longest-word) widths per column.
Called only when a table needs to be allocated narrower than its
natural total — see `agent-shell-markdown--render-table-source'."
  (let ((min-widths nil))
    (dolist (entry processed-rows)
      (let ((cells (cdr entry))
            (col 0))
        (dolist (processed cells)
          (let ((mw (agent-shell-markdown--table-longest-word
                     :str processed :window window)))
            (if (nth col min-widths)
                (setf (nth col min-widths) (max (nth col min-widths) mw))
              (setq min-widths (append min-widths (list mw))))
            (setq col (1+ col))))))
    min-widths))

(defun agent-shell-markdown--render-table (table)
  "Render TABLE by replacing [:start, :end] with the rendered :source.

The rendered chars carry:
  - `agent-shell-markdown-frozen t' — so subsequent passes skip them.
  - `agent-shell-markdown-table-source SOURCE' — the original markdown
    source, stashed so a future `agent-shell-markdown-replace-markup'
    call can combine it with freshly-streamed rows that arrive
    right after, then re-render the whole table with updated
    column widths.

Caller-set text properties at the table's start position (e.g.,
`read-only', application-specific tags like an agent-shell block
id) are also carried onto the rendered region — otherwise the
delete+insert would drop them and break callers that look up
regions by text property.

`rear-nonsticky' prevents new chars inserted just after the
rendered region from inheriting either of our two properties."
  (let* ((source (map-elt table :source))
         (table-start (map-elt table :start))
         (table-end (map-elt table :end))
         ;; Capture the destination window for pixel-accurate
         ;; measurement of non-ASCII cells.  This is the window into
         ;; which we're rendering; the render-table-source helper
         ;; forwards it through to width / padding measurement.
         (window (or (get-buffer-window (current-buffer))
                     (selected-window)))
         (rendered (agent-shell-markdown--render-table-source
                    :source source :window window))
         (carried (agent-shell-markdown--carry-properties table-start)))
    (delete-region table-start table-end)
    (goto-char table-start)
    (insert rendered)
    (let ((end (+ table-start (length rendered))))
      (when carried
        (add-text-properties table-start end carried))
      (add-text-properties
       table-start end
       `(agent-shell-markdown-frozen t
         agent-shell-markdown-table-source ,source
         rear-nonsticky (agent-shell-markdown-frozen
                         agent-shell-markdown-table-source))))))

(defun agent-shell-markdown--carry-properties (pos)
  "Return a plist of properties at POS to carry across our delete+insert.

Filters out properties our rendering itself sets (`face',
`agent-shell-markdown-frozen', `agent-shell-markdown-table-source',
`rear-nonsticky') so callers' application-level properties
(read-only, agent-shell block ids, etc.) survive on the rendered
output."
  (let ((props (text-properties-at pos))
        (carried nil))
    (while props
      (let ((key (car props))
            (val (cadr props)))
        (unless (memq key '(face
                            agent-shell-markdown-frozen
                            agent-shell-markdown-table-source
                            rear-nonsticky))
          (setq carried (cons val (cons key carried))))
        (setq props (cddr props))))
    (nreverse carried)))

(cl-defun agent-shell-markdown--render-table-source (&key source window)
  "Render SOURCE (markdown table text) to a propertized string.

SOURCE may carry text properties from earlier passes (bold faces
on cell content, `agent-shell-markdown-frozen' on inline-code bodies,
etc.); these are preserved through to the rendered output via
the cell parser.

WINDOW, when given, is the destination window used for pixel-
accurate width measurement of non-ASCII cell content (emoji,
CJK) so right borders align across rows.  Without it,
measurement falls back to `string-width' — fine for ASCII but
prone to a few-pixel drift on emoji-heavy tables."
  (with-temp-buffer
    (insert source)
    ;; SOURCE inherits `field' text properties from the calling buffer
    ;; (e.g. agent-shell tags chars with `field output'); inter-row
    ;; `\\n's may carry different field values, which would otherwise
    ;; cause `forward-line' / `line-end-position' in the parsers below
    ;; to stop at field boundaries and silently drop rows.
    (setq-local inhibit-field-text-motion t)
    (let* ((rows (agent-shell-markdown--collect-table-rows))
           (separator-row-num (agent-shell-markdown--find-separator-row-num rows))
           (preprocessed (agent-shell-markdown--preprocess-table
                          :rows rows :window window))
           (natural-widths (map-elt preprocessed :natural-widths))
           (processed-rows (map-elt preprocessed :processed-rows))
           (target-width (when agent-shell-markdown-table-wrap-columns
                           (floor (* (agent-shell-markdown--display-width)
                                     agent-shell-markdown-table-max-width-fraction))))
           (needs-allocation (and target-width
                                  (> (agent-shell-markdown--table-total-width
                                      natural-widths)
                                     target-width)))
           ;; `:min-widths' is expensive (longest-word per cell) and only
           ;; consumed by allocation, which kicks in only when the
           ;; natural total exceeds the target.  Compute lazily.
           (col-widths (if needs-allocation
                           (agent-shell-markdown--table-allocate-widths
                            natural-widths
                            (agent-shell-markdown--table-min-widths
                             :processed-rows processed-rows
                             :window window)
                            target-width)
                         natural-widths))
           (data-row-num 0)
           (rendered-rows '()))
      (dolist (entry processed-rows)
        (let* ((row (car entry))
               (processed-cells (cdr entry))
               (row-num (map-elt row :num))
               (is-separator (map-elt row :separator))
               (is-header (and separator-row-num
                               (< row-num separator-row-num)))
               (is-zebra (and agent-shell-markdown-table-zebra-stripe
                              (not is-header)
                              (not is-separator)
                              (= (mod data-row-num 2) 1)))
               (row-face (cond
                          (is-header 'agent-shell-markdown-table-header)
                          (is-zebra 'agent-shell-markdown-table-zebra))))
          (unless (or is-header is-separator)
            (setq data-row-num (1+ data-row-num)))
          (push (if is-separator
                    (agent-shell-markdown--render-table-separator-row col-widths)
                  (agent-shell-markdown--render-table-data-row
                   :processed-cells processed-cells
                   :col-widths col-widths
                   :row-face row-face
                   :window window))
                rendered-rows)))
      (string-join (nreverse rendered-rows) "\n"))))

(defun agent-shell-markdown--collect-table-rows ()
  "Collect table rows in current buffer (typically a temp buffer).
Each row is an alist with :start, :end, :num, :separator."
  (save-excursion
    (goto-char (point-min))
    (let ((rows '())
          (row-num 0))
      (while (and (not (eobp))
                  (looking-at agent-shell-markdown--table-line-regexp))
        (push `((:start . ,(point))
                (:end . ,(line-end-position))
                (:num . ,row-num)
                (:separator . ,(looking-at
                                agent-shell-markdown--table-separator-regexp)))
              rows)
        (setq row-num (1+ row-num))
        (forward-line 1))
      (nreverse rows))))

(defun agent-shell-markdown--find-separator-row-num (rows)
  "Return the index of the first separator row in ROWS, or nil."
  (let ((idx 0) (result nil))
    (dolist (row rows)
      (when (and (not result) (map-elt row :separator))
        (setq result idx))
      (setq idx (1+ idx)))
    result))

(cl-defun agent-shell-markdown--style-tables (&key avoid-ranges)
  "Render markdown tables found in current buffer.

Each detected table has its source rows deleted from the buffer
and the prettified rendering inserted in their place; the
inserted text carries `agent-shell-markdown-frozen' so subsequent calls
skip it.  Tables whose first row is already frozen — meaning
they live inside a fenced block, an inline-code body, or a
previously-rendered table — are left alone.

AVOID-RANGES is a list of (START . END) cons cells covering
regions the renderer must not touch (e.g. still-open fenced code
blocks whose closing fence hasn't streamed in yet).

Honours `agent-shell-markdown-prettify-tables'.  Cell content is taken
directly from the buffer (with text properties preserved from
the earlier inline passes), so bold/italic/inline-code/link
rendering inside cells is provided for free."
  (when agent-shell-markdown-prettify-tables
    ;; Process tables in reverse so earlier positions stay valid as
    ;; each replacement shifts everything after it.
    (dolist (table (nreverse (agent-shell-markdown--find-tables
                              :avoid-ranges avoid-ranges)))
      (agent-shell-markdown--render-table table))))

(defun agent-shell-markdown-table-next-cell ()
  "Move point to the start of the next table cell.
Wraps from the end of a row to the first cell of the next row.
Skips the separator row.  Signals `No more cells left' when
point is at or past the last cell of the table at point.

For example, with point inside cell `A' of:

  │ A │ B │
  ├───┼───┤
  │ 1 │ 2 │

a single call lands point on `B', another lands on `1', another
on `2', and a fourth signals `No more cells left'."
  (interactive)
  (agent-shell-markdown-table--move-cell :forward))

(defun agent-shell-markdown-table-previous-cell ()
  "Move point to the start of the previous table cell.
Wraps from the start of a row to the last cell of the previous
row.  Skips the separator row.  Signals `No more cells left'
when point is at or before the first cell of the table at point.

Inverse of `agent-shell-markdown-table-next-cell'."
  (interactive)
  (agent-shell-markdown-table--move-cell :backward))

(defun agent-shell-markdown-table--move-cell (direction)
  "Move point to the next or previous cell in the table at point.
DIRECTION is `:forward' or `:backward'.  Signals `user-error' when
there's no cell in that direction."
  (let* ((cells (agent-shell-markdown-table--cell-starts))
         ;; Largest cell-start index whose position is <= point — the
         ;; cell currently containing point.  -1 means point is before
         ;; the first cell.  CELLS is sorted ascending so we just walk
         ;; it tracking the last index that still satisfies the bound.
         (point-pos (point))
         (current -1)
         (i 0))
    (dolist (c cells)
      (when (<= c point-pos)
        (setq current i))
      (setq i (1+ i)))
    (let ((target (if (eq direction :forward) (1+ current) (1- current))))
      (if (and cells (<= 0 target) (< target (length cells)))
          (goto-char (nth target cells))
        (user-error "No more cells left")))))

(defun agent-shell-markdown-table--cell-starts ()
  "Return a sorted list of cell-start positions in the table at point.
Returns nil when point isn't inside a rendered agent-shell-markdown
table.  Navigable cells are tagged by the renderer with the
`agent-shell-markdown-table-cell-start' text property, so separator rows
and continuation lines of wrapped rows are skipped automatically."
  (when-let* ((region (agent-shell-markdown-table--region-at-point)))
    (let ((positions nil))
      (save-excursion
        (save-restriction
          (narrow-to-region (car region) (cdr region))
          (goto-char (point-min))
          (while (let ((m (text-property-search-forward
                           'agent-shell-markdown-table-cell-start t t)))
                   (when m
                     (push (prop-match-beginning m) positions)
                     t)))))
      (nreverse positions))))

(defun agent-shell-markdown-table--region-at-point ()
  "Return (START . END) of the rendered table at point, or nil."
  (when (get-text-property (point) 'agent-shell-markdown-table-source)
    (cons (or (previous-single-property-change
               (1+ (point)) 'agent-shell-markdown-table-source nil (point-min))
              (point-min))
          (or (next-single-property-change
               (point) 'agent-shell-markdown-table-source nil (point-max))
              (point-max)))))

(defun agent-shell-markdown--apply-faces-from (propertized buffer-start)
  "Layer `face' properties from PROPERTIZED on chars at BUFFER-START..

Uses `add-face-text-property' with PREPEND so the language's
font-lock faces take priority in the cascade over whatever face
the caller seeded the region with (e.g. a background panel face).
Chars in PROPERTIZED without a `face' are left untouched, so the
caller's seeded face shows through."
  (let ((pos 0)
        (len (length propertized)))
    (while (< pos len)
      (let ((face (get-text-property pos 'face propertized))
            (next (or (next-single-property-change pos 'face propertized) len)))
        (when face
          (add-face-text-property (+ buffer-start pos) (+ buffer-start next)
                                  face))
        (setq pos next)))))

(defun agent-shell-markdown--mirror-face-to-font-lock-face (start end)
  "Copy each `face' run across [START, END) to `font-lock-face'.

`font-lock-mode' takes ownership of the `face' property and
clears it on re-fontification, which would wipe out our markup
styling in buffers that fontify continuously (comint, shell-maker,
agent-shell, etc.).  `font-lock-face' is the property reserved
for callers who want their face to coexist — when font-lock is
on, the display engine renders `font-lock-face' as if it were
`face' and font-lock leaves it alone; when font-lock is off,
`font-lock-face' is ignored and our plain `face' renders.
Setting both means we look right in both contexts.

Only positions with a non-nil `face' are mirrored; positions
already carrying a `font-lock-face' from elsewhere are
overwritten — agent-shell-markdown owns the styling for the chars it
produced."
  (let ((pos start))
    (while (< pos end)
      (let ((face (get-text-property pos 'face))
            (next (or (next-single-property-change pos 'face nil end) end)))
        (when face
          (put-text-property pos next 'font-lock-face face))
        (setq pos next)))))

(defun agent-shell-markdown--highlight-code (code lang)
  "Return CODE syntax-highlighted using LANG's major mode.

LANG is a language identifier as written after the opening
fence (e.g. \"python\", \"elisp\").  When the resolved mode is
loadable, CODE is fontified in a temporary buffer and returned
with face properties applied.  Otherwise CODE is returned
unchanged."
  (if-let ((mode (agent-shell-markdown--resolve-lang-mode lang))
           ((fboundp mode)))
      (with-temp-buffer
        (insert code)
        (let ((inhibit-message t)
              (delay-mode-hooks t))
          (funcall mode)
          (font-lock-ensure))
        (buffer-string))
    code))

(defun agent-shell-markdown--resolve-lang-mode (lang)
  "Resolve LANG string to a major mode symbol, or nil.
LANG is case-folded and trimmed; `agent-shell-markdown-language-mapping'
is consulted for aliases before the `-mode' suffix is appended."
  (when (and lang (not (string-empty-p (string-trim lang))))
    (let* ((normalized (downcase (string-trim lang)))
           (resolved (or (map-elt agent-shell-markdown-language-mapping
                                  normalized)
                         normalized))
           (mode (intern (concat resolved "-mode"))))
      (when (fboundp mode)
        mode))))

(defun agent-shell-markdown--make-ret-binding-map (fun)
  "Return a sparse keymap binding RET and mouse-1 to FUN."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") fun)
    (define-key map [mouse-1] fun)
    (define-key map [remap self-insert-command] 'ignore)
    map))

(defun agent-shell-markdown--open-link (url)
  "Open URL.  Use local navigation for file links, `browse-url' otherwise."
  (unless (agent-shell-markdown--open-local-link url)
    (browse-url url)))

(defun agent-shell-markdown--open-local-link (url)
  "Open URL as a local file link if possible.
Return non-nil if handled, nil otherwise."
  (when-let* ((parsed (agent-shell-markdown--parse-local-link url)))
    (find-file (car parsed))
    (when (cdr parsed)
      (goto-char (point-min))
      (forward-line (1- (cdr parsed))))
    t))

(defun agent-shell-markdown--parse-local-link (url)
  "Parse URL as a local file link.
Return a (FILE . LINE) cons when URL points to an existing local
file (LINE may be nil), or nil otherwise.

For example:

  \"foo.el#L10\"             => (\"/abs/foo.el\" . 10)
  \"foo.el\"                 => (\"/abs/foo.el\" . nil)
  \"file:src/bar.el:5\"      => (\"/abs/src/bar.el\" . 5)
  \"file:///tmp/baz.el#L20\" => (\"/tmp/baz.el\" . 20)
  \"https://example.com\"    => nil"
  (when-let* ((match
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

(defun agent-shell-markdown--resolve-image-url (url)
  "Resolve image URL to an absolute local file path, or nil.
Handles file:// URIs, absolute paths, and paths starting with
`~/', `./', or `../'."
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

(defun agent-shell-markdown--image-max-width ()
  "Return the maximum image width in pixels.
Resolves `agent-shell-markdown-image-max-width' which may be an integer
(pixels) or a float between 0 and 1 (ratio of window body width)."
  (if (floatp agent-shell-markdown-image-max-width)
      (let ((window (or (get-buffer-window (current-buffer))
                        (frame-first-window))))
        (round (* agent-shell-markdown-image-max-width
                  (window-body-width window t))))
    agent-shell-markdown-image-max-width))

(defun agent-shell-markdown--watermark-start ()
  "Return the position the next scan should start from.

Reads the `agent-shell-markdown-watermark' text property off the
first character.  When absent or out of range, returns
`point-min' (whole-buffer scan — the conservative default for the
first call or after the watermark anchor has been rewritten away).

The property is stored on the rendered text itself so it travels
with the string when callers shuttle the buffer contents around
via `agent-shell-markdown-convert', avoiding a buffer-local
variable that wouldn't survive serialization."
  (let ((stored (and (> (point-max) (point-min))
                     (get-text-property (point-min)
                                        'agent-shell-markdown-watermark))))
    (if (and (integerp stored)
             (>= stored (point-min))
             (<= stored (point-max)))
        stored
      (point-min))))

(defun agent-shell-markdown--extending-table-start ()
  "Start of a table region whose rendering is still pending, or nil.

Walks lines backward from `point-max' through pipe-row
candidates.  Two cases warrant a backoff:

- A line already carries `agent-shell-markdown-table-source' —
  i.e. a previously-rendered table whose new rows we want
  `--find-tables' to fold in on the next call.

- An unbroken streak of raw pipe-rows leads back from
  `point-max' — i.e. a table whose rows have streamed in but
  whose row count has never been high enough at one call for
  `--find-tables' to render.  Without this backoff, the
  watermark advances past each row one chunk at a time and the
  table is silently never rendered.

Stops on the first non-pipe-row non-table line — past that
point, a table from there can no longer accumulate."
  (when (> (point-max) (point-min))
    (save-excursion
      ;; Walk from the last content line.  `forward-line 0' moves to
      ;; the start of the line containing point; if that landed us on
      ;; an empty trailing line (buffer ends with `\\n'), step one
      ;; line further back so the loop's first iteration examines
      ;; actual content rather than the empty tail.
      (goto-char (point-max))
      (forward-line 0)
      (when (and (eobp) (not (bobp)))
        (forward-line -1))
      (let (rendered-table-start
            pending-table-start
            (continue t))
        (while continue
          (cond
           ;; Hit a char already inside a rendered table — find its start.
           ((get-text-property (point) 'agent-shell-markdown-table-source)
            (setq rendered-table-start
                  (or (previous-single-property-change
                       (1+ (point))
                       'agent-shell-markdown-table-source)
                      (point-min)))
            (setq continue nil))
           ;; Pipe-row (or still-streaming partial of one) — remember
           ;; the earliest streak entry and step back another line.
           ;; The lenient regex also matches partial separators that
           ;; haven't grown their closing `|' yet, so the watermark
           ;; doesn't slip past the header while the separator is
           ;; mid-stream.
           ((looking-at agent-shell-markdown--table-pending-line-regexp)
            (setq pending-table-start (point))
            (if (bobp)
                (setq continue nil)
              (forward-line -1)))
           ;; Anything else — extension impossible from here.
           (t (setq continue nil))))
        (or rendered-table-start pending-table-start)))))

(defun agent-shell-markdown--set-watermark ()
  "Stamp the safe-frontier on the first character as a text property.

Safe-frontier = start of the last line in the buffer, clamped
back to the start of:
- the oldest open fenced block (if any), so the closing fence on
  a future chunk gets matched;
- a rendered table that might still extend (see
  `--extending-table-start'), so `--find-tables' under the narrow
  on the next call still sees its stashed
  `agent-shell-markdown-table-source' and folds streamed rows in.

Any position before the frontier is fully rendered and stable;
any position from the frontier onward may still resolve into new
markup as more chunks stream in.  Single-line patterns (bold,
italic, strike, header, link, image, inline code, divider) cannot
span a newline, so backing off to start-of-last-line covers their
split-across-chunks case.  Open inline backticks already extend
only to end-of-line, so they're naturally within that zone."
  (when (> (point-max) (point-min))
    (let* ((source-ranges (agent-shell-markdown--source-block-ranges))
           (open-fence-start
            (let ((last (car (last source-ranges))))
              (when (and last (= (cdr last) (point-max)))
                (car last))))
           (extending-table-start
            (agent-shell-markdown--extending-table-start))
           (last-line-start
            (save-excursion (goto-char (point-max))
                            (line-beginning-position)))
           (frontier (apply #'min
                            (delq nil (list last-line-start
                                            open-fence-start
                                            extending-table-start)))))
      (with-silent-modifications
        (put-text-property (point-min) (1+ (point-min))
                           'agent-shell-markdown-watermark frontier)))))

(defun agent-shell-markdown--make-markers (ranges)
  "Convert each (start . end) in RANGES to (start-marker . end-marker)."
  (mapcar (lambda (range)
            (cons (copy-marker (car range))
                  (copy-marker (cdr range))))
          ranges))

(defun agent-shell-markdown--sort-ranges (&rest range-collections)
  "Merge RANGE-COLLECTIONS into a vector sorted by start position.
Each collection is a sequence of (BEG . END) cons cells — list or
vector — so already-sorted vectors can be re-merged without first
being flattened.  Endpoints may be integers or markers.  The
returned vector enables O(log n) lookup via
`agent-shell-markdown--in-avoid-range-p'."
  (sort (apply #'vconcat range-collections)
        (lambda (a b) (< (car a) (car b)))))

(defun agent-shell-markdown--in-avoid-range-p (start end avoid-ranges)
  "Return the avoid-range fully containing START..END, or nil.

AVOID-RANGES is a vector of (BEG . END) cons cells sorted by BEG
— produce one with `agent-shell-markdown--sort-ranges'.
Endpoints may be integers or markers.  Ranges are assumed
non-overlapping (callers compose disjoint sources), so the first
candidate found suffices to decide containment.  The returned
range lets callers advance point past it instead of re-checking
the same range on every match inside it."
  (when avoid-ranges
    (let ((lo 0)
          (hi (length avoid-ranges))
          (candidate nil))
      (while (< lo hi)
        (let* ((mid (/ (+ lo hi) 2))
               (range (seq-elt avoid-ranges mid)))
          (if (<= (car range) start)
              (setq candidate range
                    lo (1+ mid))
            (setq hi mid))))
      (when (and candidate (<= end (cdr candidate)))
        candidate))))

(defun agent-shell-markdown--source-block-ranges ()
  "Return list of (start . end) ranges covering fenced code blocks.

Each range spans from the opening fence line to the start of the
line after the closing fence line.  A fence that is open but not
yet closed (mid-stream) extends to `point-max', so its contents
are protected as the buffer grows.

Fence widths pair like CommonMark: an opening fence of N
backticks (N>=3) is closed only by a fence line with M>=N
backticks, so a 4-backtick outer fence wraps any 3-backtick inner
fence as body rather than terminating on it.

For example, given the buffer:

  ```python
  print(\"hi\")
  ```

returns a list with one range covering the whole block."
  (let ((ranges '())
        (open-start nil)
        (open-count nil)
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx bol (zero-or-more whitespace)
                  (group (>= 3 "`"))
                  (zero-or-more not-newline))
              nil t)
        (let ((count (- (match-end 1) (match-beginning 1))))
          (cond
           ((and open-count (>= count open-count))
            (push (cons open-start (line-beginning-position 2)) ranges)
            (setq open-start nil open-count nil))
           ((not open-count)
            (setq open-start (match-beginning 0)
                  open-count count)))))
      (when open-count
        (push (cons open-start (point-max)) ranges)))
    (nreverse ranges)))

(defun agent-shell-markdown--frozen-ranges ()
  "Return ranges of buffer chars tagged `agent-shell-markdown-frozen'.

The tag is written on rendered content whose body text could
otherwise look like markdown (e.g. inline code body or source
block body).  Treating tagged ranges as avoid-ranges keeps
subsequent calls from re-processing them — important for
streaming, where the convert/replace-markup function may be
invoked many times as content grows."
  (let ((ranges '())
        (pos (point-min))
        (limit (point-max)))
    (while (< pos limit)
      (if (get-text-property pos 'agent-shell-markdown-frozen)
          (let ((end (or (next-single-property-change
                          pos 'agent-shell-markdown-frozen nil limit)
                         limit)))
            (push (cons pos end) ranges)
            (setq pos end))
        (setq pos (or (next-single-property-change
                       pos 'agent-shell-markdown-frozen nil limit)
                      limit))))
    (nreverse ranges)))

(cl-defun agent-shell-markdown--inline-code-ranges (&key avoid-ranges)
  "Return list of (start . end) ranges covering inline `X` bodies.

Each range covers the text between backticks (the backticks
themselves are not included).  Backticks inside any of
AVOID-RANGES are ignored.  A line with an odd number of backticks
has its trailing unmatched backtick treated as still-streaming:
the range extends from that backtick to end-of-line.

For example, given the buffer \"a `code` b\" returns a list with
one range covering the body \"code\"."
  (let ((ranges '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-end (line-end-position))
              (open nil))
          (while (re-search-forward "`" line-end t)
            (let ((pos (match-beginning 0)))
              (unless (agent-shell-markdown--in-avoid-range-p pos pos avoid-ranges)
                (if open
                    (progn
                      (push (cons (1+ open) pos) ranges)
                      (setq open nil))
                  (setq open pos)))))
          (when open
            (push (cons (1+ open) line-end) ranges)))
        (forward-line 1)))
    (nreverse ranges)))

(defun agent-shell-markdown--deconstruct (text)
  "Return TEXT broken into (SUBSTRING FACES) runs.

Each element is a contiguous run of characters with the same
`face' property: SUBSTRING is the run text, FACES is a list of
face symbols (a single symbol is wrapped, an unfaced run gets an
empty list).  Runs are returned in left-to-right order and cover
TEXT in full.

For example:

  (agent-shell-markdown--deconstruct
   (agent-shell-markdown-convert \"_my_ **text**\"))
  => ((\"my\" (italic)) (\" \" nil) (\"text\" (bold)))"
  (let ((runs '())
        (pos 0)
        (len (length text)))
    (while (< pos len)
      (let ((face (get-text-property pos 'face text))
            (next (or (next-single-property-change pos 'face text) len)))
        (push (list (substring-no-properties text pos next)
                    (cond ((null face) nil)
                          ((listp face) face)
                          (t (list face))))
              runs)
        (setq pos next)))
    (nreverse runs)))

(provide 'agent-shell-markdown)

;;; agent-shell-markdown.el ends here
