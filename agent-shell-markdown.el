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
;;                 (`(<url>)' also OK — angle brackets allow spaces in the url)
;;   image       `![alt](url)'            `display' property carries image
;;                 (`(<url>)' also OK — angle brackets allow spaces in the url)
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
(require 'agent-shell-work-buffer)
(require 'map)
(require 'seq)
(require 'org-faces)
(require 'url)
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
  '((t :inherit org-code))
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
  '((t :inherit org-block :foreground unspecified :extend t))
  "Background face applied to rendered fenced source-block bodies.
Inherits background from `org-block'.  `:foreground unspecified'
preserves font-lock colors.  `:extend t' fills the line to the
window edge."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-source-block-language
  '((t :inherit (italic font-lock-type-face agent-shell-markdown-source-block)))
  "Face for the language label shown above a fenced source block."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-list-marker
  '((t :inherit default))
  "Face for list bullets, task checkboxes, and ordered-list numbers
rendered by `agent-shell-markdown-convert'.  Plain by default;
restyle this face to colour or emphasise list markers."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-list-done
  '((t :inherit shadow :strike-through t))
  "Face for the text of a completed task-list item (`- [x]')
rendered by `agent-shell-markdown-convert'."
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

(defvar agent-shell-markdown-list-bullets '("•" "◦")
  "Bullet glyphs for unordered lists, cycled by nesting depth.
The Nth entry renders at depth N, wrapping past the end, so the
list length sets how many levels get a distinct bullet.")

(defvar agent-shell-markdown-list-checkbox-unchecked "□"
  "Glyph shown for an unchecked task-list item (`- [ ]').
WHITE SQUARE (U+25A1) has no emoji form, so it renders as text.")

(defvar agent-shell-markdown-list-checkbox-checked "✓"
  "Glyph shown for a checked task-list item (`- [x]').
CHECK MARK (U+2713) has no emoji form, so it renders as text.")

(defvar agent-shell-markdown-list-line-prefix "    "
  "Display-only `line-prefix' giving a rendered list its base indent.
Not part of buffer text, so it never lands in copied / saved
markdown.")

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

(defvar agent-shell-markdown-render-functions nil
  "Abnormal hook of external renderers, run before the styling passes.

Lets a third-party package (e.g. a LaTeX-math renderer) claim and
render regions of the buffer that the built-in passes should not
touch.  Each function receives a single alist CONTEXT and runs with
the buffer narrowed to the streaming region.  A renderer renders
its regions in place and tags the rendered chars with text
property `agent-shell-markdown-frozen' so the built-in passes
\(bold, italic, links, tables, ...) leave them alone, the same
mechanism fenced code blocks use.  Renderers run before the
emphasis passes, so raw delimiters are claimed before those passes
could mangle them; a renderer should skip already-frozen content
so streaming re-runs don't reprocess it.

To take part in `agent-shell-copy-as-markdown', a renderer should
also put the `agent-shell-markdown-source' text property on its
rendered region, holding the original markdown (e.g. the `$$...$$'
LaTeX) as a string.  Copying reconstructs each fully-selected span
from that property, so the region yields its source rather than its
visible text — the same mechanism fenced blocks and tables use.  A
text property (not an overlay) is required so it survives being
copied into another buffer (e.g. the viewport).

CONTEXT keys:

  :source-blocks  List of fenced-block descriptors (see
                  `agent-shell-markdown--source-blocks'), so a
                  renderer can claim blocks of its own language
                  (e.g. math, latex) and skip delimiters that fall
                  inside other code.  Each descriptor is an alist
                  with :language, :block (a :start/:end marker
                  range), :body, and :complete.

  :inline-code-ranges  List of (start . end) marker ranges covering
                  inline `code' span bodies, so a renderer can skip
                  delimiters that fall inside a verbatim span (e.g. a
                  literal `\\(x\\)' the agent meant as code, not math).

  :ui-section  The `agent-shell-ui-section' text-property value at the
                  start of the narrowed region, or nil.  Agent Shell
                  renders each fragment section (`label-left', `body',
                  `label-right', ...) in its own narrowed pass and tags
                  the whole section with this property, so a renderer
                  can skip UI chrome — e.g. a tool-command label in
                  `label-right' whose `\\(...\\)' is shell grouping, not
                  math.  nil when the narrowed region carries no section
                  property (e.g. a static `agent-shell-markdown-convert'
                  string), in which case a renderer should treat it as
                  ordinary body content.

Each function returns an alist (nil for no-op).  Recognised keys:

  :watermark  Buffer position the streaming frontier must not pass,
              so an unclosed delimiter at the buffer tail (e.g. an
              open `$$') is re-examined on the next chunk.  The
              earliest :watermark across all renderers is honoured.

For example, a renderer holding the frontier behind an open `$$'
at position 1200 returns:

  ((:watermark . 1200))")

(cl-defun agent-shell-markdown-convert (markdown)
  "Convert MARKDOWN string into propertized text.

Bold, italic, strikethrough, headers, and inline code are
rendered as text properties on the inner text; the markup
characters are removed.  See `agent-shell-markdown-replace-markup' for
the in-buffer equivalent.

For example:

  (agent-shell-markdown-convert \"_my_ **text**\")
  => #(\"my text\" 0 2 (face italic) 3 7 (face bold))"
  (agent-shell-with-work-buffer
    (insert markdown)
    ;; A complete string, not a stream: force a final render so trailing
    ;; constructs (e.g. an image at end of buffer) aren't held back.
    (agent-shell-markdown-replace-markup :force t)
    (buffer-string)))

(cl-defun agent-shell-markdown-replace-markup (&key force
                                                    (render-images t)
                                                    (highlight-blocks t)
                                                    image-cache-directory)
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

Before the built-in passes, each function in
`agent-shell-markdown-render-functions' runs so an external package
can claim and render regions (e.g. LaTeX math) that the built-in
passes should leave alone.

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
file; nil leaves the markup as-is.  IMAGE-CACHE-DIRECTORY is where
remote (http) image URLs are downloaded and cached; when nil
\(the default), remote images are not fetched and their markup is
left as text.  HIGHLIGHT-BLOCKS, when non-nil
(the default), runs the fenced-block body through the language's
major-mode font-lock to colour keywords / strings / etc.; nil
strips the fences and inserts the action label but leaves the
body un-fontified."
  (save-excursion
    (when force
      (with-silent-modifications
        (remove-text-properties (point-min) (point-max)
                                '(agent-shell-markdown-watermark nil))))
    (let ((watermark (agent-shell-markdown--watermark-start))
          (external-results)
          (context)
          (source-blocks)
          (source-ranges)
          (rendered-ranges)
          (inline-ranges)
          (avoid-ranges))
      (save-restriction
        (narrow-to-region watermark (point-max))
        ;; Build the render context (fenced-block descriptors + inline
        ;; `code' ranges) once, via the same function any external code
        ;; renders a static region through, so the two cannot drift.
        (setq context (agent-shell-markdown-context))
        (setq source-blocks (map-elt context :source-blocks))
        ;; Inline `code' spans, computed before the renderers run so an
        ;; external renderer can skip verbatim spans the same way it skips
        ;; fenced blocks.  The markers survive any buffer edits a renderer
        ;; makes and are reused as an avoid-range for the built-in passes
        ;; below.
        (setq inline-ranges (map-elt context :inline-code-ranges))
        ;; Re-project the fenced-block descriptors to the plain (start . end)
        ;; ranges the avoid-range machinery expects.  `agent-shell-markdown-context'
        ;; is the single source of truth; this is a cheap derivation from
        ;; its result.
        (setq source-ranges (agent-shell-markdown--source-block-ranges source-blocks))
        ;; Run external renderers (when any are registered) before the
        ;; styling passes.  They tag their regions
        ;; `agent-shell-markdown-frozen', so the frozen ranges captured
        ;; below (and `avoid-ranges') pick them up and the styling passes
        ;; skip them.
        (when agent-shell-markdown-render-functions
          (setq external-results (agent-shell-markdown--run-render-functions
                                  context)))
        (setq rendered-ranges (agent-shell-markdown--make-markers
                               (agent-shell-markdown--frozen-ranges)))
        (setq avoid-ranges (agent-shell-markdown-sort-ranges
                            source-ranges rendered-ranges inline-ranges))
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
          (agent-shell-markdown--replace-images
           :avoid-ranges avoid-ranges
           :image-cache-directory image-cache-directory
           :complete force)
          (agent-shell-markdown--replace-image-file-paths
           :avoid-ranges avoid-ranges))
        (agent-shell-markdown--style-dividers :avoid-ranges avoid-ranges)
        (agent-shell-markdown--style-blockquotes :avoid-ranges avoid-ranges)
        (agent-shell-markdown--style-lists :avoid-ranges avoid-ranges)
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
        ;; `--update-watermark'), so `--find-tables' under the narrow
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
                                   (insert (substring-no-properties s)))))
        ;; Mark rendered chars `fontified' so jit-lock never re-runs over
        ;; them during a mouse drag.  We style via `face'/`font-lock-face'
        ;; text properties, not font-lock keywords (`font-lock-defaults'
        ;; is `(nil t)'), so an in-drag jit-lock pass applies nothing —
        ;; but firing at all disturbs drag tracking and collapses the
        ;; selection to empty, silently breaking mouse copy of rendered
        ;; text (keyboard selection is unaffected).
        (put-text-property (point-min) (point-max) 'fontified t))
      ;; Frame rendered blocks with a blank line where they butt against
      ;; prose.  Runs outside the watermark narrow, since a block's lower
      ;; boundary sits behind the watermark by the time its successor
      ;; streams in.
      (agent-shell-markdown--pad-rendered-blocks)
      (agent-shell-markdown--update-watermark
       :source-blocks source-blocks
       :external-candidates (seq-keep (lambda (result) (map-elt result :watermark))
                                      external-results)))))

(defun agent-shell-markdown--source-block-ranges (source-blocks)
  "Project SOURCE-BLOCKS to sorted (START . END) marker ranges.

Each descriptor in SOURCE-BLOCKS (see
`agent-shell-markdown--source-blocks') carries a `:block' marker
range; this returns just those ranges as plain (START . END) conses,
sorted — the form the avoid-range machinery
\(`agent-shell-markdown-in-avoid-range-p',
`agent-shell-markdown-sort-ranges') expects."
  (agent-shell-markdown-sort-ranges
   (mapcar (lambda (source-block)
             (cons (map-nested-elt source-block '(:block :start))
                   (map-nested-elt source-block '(:block :end))))
           source-blocks)))

(defun agent-shell-markdown-context ()
  "Return the render context for the current narrowed region.

Builds the same CONTEXT alist that
`agent-shell-markdown-replace-markup' hands to the functions in
`agent-shell-markdown-render-functions', but for whatever region
the buffer is narrowed to right now:

  ((:source-blocks . SOURCE-BLOCKS)
   (:inline-code-ranges . INLINE-CODE-RANGES)
   (:ui-section . UI-SECTION))

SOURCE-BLOCKS are the fenced-block descriptors from
`agent-shell-markdown--source-blocks'.  INLINE-CODE-RANGES are
marker ranges covering inline `code' span bodies, computed with the
fenced blocks as avoid-ranges so backticks inside a fenced block are
not mistaken for an inline span.  UI-SECTION is the
`agent-shell-ui-section' property value at the start of the narrowed
region (or nil).  See `agent-shell-markdown-render-functions' for the
meaning of each key.

`agent-shell-markdown-replace-markup' builds its context through
this function too, so code that renders a static (non-streamed)
region — which never passes through the streaming render hook — can
obtain the identical context and stay in sync with the streaming
path."
  (let* ((source-blocks (agent-shell-markdown--source-blocks))
         (source-ranges (agent-shell-markdown--source-block-ranges source-blocks))
         (inline-ranges (agent-shell-markdown--make-markers
                         (agent-shell-markdown--inline-code-ranges
                          :avoid-ranges source-ranges))))
    (list (cons :source-blocks source-blocks)
          (cons :inline-code-ranges inline-ranges)
          ;; The narrowed region is a single Agent Shell fragment section,
          ;; tagged uniformly with `agent-shell-ui-section', so its value at
          ;; `point-min' identifies the section (e.g. `label-right' for a
          ;; tool-command label).  nil for a section-less region.
          (cons :ui-section (get-text-property (point-min)
                                               'agent-shell-ui-section)))))

(defun agent-shell-markdown--run-render-functions (context)
  "Run `agent-shell-markdown-render-functions' with CONTEXT.

CONTEXT is the alist from `agent-shell-markdown-context', holding
\(:source-blocks . SOURCE-BLOCKS) and (:inline-code-ranges .
INLINE-CODE-RANGES).  Each registered function is called with it
and may render and freeze regions of the current (narrowed) buffer.
Returns the list of non-nil result alists, in hook order.

For example, with one renderer returning `((:watermark . 1200))'
this returns `(((:watermark . 1200)))'."
  (let ((results '()))
    (run-hook-wrapped 'agent-shell-markdown-render-functions
                      (lambda (fn)
                        (when-let* ((result (funcall fn context)))
                          (push result results))
                        nil))
    (nreverse results)))

(cl-defun agent-shell-markdown--emphasize-span (&key markup-start markup-end
                                                     content-start content-end face)
  "Strip emphasis delimiters around CONTENT-START..CONTENT-END and face the text.

MARKUP-START..MARKUP-END spans the whole construct including its
delimiters (e.g. `**X**'); CONTENT-START..CONTENT-END is the content
text (`X').  Only the delimiters are deleted; the content text is left
in place rather than deleted and re-inserted, so any overlays, markers,
or `agent-shell-markdown-frozen' regions an earlier pass or an
external `agent-shell-markdown-render-functions' renderer anchored
inside survive.

FACE is layered on the remaining text with `add-face-text-property',
so it composes with faces from earlier passes.  Unless the text
already carries one, the original markdown is stashed on
`agent-shell-markdown-source' for `agent-shell-copy-as-markdown'.
Point is left at the end of the faced text."
  (let ((source (unless (get-text-property markup-start
                                           'agent-shell-markdown-source)
                  (agent-shell-markdown-reconstruct markup-start markup-end))))
    ;; Delete the trailing delimiter first so CONTENT-START/CONTENT-END stay
    ;; valid, then the leading one.  Removing only the delimiters keeps the
    ;; content text's characters (and anything anchored to them) intact.
    (delete-region content-end markup-end)
    (delete-region markup-start content-start)
    (let ((end (+ markup-start (- content-end content-start))))
      (add-face-text-property markup-start end face)
      (when source
        (put-text-property markup-start end
                           'agent-shell-markdown-source source))
      (goto-char end))))

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
             (avoid (agent-shell-markdown-in-avoid-range-p
                     markup-start markup-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (agent-shell-markdown--emphasize-span
           :markup-start markup-start :markup-end markup-end
           :content-start (or (match-beginning 2) (match-beginning 3))
           :content-end (or (match-end 2) (match-end 3))
           :face 'agent-shell-markdown-bold)
          (setq changed t))))
    changed))

(cl-defun agent-shell-markdown--replace-italics (&key avoid-ranges)
  "Replace `*X*' / `_X_' spans in current buffer with italic X.

Markup characters are deleted; remaining inner text carries face
`agent-shell-markdown-italic' layered on top of any existing face
properties.  Spans that fall inside any of AVOID-RANGES are left
untouched.  Returns non-nil if at least one replacement was made.

A `_X_' span must be followed by punctuation, whitespace, or a line
end, so intraword underscores such as \"_hello_world\" are left as
literal text rather than emphasized.

For example, the buffer \"hello *world*.\" becomes \"hello
world.\" with face `agent-shell-markdown-italic' on \"world\"."
  (let ((case-fold-search nil)
        (changed nil))
    (goto-char (point-min))
    (while (re-search-forward
            (rx (or (seq (or bol (one-or-more (any "\n \t")))
                         (group "*" (group (one-or-more (not (any "\n*")))) "*"))
                    (seq (or bol (one-or-more (any "\n \t")))
                         (group "_" (group (one-or-more (not (any "\n_")))) "_")
                         (or (syntax punctuation) (syntax whitespace) line-end))))
            nil t)
      (let* ((markup-start (or (match-beginning 1) (match-beginning 3)))
             (markup-end (or (match-end 1) (match-end 3)))
             (avoid (agent-shell-markdown-in-avoid-range-p
                     markup-start markup-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (agent-shell-markdown--emphasize-span
           :markup-start markup-start :markup-end markup-end
           :content-start (or (match-beginning 2) (match-beginning 4))
           :content-end (or (match-end 2) (match-end 4))
           :face 'agent-shell-markdown-italic)
          (setq changed t))))
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
             (avoid (agent-shell-markdown-in-avoid-range-p
                     markup-start markup-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (agent-shell-markdown--emphasize-span
           :markup-start markup-start :markup-end markup-end
           :content-start (match-beginning 1) :content-end (match-end 1)
           :face 'agent-shell-markdown-strikethrough)
          (setq changed t))))
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
             (avoid (agent-shell-markdown-in-avoid-range-p
                     markup-start markup-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (let* ((level (- (match-end 1) (match-beginning 1)))
                 (text (buffer-substring (match-beginning 2) (match-end 2)))
                 (source (unless (get-text-property markup-start
                                                    'agent-shell-markdown-source)
                           (agent-shell-markdown-reconstruct
                            markup-start (match-end 2))))
                 ;; `text' keeps the title's own properties.  Carry the newline
                 ;; separately so its trailing-whitespace invisibility does not
                 ;; hide the title.
                 (newline-properties
                  (agent-shell-markdown--carry-properties (1- markup-end))))
            (delete-region markup-start markup-end)
            (goto-char markup-start)
            (insert text)
            (let ((end (point)))
              (insert "\n")
              (when newline-properties
                (add-text-properties end (point) newline-properties))
              (add-face-text-property
               markup-start end
               (intern (format "agent-shell-markdown-header-%d"
                               (min (max level 1) 6))))
              (when source
                (put-text-property markup-start end
                                   'agent-shell-markdown-source source)))))))))

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
             (avoid (agent-shell-markdown-in-avoid-range-p
                     markup-start markup-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (let ((text (buffer-substring (match-beginning 1) (match-end 1)))
                (source (unless (get-text-property markup-start
                                                   'agent-shell-markdown-source)
                          (agent-shell-markdown-reconstruct
                           markup-start markup-end))))
            (delete-region markup-start markup-end)
            (goto-char markup-start)
            (insert text)
            (let ((end (+ markup-start (length text))))
              (add-face-text-property markup-start end 'agent-shell-markdown-inline-code)
              (add-text-properties markup-start end
                                   '(agent-shell-markdown-frozen t
                                                                 rear-nonsticky (agent-shell-markdown-frozen)))
              (when source
                (put-text-property markup-start end
                                   'agent-shell-markdown-source source)))))))))

(cl-defun agent-shell-markdown--link-markup-regexp (&key as-image?)
  "Return a regexp matching link (or image, when AS-IMAGE?) markup.

The destination accepts both the bare `(url)' form and the
CommonMark angle-bracketed `(<url>)' form, the latter allowing the
URL to contain spaces (e.g. `(</path/with spaces.png>)').

Capture groups: group 1 is the label (link title or image alt);
group 2 is the angle-bracketed destination body; group 3 is the
bare destination body.  Exactly one of groups 2 and 3 participates
in any match — read the URL from whichever did."
  (rx-to-string
   `(seq ,@(when as-image? '("!"))
         "["
         (group (,(if as-image? 'zero-or-more 'one-or-more) (not (any "]"))))
         "]"
         "("
         (or (seq "<" (group (zero-or-more (not (any "<" ">" "\n")))) ">")
             (group (one-or-more (not (any ")")))))
         ")")
   t))

(defun agent-shell-markdown--link-markup-url ()
  "Return the URL from the last `agent-shell-markdown--link-markup-regexp' match.
Reads capture group 2 (angle-bracketed form) when it participated,
otherwise group 3 (bare form)."
  (let ((group (if (match-beginning 2) 2 3)))
    (buffer-substring-no-properties (match-beginning group) (match-end group))))

(cl-defun agent-shell-markdown--replace-links (&key avoid-ranges)
  "Replace `[title](url)' markup with title faced as link.

The bracket/parenthesis markup is stripped; the title is left
with face `agent-shell-markdown-link' and a keymap text property that
opens the URL on RET or mouse-1.  Matches preceded by `!' (the
image syntax) are skipped, as are links inside any of
AVOID-RANGES.

A bare `(url)' destination and the CommonMark angle-bracketed
`(<url>)' form are both accepted; the latter allows spaces in the
URL (see `agent-shell-markdown--link-markup-regexp').

For example, the buffer \"see [docs](https://example.com)\"
becomes \"see docs\" with face `agent-shell-markdown-link' on \"docs\"
and a keymap that opens the URL."
  (let ((case-fold-search nil)
        (regexp (agent-shell-markdown--link-markup-regexp)))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let* ((markup-start (match-beginning 0))
             (markup-end (match-end 0))
             (is-image (eq (char-before markup-start) ?!))
             (avoid (unless is-image
                      (agent-shell-markdown-in-avoid-range-p
                       markup-start markup-end avoid-ranges))))
        (cond
         (avoid (goto-char (cdr avoid)))
         (is-image nil)
         (t
          (let ((title (buffer-substring (match-beginning 1) (match-end 1)))
                (url (agent-shell-markdown--link-markup-url))
                (source (unless (get-text-property markup-start
                                                   'agent-shell-markdown-source)
                          (agent-shell-markdown-reconstruct
                           markup-start markup-end))))
            (delete-region markup-start markup-end)
            (goto-char markup-start)
            (insert title)
            (let ((end (+ markup-start (length title))))
              (add-face-text-property markup-start end 'agent-shell-markdown-link)
              (put-text-property markup-start end 'keymap
                                 (agent-shell-markdown--make-ret-binding-map
                                  (lambda () (interactive)
                                    (agent-shell-markdown--open-link url))))
              (put-text-property markup-start end 'mouse-face 'highlight)
              ;; Expose the target as recoverable metadata so copy/export
              ;; integrations can reconstruct the link once the `(url)' is
              ;; gone from the buffer (see
              ;; `agent-shell-markdown-link-url-at-point').
              (put-text-property markup-start end 'agent-shell-markdown-url url)
              (when source
                (put-text-property markup-start end
                                   'agent-shell-markdown-source source))))))))))

(defun agent-shell-markdown--image-attributes-pending-p (pos)
  "Return non-nil when an image ending at POS may still gain `{...}' attributes.

True at end of buffer (a trailing `{width= ...}' block may still
stream in) or when such a block has opened with `{' but not closed
before end of buffer.  Callers defer rendering the image until it
settles, otherwise it renders and freezes before the attributes
arrive and they leak as literal text.

For example, with POS after the `)' of a just-streamed
`![a](x.png)' at end of buffer, returns non-nil; once `{width=50%}'
has fully streamed in after it, returns nil."
  (or (= pos (point-max))
      (save-excursion
        (goto-char pos)
        (looking-at-p "{[^}\n]*\\'"))))

(cl-defun agent-shell-markdown--replace-images (&key avoid-ranges image-cache-directory complete)
  "Replace `![alt](url)' image markup with displayed images.

If URL resolves to an existing local file that is image-supported
and a graphical display is available, the full markup is replaced
by the alt text (or a single space if alt is empty) carrying a
`display' property with the image and a keymap that opens the
file on RET or mouse-1.  Remote (http) URLs are downloaded into
IMAGE-CACHE-DIRECTORY first (see
`agent-shell-markdown--fetch-remote-image').

When a remote image can't be shown inline (no IMAGE-CACHE-DIRECTORY,
the download failed, or a non-graphical display), its markup is
replaced by a link -- the alt text, or the URL when alt is empty --
faced as `agent-shell-markdown-link' with a keymap that opens the
URL on RET or mouse-1.  Any other unresolvable markup is left
untouched.  Images inside any of AVOID-RANGES are left alone.

A bare `(url)' destination and the CommonMark angle-bracketed
`(<url>)' form are both accepted; the latter allows spaces in the
URL (see `agent-shell-markdown--link-markup-regexp').

While streaming (COMPLETE nil, the default) an image that could
still gain a trailing `{width= ...}' attribute block is left raw
until it settles (see `agent-shell-markdown--image-attributes-pending-p').
COMPLETE non-nil marks a final, non-streaming render, so such
images render immediately.

For example, the buffer \"see ![logo](logo.png)\" becomes
\"see logo\" with the image shown in place of \"logo\"."
  (let ((case-fold-search nil)
        (regexp (agent-shell-markdown--link-markup-regexp :as-image? t)))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let* ((markup-start (match-beginning 0))
             (markup-end (match-end 0))
             (avoid (agent-shell-markdown-in-avoid-range-p
                     markup-start markup-end avoid-ranges)))
        (cond
         (avoid (goto-char (cdr avoid)))
         ;; Mid-stream, the image may still gain a trailing `{width= ...}'
         ;; attribute block that hasn't fully arrived.  Rendering now would
         ;; freeze the image and orphan those attributes as literal text on
         ;; the next chunk, so leave the markup raw until it settles.  When
         ;; COMPLETE (a final, non-streaming render) there is no more to
         ;; come, so render regardless.
         ((and (not complete)
               (agent-shell-markdown--image-attributes-pending-p markup-end))
          nil)
         (t
          (let* ((alt (buffer-substring-no-properties
                       (match-beginning 1) (match-end 1)))
                 ;; The placeholder that replaces the markup must carry the
                 ;; surrounding text's properties -- the shell tags the whole
                 ;; body run with `agent-shell-ui-section', read-only, and an
                 ;; invisibility state, and the fragment layer locates the body
                 ;; by that contiguous run.  Reinserting a bare string (as
                 ;; `buffer-substring-no-properties' would give) punches a hole
                 ;; in the run, so the next streaming chunk mis-locates the body
                 ;; and hides the text after the image.  Mirror
                 ;; `--replace-links': keep the alt's properties for a non-empty
                 ;; alt, and inherit the markup's own properties for the empty
                 ;; case (space placeholder).
                 (placeholder (if (string-empty-p alt)
                                  (apply #'propertize " "
                                         (text-properties-at markup-start))
                                (buffer-substring (match-beginning 1)
                                                  (match-end 1))))
                 (url (agent-shell-markdown--link-markup-url))
                 ;; Optional Pandoc-style `{width= height=}' block directly
                 ;; after the markup; consumed with it so it doesn't leak as
                 ;; literal text, and folded into the stashed source.
                 (attribute-match (save-excursion
                                    (goto-char markup-end)
                                    (when (looking-at "{\\([^}\n]*\\)}")
                                      (cons (match-string 1) (match-end 0)))))
                 (attributes (when attribute-match
                               (agent-shell-markdown--parse-image-attributes
                                (car attribute-match))))
                 (content-end (if attribute-match (cdr attribute-match) markup-end))
                 ;; Stash the original `![alt](url)' markup so
                 ;; `agent-shell-copy-as-markdown' round-trips the image back to
                 ;; source rather than yielding the bare alt placeholder (mirrors
                 ;; `--replace-links').  Guarded so a re-render doesn't overwrite
                 ;; an already-captured source.
                 (source (unless (get-text-property markup-start
                                                    'agent-shell-markdown-source)
                           (agent-shell-markdown-reconstruct markup-start content-end)))
                 (path (agent-shell-markdown--resolve-image-url
                        url image-cache-directory)))
            (cond
             ((and path
                   (image-supported-file-p path)
                   (display-graphic-p))
              (let ((image (create-image
                            path nil nil
                            :max-width (or (map-elt attributes :max-width)
                                           (agent-shell-markdown--image-max-width))
                            :max-height (map-elt attributes :max-height))))
                (image-flush image)
                (delete-region markup-start content-end)
                (goto-char markup-start)
                (insert placeholder)
                (let ((end (+ markup-start (length placeholder))))
                  (put-text-property markup-start end 'display image)
                  (put-text-property markup-start end 'keymap
                                     (agent-shell-markdown--make-ret-binding-map
                                      (lambda () (interactive)
                                        (find-file path))))
                  (put-text-property markup-start end 'mouse-face 'highlight)
                  (when source
                    (put-text-property markup-start end
                                       'agent-shell-markdown-source source)))))
             ;; Remote image we couldn't show inline (no cache configured, the
             ;; download failed, or a non-graphical display): render a link
             ;; that opens the url, rather than leaving raw `![alt](url)' text.
             ((string-match-p "\\`https?://" url)
              (let ((label (if (string-empty-p alt)
                               (apply #'propertize url
                                      (text-properties-at markup-start))
                             placeholder)))
                (delete-region markup-start content-end)
                (goto-char markup-start)
                (insert label)
                (let ((end (+ markup-start (length label))))
                  (add-face-text-property markup-start end 'agent-shell-markdown-link)
                  (put-text-property markup-start end 'keymap
                                     (agent-shell-markdown--make-ret-binding-map
                                      (lambda () (interactive)
                                        (agent-shell-markdown--open-link url))))
                  (put-text-property markup-start end 'mouse-face 'highlight)
                  (when source
                    (put-text-property markup-start end
                                       'agent-shell-markdown-source source)))))))))))))

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
             (avoid (agent-shell-markdown-in-avoid-range-p
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
             (avoid (agent-shell-markdown-in-avoid-range-p
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
             (avoid (agent-shell-markdown-in-avoid-range-p
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

(defun agent-shell-markdown--display-width (&optional window)
  "Return a usable display width (in columns) for rendering.
Measures WINDOW's body width when given, otherwise the selected
window's, and falls back to 80 columns when no usable window is
available (e.g. batch).  Passing the destination WINDOW matters
when rendering happens with a different window selected (e.g. a
table re-laid out from an idle timer after a resize): measuring
the selected window instead would size the layout for the wrong
window and overflow the one actually showing the content."
  (or (ignore-errors (window-body-width window))
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
      ;; Honor `agent-shell-markdown-frozen'.
      (unless (get-text-property (match-beginning 4)
                                 'agent-shell-markdown-frozen)
        (let* ((open-start (match-beginning 1))
               (open-end (match-end 1))
               (lang (buffer-substring-no-properties (match-beginning 3)
                                                     (match-end 3)))
               (body-start (copy-marker (match-beginning 4)))
               (body-end (copy-marker (match-end 4)))
               (close-start (match-beginning 5))
               (close-end (match-end 5))
               (source (buffer-substring-no-properties open-start close-end))
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
              (let ((panel-bottom (marker-position body-end)))
                (save-excursion
                  (when (and (< (marker-position body-end) (point-max))
                             (eq (char-after (marker-position body-end)) ?\n))
                    (goto-char (1+ (marker-position body-end)))
                    (let ((vpad-start (point)))
                      (insert vpad-line)
                      (when carried
                        (add-text-properties vpad-start (point) carried))
                      (setq panel-bottom (point)))))
                ;; The inserted panel chrome (language label, copy icon,
                ;; padding) is not markdown, so give it an empty source: a
                ;; selection that contains it reconstructs to nothing
                ;; rather than leaking e.g. "python ⧉".  The body content
                ;; carries the fenced source.
                (put-text-property (marker-position body-start) panel-bottom
                                   'agent-shell-markdown-source "")
                (put-text-property (marker-position content-start)
                                   (marker-position body-end)
                                   'agent-shell-markdown-source source)
                ;; Tag the whole block (top vpad through bottom vpad) so
                ;; `--pad-source-blocks' can find it.  `body-start' has
                ;; collapsed to the block's top after the header insert
                ;; above.
                (put-text-property (marker-position body-start) panel-bottom
                                   'agent-shell-markdown-source-block-rendered t))
              ;; Move point past the body so the outer `re-search-forward'
              ;; loop doesn't backtrack into body content (e.g. shorter
              ;; inner fences inside a wider outer fence).
              (goto-char (marker-position body-end)))))))))

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

(defconst agent-shell-markdown--list-item-line-regexp
  (rx bol
      (group (zero-or-more (any " \t")))
      (group (or (any "-*+")
                 (seq (one-or-more digit) (any ".)"))))
      (group (one-or-more (any " \t")))
      (zero-or-more (not (any "\n")))
      "\n")
  "Regexp matching a complete markdown list-item line (trailing `\\n').
Group 1 is the leading indent, group 2 the marker (`-'/`*'/`+' or
`N.'/`N)'), group 3 the gap before the content.  The required gap
after the marker keeps `---' (a divider) and `*emphasis*' from
matching.")

(defconst agent-shell-markdown--list-item-last-line-regexp
  (rx bol
      (group (zero-or-more (any " \t")))
      (group (or (any "-*+")
                 (seq (one-or-more digit) (any ".)"))))
      (group (one-or-more (any " \t")))
      (zero-or-more (not (any "\n")))
      eos)
  "Regexp matching a list-item line anchored at the accessible buffer end.
Like `agent-shell-markdown--list-item-line-regexp' but ending at `eos'
instead of a trailing `\\n', with the same groups.  Used for a list item
on the last line of a narrowed body, whose terminating newline sits just
outside the narrow.")

(defconst agent-shell-markdown--list-item-pending-regexp
  (rx bol (zero-or-more (any " \t"))
      (or (any "-*+") (seq (one-or-more digit) (any ".)")))
      (one-or-more (any " \t")))
  "Lenient regexp: a line that is or may still become a list item.
Used to hold off a rendered list's bottom padding while another
item could still stream in below it.")

(defun agent-shell-markdown--list-bullet (depth)
  "Return the bullet glyph for nesting DEPTH, cycling the glyph set."
  (seq-elt agent-shell-markdown-list-bullets
           (mod depth (length agent-shell-markdown-list-bullets))))

(defun agent-shell-markdown--replace-list-marker (start end glyph)
  "Replace buffer text [START, END) with GLYPH, faced as a list marker.
The replaced text's application-level properties (agent-shell block
ids and `agent-shell-ui-state', `read-only', `field') are carried
onto GLYPH so it stays part of the surrounding output block; a bare
insert would split that block and make navigation stop on the
glyph."
  (let ((carried (agent-shell-markdown--carry-properties start)))
    (delete-region start end)
    (goto-char start)
    (insert (propertize glyph 'face 'agent-shell-markdown-list-marker))
    (when carried
      (add-text-properties start (point) carried))))

(cl-defun agent-shell-markdown--render-list-line
    (&key line-start line-end indent-width marker-start marker-end
          content-start)
  "Render one markdown list line in place.

The marker is replaced with its glyph as real buffer text (so a
plain copy yields the rendered bullet / checkbox, like the other
renderers); the original markdown is stashed on
`agent-shell-markdown-source' for `agent-shell-copy-as-markdown'.
An ordered `N.' keeps its number.  A display-only `line-prefix'
adds the base indent, and the line is tagged
`agent-shell-markdown-list-rendered' so `--pad-rendered-blocks'
frames it.  A checked task item's text gets
`agent-shell-markdown-list-done'.

Depth is measured in two-column units of INDENT-WIDTH, so ordinary
two-space nesting steps one glyph per level.  For example the line
`- [x] Done' renders as the buffer text `✓ Done' (struck through),
reconstructing to `- [x] Done'."
  (let* ((ordered (string-match-p
                   "\\`[0-9]"
                   (buffer-substring-no-properties marker-start marker-end)))
         (checkbox (unless ordered
                     (save-excursion
                       (goto-char content-start)
                       (when (looking-at "\\[\\([ xX]\\)\\]\\(?:[ \t]\\|$\\)")
                         (match-string 1)))))
         ;; The marker rewrite shifts everything after it; a marker keeps
         ;; the line's end (and its trailing newline) locatable.
         (end (copy-marker line-end))
         ;; End of the line's content: LINE-END drops its trailing newline
         ;; when present, but a list item on the last line of a narrowed
         ;; body (see `--style-lists') runs to the narrow with its newline
         ;; just outside, so there is nothing to drop there.
         (content-end (copy-marker (if (eq (char-before line-end) ?\n)
                                       (1- line-end)
                                     line-end)))
         ;; Stash the markdown before rewriting so copy-as-markdown can
         ;; restore it.  Excludes the trailing newline (reconstructs it
         ;; verbatim).
         (source (unless (get-text-property line-start
                                            'agent-shell-markdown-source)
                   (agent-shell-markdown-reconstruct
                    line-start (marker-position content-end)))))
    (cond
     (ordered
      (add-face-text-property marker-start marker-end
                              'agent-shell-markdown-list-marker))
     (checkbox
      (let ((glyph (if (equal checkbox " ")
                       agent-shell-markdown-list-checkbox-unchecked
                     agent-shell-markdown-list-checkbox-checked)))
        ;; Replace the whole `- [ ]' run (marker, gap, box) with one glyph.
        (agent-shell-markdown--replace-list-marker
         marker-start (+ content-start 3) glyph)
        (unless (equal checkbox " ")
          (save-excursion
            (goto-char (+ marker-start (length glyph)))
            (skip-chars-forward " \t" (marker-position content-end))
            (add-face-text-property (point) (marker-position content-end)
                                    'agent-shell-markdown-list-done)))))
     (t
      (agent-shell-markdown--replace-list-marker
       marker-start marker-end
       (agent-shell-markdown--list-bullet (/ indent-width 2)))))
    (put-text-property line-start (marker-position end)
                       'line-prefix agent-shell-markdown-list-line-prefix)
    (when source
      (put-text-property line-start (marker-position content-end)
                         'agent-shell-markdown-source source))
    (add-text-properties
     line-start (marker-position end)
     '(agent-shell-markdown-frozen t
       agent-shell-markdown-list-rendered t
       rear-nonsticky (agent-shell-markdown-frozen
                       agent-shell-markdown-list-rendered)))
    (set-marker end nil)
    (set-marker content-end nil)))

(cl-defun agent-shell-markdown--style-lists (&key avoid-ranges)
  "Render markdown list lines: bullets, task checkboxes, ordered numbers.

Each `-'/`*'/`+' or `N.' item line (with an explicit trailing
newline, so a still-streaming last line is left raw) has its
marker replaced by a glyph and gets a base indent, via
`agent-shell-markdown--render-list-line'.  The original markdown is
stashed on `agent-shell-markdown-source' so
`agent-shell-copy-as-markdown' round-trips it; a plain copy yields
the rendered glyphs.  Lines inside AVOID-RANGES (e.g. fenced code
blocks) are left untouched.

For example, the buffer:

  - Todo
  - [x] Done

renders as `• Todo' and a checkbox line struck through, each behind
a two-column base indent."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward
            agent-shell-markdown--list-item-line-regexp nil t)
      (let* ((line-start (match-beginning 0))
             (line-end (match-end 0))
             (avoid (agent-shell-markdown-in-avoid-range-p
                     line-start line-end avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (agent-shell-markdown--render-list-line
           :line-start line-start
           :line-end line-end
           :indent-width (- (match-end 1) (match-beginning 1))
           :marker-start (match-beginning 2)
           :marker-end (match-end 2)
           :content-start (match-end 3)))))
    ;; A fragment body is rendered under a narrow to its content, so a
    ;; list item on the last line has its terminating newline just past
    ;; the narrow (or none yet).  The loop above is newline-anchored, so
    ;; that last item is never rendered.  Handle it here, but only when a
    ;; newline actually exists immediately past the narrow: that proves
    ;; the line is complete rather than a still-streaming frontier (whose
    ;; marker must stay raw until it is known to be a list item).
    (when-let* ((narrow-end (point-max))
                ((save-restriction (widen) (eq (char-after narrow-end) ?\n))))
      (goto-char narrow-end)
      (beginning-of-line)
      (when (and (not (get-text-property (point)
                                         'agent-shell-markdown-list-rendered))
                 (looking-at agent-shell-markdown--list-item-last-line-regexp)
                 (not (agent-shell-markdown-in-avoid-range-p
                       (match-beginning 0) (match-end 0) avoid-ranges)))
        (agent-shell-markdown--render-list-line
         :line-start (match-beginning 0)
         :line-end (match-end 0)
         :indent-width (- (match-end 1) (match-beginning 1))
         :marker-start (match-beginning 2)
         :marker-end (match-end 2)
         :content-start (match-end 3))))))

(defun agent-shell-markdown--blank-line-at-p (pos)
  "Return non-nil when the line holding POS is blank.
A line is blank when it holds only whitespace before its newline
\(or the buffer end)."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(defun agent-shell-markdown--insert-block-padding ()
  "Insert an untinted blank line at point to frame a rendered block.

The `\\n' carries `agent-shell-markdown-source' `\"\\n\"' so copy
reconstructs a normal blank line, and `agent-shell-non-trimmable'
so `agent-shell-trim' keeps it at a response's edge.  Caller
properties at point (block ids, `read-only', `field') are carried
over so the gap stays part of the caller's block range.

A top gap is inserted on the block's first line, so any chrome that
would corrupt the gap is stripped afterwards: the block tags, the
`line-prefix' / `wrap-prefix' indent, `font-lock-face' (which,
unlike `face', survives `--carry-properties'), and `display' (a
list marker's bullet / checkbox glyph, which would otherwise render
in place of the gap's newline)."
  (let ((start (point))
        (carried (agent-shell-markdown--carry-properties (point))))
    (insert (propertize "\n"
                        'agent-shell-markdown-source "\n"
                        'agent-shell-non-trimmable t
                        'fontified t
                        'rear-nonsticky '(agent-shell-non-trimmable)))
    (when carried
      (add-text-properties start (point) carried))
    (remove-text-properties start (point)
                            '(display nil
                              font-lock-face nil
                              line-prefix nil
                              wrap-prefix nil
                              agent-shell-markdown-source-block-rendered nil
                              agent-shell-markdown-list-rendered nil))))

(defun agent-shell-markdown--frame-block (start end continues-p)
  "Frame the rendered block spanning [START, END) with blank lines.

Insert an untinted blank line above START and below END wherever
the block butts directly against non-blank text.  A side that is
already blank is left alone, so framing is idempotent.

The bottom side is held back until following text exists (so a
block whose successor hasn't streamed in yet is not padded).
CONTINUES-P, when non-nil, is a predicate called with point at the
following line's start; a non-nil return means that line could
still fold into this block (a streaming table row or list item), so
the bottom gap is deferred.  A blank line dropped there would split
the block."
  ;; Bottom first: it sits below START, so START stays valid for the
  ;; top insert without tracking it across the edit.
  (let ((following (save-excursion
                     (goto-char (max (point-min) (1- end)))
                     (forward-line 1)
                     (point))))
    (when (and (< following (point-max))
               (not (agent-shell-markdown--blank-line-at-p following))
               (not (and continues-p
                         (save-excursion
                           (goto-char following)
                           (funcall continues-p)))))
      (goto-char following)
      (agent-shell-markdown--insert-block-padding)))
  (when (and (> start (point-min))
             (not (save-excursion
                    (goto-char start)
                    (forward-line -1)
                    (agent-shell-markdown--blank-line-at-p (point)))))
    (goto-char start)
    (agent-shell-markdown--insert-block-padding)))

(defun agent-shell-markdown--pad-regions (property continues-p)
  "Frame every maximal run of non-nil PROPERTY with blank lines.
CONTINUES-P is forwarded to `agent-shell-markdown--frame-block' to
gate the bottom gap.  See `agent-shell-markdown--pad-rendered-blocks'."
  (save-excursion
    (goto-char (point-min))
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (if (get-text-property pos property)
            (let* ((end (or (next-single-property-change
                             pos property nil (point-max))
                            (point-max)))
                   ;; The top insert shifts END; a marker tracks it so the
                   ;; scan resumes just past the framed block.
                   (resume (copy-marker end)))
              (agent-shell-markdown--frame-block pos end continues-p)
              (setq pos (marker-position resume))
              (set-marker resume nil))
          (setq pos (or (next-single-property-change
                         pos property nil (point-max))
                        (point-max))))))))

(defun agent-shell-markdown--pad-rendered-blocks ()
  "Frame each rendered block with a blank line where it butts against prose.

Rendered source blocks (tagged
`agent-shell-markdown-source-block-rendered'), tables (tagged
`agent-shell-markdown-table-source') and lists (tagged
`agent-shell-markdown-list-rendered') that sit flush against
surrounding text read as cramped; this inserts a single blank line
on each such side.  Tables and lists pass a predicate so their
bottom gap waits until no further row / item can fold in.  See
`agent-shell-markdown--frame-block' for the per-side rules.

For example, a block the agent emitted flush against the text on
both sides:

  intro
  ```emacs-lisp
  (message \"hello\")
  ```
  outro

gains a blank line above and below it:

  intro

  ```emacs-lisp
  (message \"hello\")
  ```

  outro"
  ;; `inhibit-field-text-motion' so `beginning-of-line' / `forward-line'
  ;; don't stop at agent-shell's `field' boundaries between output lines.
  (let ((inhibit-field-text-motion t))
    (agent-shell-markdown--pad-regions
     'agent-shell-markdown-source-block-rendered nil)
    (agent-shell-markdown--pad-regions
     'agent-shell-markdown-table-source
     (lambda () (looking-at-p
                 agent-shell-markdown--table-pending-line-regexp)))
    (agent-shell-markdown--pad-regions
     'agent-shell-markdown-list-rendered
     (lambda () (looking-at-p
                 agent-shell-markdown--list-item-pending-regexp)))))

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
         ((let ((avoid (agent-shell-markdown-in-avoid-range-p
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
                            (not (agent-shell-markdown-in-avoid-range-p
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
                        (not (agent-shell-markdown-in-avoid-range-p
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
          ;; Strip `line-prefix' / `wrap-prefix' before measuring
          (remove-text-properties m (point) '(line-prefix nil wrap-prefix nil))
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
  "Return display width of the longest unbreakable unit in STR.

Runs of non-breakable characters form unbreakable words, measured
via `agent-shell-markdown--table-display-width' (pixel-accurate
when WINDOW is given).  Line-breakable characters (category `|':
CJK ideographs, kana, Hangul, etc.) can wrap anywhere, so each
contributes only its own `char-width'.  Otherwise a
whitespace-free CJK sentence would count as one word and pin its
column at the full sentence width.

For example, \"foo bar\" yields 3 (\"foo\"), \"日本語\" yields 2,
and \"日本のfoo語\" yields 3 (\"foo\")."
  (if (or (null str) (string-empty-p str))
      0
    (let ((len (length str))
          (longest 0)
          (word-start nil))
      (dotimes (i (1+ len))
        (let* ((ch (and (< i len) (seq-elt str i)))
               (separator (or (null ch) (memq ch '(?\s ?\t ?\n))))
               (breakable (and (not separator)
                               (aref (char-category-set ch) ?|))))
          (when (and word-start (or separator breakable))
            (setq longest (max longest
                               (agent-shell-markdown--table-display-width
                                :str (substring str word-start i)
                                :window window)))
            (setq word-start nil))
          (cond
           (breakable (setq longest (max longest (char-width ch))))
           ((and (not separator) (not word-start))
            (setq word-start i)))))
      longest)))

(defun agent-shell-markdown--table-total-width (widths)
  "Return total rendered width for a table with column WIDTHS.
Accounts for borders and padding (`| X | Y |' = 2 padding +
1 pipe per column, plus one leading pipe)."
  (+ 1 (seq-reduce (lambda (acc w) (+ acc w 3)) widths 0)))

(defun agent-shell-markdown--table-allocate-widths (natural-widths min-widths target)
  "Shrink NATURAL-WIDTHS proportionally to fit TARGET, respecting MIN-WIDTHS.

MIN-WIDTHS holds each column's longest unbreakable word, the width
below which word-wrapping alone keeps content intact.  When even
those minimums cannot fit TARGET (a window narrower than the longest
words), columns shrink below them down to a single column and the
cell wrapper hard-breaks those long words across lines, so the table
still fits rather than overflowing and line-wrapping as a whole."
  (let* ((total (agent-shell-markdown--table-total-width natural-widths))
         (excess (- total target))
         (floors (if (> (agent-shell-markdown--table-total-width min-widths) target)
                     (make-list (length min-widths) 1)
                   min-widths)))
    (if (<= excess 0)
        natural-widths
      (let* ((shrinkable (seq-mapn (lambda (w m) (max 0 (- w m)))
                                   natural-widths floors))
             (total-shrinkable (seq-reduce #'+ shrinkable 0)))
        (if (<= total-shrinkable 0)
            floors
          (let ((ratio (min 1.0 (/ (float excess) total-shrinkable))))
            (seq-mapn (lambda (w m s)
                        (max m (floor (- w (* s ratio)))))
                      natural-widths floors shrinkable)))))))

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

(defun agent-shell-markdown--table-break-after-p (text i)
  "Return non-nil when a wrapped line may break after index I in TEXT.
I + 1 must be a valid index into TEXT.  Breaks are allowed after a
line-breakable character (category `|': CJK ideographs, kana,
Hangul, etc.), unless the next character is zero-width (combining
character, variation selector, ZWJ) and must stay attached."
  (and (aref (char-category-set (seq-elt text i)) ?|)
       (> (char-width (seq-elt text (1+ i))) 0)))

(cl-defun agent-shell-markdown--table-wrap-text (text width &optional window)
  "Wrap TEXT to fit within WIDTH, returning a list of lines.
Preserves text properties across wrapped lines.

Breaks after whitespace or a line-breakable (CJK) character; a
run with no break point splits at the width limit.

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
          ;; Try to break at the last clean break point within
          ;; [pos, end-pos): after whitespace, or after a
          ;; line-breakable (CJK) character.
          (let ((break-pos end-pos))
            (when (< end-pos len)
              (let ((scan (1- end-pos)))
                (while (and (>= scan pos)
                            (not (and (> scan pos)
                                      (memq (seq-elt text scan) '(?\s ?\t))))
                            (not (agent-shell-markdown--table-break-after-p
                                  text scan)))
                  (setq scan (1- scan)))
                (when (>= scan pos)
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
         (window (get-buffer-window (current-buffer) t))
         ;; The window pixel width column widths were measured against
         ;; (nil when off-screen / string-width).  Stashed so
         ;; `agent-shell-markdown-rerender-tables' can re-lay out only the
         ;; tables whose width no longer matches the display.
         (width (and window (window-body-width window t)))
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
                                     agent-shell-markdown-table-width ,width
                                     ;; Mirror the source under the generic property that
                                     ;; `agent-shell-copy-as-markdown' reads, so tables reconstruct
                                     ;; the same way every other block does.
                                     agent-shell-markdown-source ,source
                                     rear-nonsticky (agent-shell-markdown-frozen
                                                     agent-shell-markdown-table-source
                                                     agent-shell-markdown-table-width
                                                     agent-shell-markdown-source)))
      ;; Mirror the per-cell `face' onto `font-lock-face' and mark the
      ;; region fontified, so the rendered table survives font-lock
      ;; re-fontification on its own.  `agent-shell-markdown-replace-markup'
      ;; does this globally after streaming, but a standalone re-render
      ;; (`agent-shell-markdown-rerender-tables' on resize) has no such
      ;; follow-up pass and would otherwise render mostly greyed out.
      (agent-shell-markdown--mirror-face-to-font-lock-face table-start end)
      (put-text-property table-start end 'fontified t))))

(defun agent-shell-markdown-rerender-tables ()
  "Re-lay out tables whose stored width no longer matches the display.

Each rendered table carries its markdown on
`agent-shell-markdown-table-source' and the window pixel width its
columns were measured against on `agent-shell-markdown-table-width'
\(see `agent-shell-markdown--render-table').  This re-renders from
the stashed source, at the current window width, only the tables
whose stored width differs.  A table first laid out off-screen
\(string-width) or at another width is realigned once shown, while
tables already correct for the current width are left untouched.

A no-op when the buffer isn't displayed (nothing to measure
against) or when every table is already at the current width, so it
is safe to call on display and on resize."
  (when-let* ((window (get-buffer-window (current-buffer) t))
              (width (window-body-width window t)))
    (let ((regions nil))
      ;; Collect the stale tables' bounds (as markers, so re-rendering
      ;; one doesn't invalidate the others) and stashed source.
      (save-excursion
        (goto-char (point-min))
        (let (match)
          (while (setq match (text-property-search-forward
                              'agent-shell-markdown-table-source))
            (let ((beg (prop-match-beginning match)))
              (unless (eql width (get-text-property
                                  beg 'agent-shell-markdown-table-width))
                (push (list (copy-marker beg)
                            (copy-marker (prop-match-end match))
                            (get-text-property
                             beg 'agent-shell-markdown-table-source))
                      regions))))))
      ;; A re-layout, not a content change: keep it off the undo list and
      ;; don't flip the buffer's modified flag.
      (when regions
        (let ((inhibit-read-only t)
              (buffer-undo-list t)
              (modified (buffer-modified-p)))
          (dolist (region (nreverse regions))
            (agent-shell-markdown--render-table
             (list (cons :source (nth 2 region))
                   (cons :start (marker-position (nth 0 region)))
                   (cons :end (marker-position (nth 1 region)))))
            (set-marker (nth 0 region) nil)
            (set-marker (nth 1 region) nil))
          (restore-buffer-modified-p modified))))))

(defun agent-shell-markdown--carry-properties (pos)
  "Return a plist of properties at POS to carry across our delete+insert.

Filters out properties our rendering itself sets (`face',
`font-lock-face', `agent-shell-markdown-frozen',
`agent-shell-markdown-table-source', `agent-shell-markdown-source',
`rear-nonsticky') so callers' application-level properties such as
`read-only' or agent-shell block ids survive on the rendered
output.  `font-lock-face' in particular must not be carried: the
first char's value (e.g. a table border) would otherwise be
spread uniformly across the re-rendered region, greying out the
per-cell styling on re-render."
  (let ((props (text-properties-at pos))
        (carried nil))
    (while props
      (let ((key (car props))
            (val (cadr props)))
        (unless (memq key '(face
                            font-lock-face
                            agent-shell-markdown-frozen
                            agent-shell-markdown-table-source
                            agent-shell-markdown-source
                            rear-nonsticky))
          (setq carried (cons val (cons key carried))))
        (setq props (cddr props))))
    (nreverse carried)))

(defun agent-shell-markdown-reconstruct (beg end)
  "Return the text between BEG and END with the original markdown restored.

Each rendered construct stashes the markdown for its span on the
`agent-shell-markdown-source' text property.  A span fully contained
in [BEG, END) contributes its stored source; a span whose source is
empty (inserted chrome, e.g. a code-block's language label) always
contributes nothing; other partially-selected and unrendered text
contribute their visible buffer text verbatim.

This is the reconstruction behind `agent-shell-copy-as-markdown'.
The styling passes also call it (guarded, before deleting their
markup) to capture a construct's own source: expanding the markup
span in place lets the delimiters and un-rendered inner markup pass
through verbatim, while any nested run a deeper construct already
stashed is substituted with that construct's source (such runs are
always fully inside the markup span, so the containment test holds)."
  (let ((pos beg)
        (parts '()))
    (while (< pos end)
      (let* ((source (get-text-property pos 'agent-shell-markdown-source))
             (run-end (next-single-property-change
                       pos 'agent-shell-markdown-source nil (point-max)))
             (limit (min run-end end)))
        (push
         (cond
          ;; Inserted chrome (empty source, e.g. a code-block label) is
          ;; not markdown, so it vanishes whether wholly or partly
          ;; selected.
          ((equal source "") "")
          ;; A stashed span contributes its source only when wholly inside
          ;; [BEG, END).  Test the cheap bound before the backward scan.
          ((and source
                (<= run-end end)
                (>= (previous-single-property-change
                     (min (1+ pos) (point-max))
                     'agent-shell-markdown-source nil (point-min))
                    beg))
           source)
          ;; Plain text, or a partially-selected span: emit as shown.
          (t (buffer-substring-no-properties pos limit)))
         parts)
        (setq pos limit)))
    (apply #'concat (nreverse parts))))

(defun agent-shell-markdown-link-url-at-point (&optional pos)
  "Return the rendered Markdown link URL at POS (or point), when available.

The renderer stamps `[title](url)' links with the target URL on the
`agent-shell-markdown-url' text property, so copy/export integrations
can recover the link once the `(url)' markup is gone from the buffer.
Returns nil when POS is not on a rendered link."
  (get-text-property (or pos (point)) 'agent-shell-markdown-url))

(defun agent-shell-markdown-source-block-at-point (&optional pos)
  "Return the rendered fenced code block body at POS (or point), when available.

Returns the code body (without the fences or the language label) when
POS lands on a rendered block's body, the region the renderer tags
with `agent-shell-markdown-source-block-body'.  Returns nil otherwise;
the language label above the body copies its own body via RET."
  (setq pos (or pos (point)))
  (when (get-text-property pos 'agent-shell-markdown-source-block-body)
    (buffer-substring-no-properties
     (or (previous-single-property-change
          (min (1+ pos) (point-max))
          'agent-shell-markdown-source-block-body)
         (point-min))
     (or (next-single-property-change
          pos 'agent-shell-markdown-source-block-body)
         (point-max)))))

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
  (agent-shell-with-work-buffer
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
                           (floor (* (agent-shell-markdown--display-width window)
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
  (if-let* ((mode (agent-shell-markdown--resolve-lang-mode lang))
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

(defun agent-shell-markdown--open-externally (file)
  "Prompt to open FILE with the operating system's default external program.
Opens FILE only if the user confirms.  Uses `shell-command-do-open' on
Emacs 31+, falling back to `browse-url-of-file' on earlier versions."
  (when (y-or-n-p (format "Open %s externally? " (file-name-nondirectory file)))
    (if (fboundp 'shell-command-do-open)
        (shell-command-do-open (list file))
      (browse-url-of-file file))))

(defun agent-shell-markdown--binary-file-p (file)
  "Return non-nil when FILE looks binary (a NUL byte in its first 4KB).
This is the heuristic git uses to tell binary from text."
  (and (file-readable-p file)
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert-file-contents-literally file nil 0 4096)
         (string-search "\0" (buffer-string)))))

(defun agent-shell-markdown--open-local-link (url)
  "Open URL as a local file link if possible.
Return non-nil if handled, nil otherwise.

Text/navigable files open in Emacs, jumping to the `#Lnnn' line when URL
carries one.  Binary files (which Emacs can't usefully display) instead
prompt to open with the operating system's default program, ignoring any
`#Lnnn' line (a line number is meaningless for binary)."
  (when-let* ((parsed (agent-shell-markdown--parse-local-link url)))
    (let ((file (car parsed))
          (line (cdr parsed)))
      (if (agent-shell-markdown--binary-file-p file)
          (agent-shell-markdown--open-externally file)
        (find-file file)
        (when line
          (goto-char (point-min))
          (forward-line (1- line)))))
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

(cl-defun agent-shell-markdown--url-copy-file (&key url file (timeout 5.0) content-type-prefix)
  "Download URL to FILE, returning FILE on success or nil on failure.

A hardened `url-copy-file': the fetch is synchronous but bounded by TIMEOUT
seconds (`url-copy-file' itself has no timeout), the response must be HTTP
200, and -- when CONTENT-TYPE-PREFIX is non-nil -- its `Content-Type' must
start with that prefix (e.g. \"image/\").  FILE is left untouched unless the
response passes every check, so an error page is never written in place of
the expected content.  Returns nil rather than signaling on any failure."
  (when-let* ((buffer (url-retrieve-synchronously url t t timeout)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (when (and (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
                     (or (not content-type-prefix)
                         (save-excursion
                           (re-search-forward
                            (concat "^Content-Type:[ \t]*" (regexp-quote content-type-prefix))
                            nil t)))
                     (re-search-forward "\r?\n\r?\n" nil t))
            (make-directory (file-name-directory file) t)
            (let ((coding-system-for-write 'no-conversion))
              (write-region (point) (point-max) file))
            file))
      (kill-buffer buffer))))

(defun agent-shell-markdown--fetch-remote-image (url image-cache-directory)
  "Download the remote image at URL into IMAGE-CACHE-DIRECTORY; return its path.

Returns the local cache file path, or nil when IMAGE-CACHE-DIRECTORY is nil,
URL is not an http(s) image URL, the download fails, or the response isn't
an image.  Remote images are only fetched when an IMAGE-CACHE-DIRECTORY is
provided, so a renderer with no cache configured leaves remote image markup
as text.  The cache file is named from URL's md5 so the same URL is fetched
at most once.  Only URLs ending in a known image extension are fetched, and
the response must carry an `image/...' Content-Type before it is cached (see
`agent-shell-markdown--url-copy-file'), so an error page is never stored as
an image."
  (when-let* (((stringp url))
              ((stringp image-cache-directory))
              ((string-match-p "\\`https?://" url))
              (extension (downcase (or (file-name-extension
                                        (replace-regexp-in-string "[?#].*\\'" "" url))
                                       "")))
              ((seq-contains-p image-file-name-extensions extension))
              (cache-path (expand-file-name
                           (format "%s.%s" (md5 url) extension)
                           image-cache-directory)))
    (if (file-exists-p cache-path)
        cache-path
      (agent-shell-markdown--url-copy-file :url url
                                           :file cache-path
                                           :content-type-prefix "image/"))))

(defun agent-shell-markdown--resolve-image-url (url &optional image-cache-directory)
  "Resolve image URL to an absolute local file path, or nil.
Handles http(s) URLs (downloaded into IMAGE-CACHE-DIRECTORY and cached via
`agent-shell-markdown--fetch-remote-image'; not fetched when
IMAGE-CACHE-DIRECTORY is nil), file:// URIs, absolute paths, and paths
starting with `~/', `./', or `../'."
  (if (string-match-p "\\`https?://" url)
      (agent-shell-markdown--fetch-remote-image url image-cache-directory)
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
      expanded)))

(defun agent-shell-markdown--image-max-width ()
  "Return the maximum image width in pixels.
Resolves `agent-shell-markdown-image-max-width' which may be an integer
(pixels) or a float between 0 and 1 (ratio of window body width)."
  (if (floatp agent-shell-markdown-image-max-width)
      ;; Prefer a window actually showing this buffer (any frame); only
      ;; guess with `frame-first-window' when none does.
      (let ((window (or (get-buffer-window (current-buffer) t)
                        (frame-first-window))))
        (round (* agent-shell-markdown-image-max-width
                  (window-body-width window t))))
    agent-shell-markdown-image-max-width))

(defun agent-shell-markdown--image-attribute-pixels (value dimension)
  "Resolve a Pandoc image-attribute VALUE to a pixel count for DIMENSION.
VALUE is a string such as `300', `300px', or `50%'.  DIMENSION is
`width' or `height'; a percentage resolves against the window body's
pixel size in that dimension.  Returns an integer, or nil when VALUE
is not a recognised size.

For example, (agent-shell-markdown--image-attribute-pixels \"300\" \\='width)
returns 300."
  (when-let* (((string-match "\\`\\([0-9]+\\)\\(%\\|px\\)?\\'" value))
              (number (string-to-number (match-string 1 value)))
              ;; Prefer a window actually showing this buffer (any frame);
              ;; only guess with `frame-first-window' when none does.
              (window (or (get-buffer-window (current-buffer) t)
                          (frame-first-window))))
    (if (equal (match-string 2 value) "%")
        (round (* (/ number 100.0)
                  (if (eq dimension 'width)
                      (window-body-width window t)
                    (window-body-height window t))))
      number)))

(defun agent-shell-markdown--parse-image-attributes (string)
  "Parse Pandoc-style image attributes from STRING (a `{...}' body).
Returns an alist with `:max-width' and/or `:max-height' pixel values
for any `width='/`height=' entries found, resolved via
`agent-shell-markdown--image-attribute-pixels'.  Other attributes
\(classes, ids) are ignored.

For example, (agent-shell-markdown--parse-image-attributes \"width=300\")
returns \\='((:max-width . 300))."
  (let (attributes)
    (dolist (dimension '(width height))
      (when-let* (((string-match (format "\\_<%s[ \t]*=[ \t]*\\([^ \t}]+\\)"
                                         dimension)
                                 string))
                  (pixels (agent-shell-markdown--image-attribute-pixels
                           (match-string 1 string) dimension)))
        (push (cons (intern (format ":max-%s" dimension)) pixels)
              attributes)))
    (nreverse attributes)))

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

(cl-defun agent-shell-markdown--update-watermark (&key source-blocks external-candidates)
  "Stamp the safe-frontier on the first character as a text property.

SOURCE-BLOCKS is the descriptor list from
`agent-shell-markdown--source-blocks' taken earlier this pass.  Its
`:block' markers have tracked every edit since, so the open fence
(the last block whose `:block' end is still `point-max') is read
from them directly rather than re-scanning.

Safe-frontier = start of the last line in the buffer, clamped
back to the start of:
- the oldest open fenced block (if any), so the closing fence on
  a future chunk gets matched;
- a rendered table that might still extend (see
  `--extending-table-start'), so `--find-tables' under the narrow
  on the next call still sees its stashed
  `agent-shell-markdown-table-source' and folds streamed rows in;
- any position in EXTERNAL-CANDIDATES, the `:watermark' values
  returned by functions in `agent-shell-markdown-render-functions',
  so a renderer can hold the watermark behind its own open
  delimiter (e.g. an unclosed `$$').

Any position before the frontier is fully rendered and stable;
any position from the frontier onward may still resolve into new
markup as more chunks stream in.  Single-line patterns (bold,
italic, strike, header, link, image, inline code, divider) cannot
span a newline, so backing off to start-of-last-line covers their
split-across-chunks case.  Open inline backticks already extend
only to end-of-line, so they're naturally within that zone."
  (when (> (point-max) (point-min))
    (let* ((open-fence-start
            (when-let* ((last-block (car (last source-blocks)))
                        ((= (map-nested-elt last-block '(:block :end)) (point-max))))
              (marker-position (map-nested-elt last-block '(:block :start)))))
           (extending-table-start
            (agent-shell-markdown--extending-table-start))
           (last-line-start
            (save-excursion (goto-char (point-max))
                            (line-beginning-position)))
           (frontier (apply #'min
                            (delq nil (append (list last-line-start
                                                    open-fence-start
                                                    extending-table-start)
                                              external-candidates)))))
      (with-silent-modifications
        (put-text-property (point-min) (1+ (point-min))
                           'agent-shell-markdown-watermark frontier)))))

(defun agent-shell-markdown--make-markers (ranges)
  "Convert each (start . end) in RANGES to (start-marker . end-marker)."
  (mapcar (lambda (range)
            (cons (copy-marker (car range))
                  (copy-marker (cdr range))))
          ranges))

(cl-defun agent-shell-markdown--make-range (&key start end)
  "Return a range alist `((:start . START) (:end . END))'.

For example, (agent-shell-markdown--make-range :start 1 :end 5)
returns `((:start . 1) (:end . 5))'."
  (list (cons :start start)
        (cons :end end)))

(defun agent-shell-markdown-sort-ranges (&rest range-collections)
  "Merge RANGE-COLLECTIONS into a vector sorted by start position.
Each collection is a sequence of (BEG . END) cons cells (a list or
a vector), so already-sorted vectors can be re-merged without
first being flattened.  Endpoints may be integers or markers.
Return a fresh vector of the cons cells sorted ascending by BEG,
suitable for O(log n) lookup with
`agent-shell-markdown-in-avoid-range-p'."
  (sort (apply #'vconcat range-collections)
        (lambda (a b) (< (car a) (car b)))))

(defun agent-shell-markdown-in-avoid-range-p (start end avoid-ranges)
  "Return the range in AVOID-RANGES fully containing START..END, or nil.

AVOID-RANGES is a vector of (BEG . END) cons cells sorted
ascending by BEG, as produced by `agent-shell-markdown-sort-ranges'.
Endpoints may be integers or markers.  Ranges are assumed
non-overlapping, so the first containing range is returned as its
own (BEG . END) cons cell.  Callers can advance point past its END
to avoid re-checking the same range on every match inside it."
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

(defun agent-shell-markdown--source-blocks ()
  "Return descriptors for the fenced code blocks in the current buffer.

Scans the fenced blocks once and returns one descriptor per block,
handed to `agent-shell-markdown-render-functions' so a renderer can
claim fenced blocks of its own language (e.g. math, latex) and skip
delimiters that fall inside other code.  Each descriptor is an
alist:

  ((:language . LANGUAGE)   lower-case token after the opening fence
   (:block . RANGE)         a `:start'/`:end' marker range covering
                            the whole block, tracking buffer edits
   (:body . BODY)           body text, or nil while still streaming
   (:complete . COMPLETE))  t once the closing fence has arrived

RANGE is `((:start . MARKER) (:end . MARKER))' (see
`agent-shell-markdown--make-range') spanning the opening fence line
to the start of the line after the closing fence (or `point-max'
while open).  Fence widths pair like CommonMark: an opening fence
of N backticks (N>=3) is closed only by a fence line with M>=N
backticks, so a 4-backtick fence wraps any 3-backtick inner fence
as body rather than terminating on it.

The avoid-range projection in `agent-shell-markdown-replace-markup'
and `agent-shell-markdown--update-watermark' read the `:block'
markers from the same result, so the fence-pairing scan happens
once per pass.

For example, given the buffer:

  ```math
  \\frac{a}{b}
  ```

returns one descriptor with :language \"math\", :body
\"\\frac{a}{b}\", and :complete t."
  (let ((source-blocks '())
        (open-start nil)
        (open-count nil)
        (open-language nil)
        (body-start nil)
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx bol (zero-or-more whitespace)
                  (group (>= 3 "`"))
                  (zero-or-more blank)
                  (group (zero-or-more (or alphanumeric "-" "+" "#")))
                  (zero-or-more not-newline))
              nil t)
        (let ((count (- (match-end 1) (match-beginning 1))))
          (cond
           ((and open-count (>= count open-count))
            (let ((raw (buffer-substring-no-properties
                        body-start (match-beginning 0))))
              (push (list (cons :language open-language)
                          (cons :block (agent-shell-markdown--make-range
                                        :start (copy-marker open-start)
                                        :end (copy-marker (line-beginning-position 2))))
                          (cons :body (if (string-suffix-p "\n" raw)
                                          (substring raw 0 -1)
                                        raw))
                          (cons :complete t))
                    source-blocks))
            (setq open-start nil open-count nil
                  open-language nil body-start nil))
           ((not open-count)
            (setq open-start (match-beginning 0)
                  open-count count
                  open-language (downcase
                                 (buffer-substring-no-properties
                                  (match-beginning 2) (match-end 2)))
                  body-start (line-beginning-position 2))))))
      (when open-count
        (push (list (cons :language open-language)
                    (cons :block (agent-shell-markdown--make-range
                                  :start (copy-marker open-start)
                                  :end (copy-marker (point-max))))
                    (cons :body nil)
                    (cons :complete nil))
              source-blocks)))
    (nreverse source-blocks)))

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

Exception: on a table row (a line beginning with `|') an unmatched
backtick only extends to the next `|', since a code span cannot
cross a cell boundary.  Without this a stray backtick in one cell
would swallow the rest of the row (e.g. a link in a later cell).

For example, given the buffer \"a `code` b\" returns a list with
one range covering the body \"code\"."
  (let ((ranges '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-beg (line-beginning-position))
              (line-end (line-end-position))
              (open nil))
          (while (re-search-forward "`" line-end t)
            (let ((pos (match-beginning 0)))
              (unless (agent-shell-markdown-in-avoid-range-p pos pos avoid-ranges)
                (if open
                    (progn
                      (push (cons (1+ open) pos) ranges)
                      (setq open nil))
                  (setq open pos)))))
          (when open
            (push (cons (1+ open)
                        (if (save-excursion
                              (goto-char line-beg)
                              (looking-at-p
                               agent-shell-markdown--table-pending-line-regexp))
                            (save-excursion
                              (goto-char (1+ open))
                              (if (search-forward "|" line-end t)
                                  (1- (point))
                                line-end))
                          line-end))
                  ranges)))
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
