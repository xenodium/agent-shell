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
(require 'xref)
(require 'browse-url)

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
  "Face for list markers rendered by `agent-shell-markdown-convert'.

Covers list bullets, task checkboxes, and ordered-list numbers.  Plain
by default; restyle this face to colour or emphasise list markers."
  :group 'agent-shell-markdown)

(defface agent-shell-markdown-list-done
  '((t :inherit shadow :strike-through t))
  "Face for the text of a completed task-list item (`- [x]')
rendered by `agent-shell-markdown-convert'."
  :group 'agent-shell-markdown)

(defvar agent-shell-markdown-image-max-width 0.4
  "Maximum width for inline images rendered from `![alt](url)'.
An integer is taken as pixels.  A float between 0 and 1 is a
ratio of the window body width.

`agent-shell-markdown-image-scale-increase' and
`agent-shell-markdown-image-scale-decrease' step this interactively,
re-sizing the images already on display.  They step a buffer-local
value, leaving this setting as configured.")

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
                                                    complete
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
\(e.g. `**_X_**' resolves in two rounds).  Headers, inline code,
links, images, bare image-path lines, dividers, source-block
styling, and table styling run once after the loop.

The buffer is narrowed to the streaming watermark for the
duration of the passes — content before the watermark is already
rendered and stable, so every regex / property scan starts there
instead of `point-min'.  The watermark is read off the
`agent-shell-markdown-watermark' text property on the first
character and re-stamped at the end of the call.  Pass FORCE
non-nil to drop the watermark and re-render the whole buffer
\(useful after mid-buffer edits, or for tests).

Pass COMPLETE non-nil when no more text will be appended, so
markup held back while it could still grow renders now: an image
ending the text is otherwise left raw in case a `{width=...}'
block is still streaming in (see
`agent-shell-markdown--image-attributes-pending-p').  FORCE
implies it.

RENDER-IMAGES, when non-nil (the default), replaces `![alt](url)'
markup with displayed images where the URL resolves to an image
file; nil leaves the markup as-is.  IMAGE-CACHE-DIRECTORY is where
remote (http) image URLs are downloaded and cached; when nil
\(the default), remote images are not fetched and their markup is
left as text.  HIGHLIGHT-BLOCKS, when non-nil
\(the default), runs the fenced-block body through the language's
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
        ;; Swap backslash-escaped punctuation for placeholders before the
        ;; styling passes, so an escaped delimiter (e.g. `\\*') is treated
        ;; as ordinary content; `--decode-escapes' restores the bare chars
        ;; after every pass.  Skips code, where a backslash is literal.
        (agent-shell-markdown--encode-escapes :avoid-ranges avoid-ranges)
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
           :complete (or force complete))
          (agent-shell-markdown--replace-image-file-paths
           :avoid-ranges avoid-ranges))
        ;; After the markup passes, so a URL already consumed as a
        ;; `[title](url)' destination or an image's is not seen again.
        (agent-shell-markdown--linkify-urls :avoid-ranges avoid-ranges)
        ;; Fenced blocks alone, not AVOID-RANGES: a citation is nearly
        ;; always inside an inline `code' span, which those would skip.
        (agent-shell-markdown--linkify-file-references
         :avoid-ranges source-ranges)
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
        ;; Restore backslash-escaped chars from their placeholders now that
        ;; every styling pass has run, before faces are mirrored below.
        (agent-shell-markdown--decode-escapes)
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
      ;; Normalize list spacing before framing: join items the source
      ;; separated with blank lines so a list always renders as one tidy
      ;; group.  Widened, like the framing below, so it sees the whole
      ;; list across the watermark.  Both start at a block boundary near
      ;; the watermark rather than `point-min', which cost the whole
      ;; accumulated body per chunk (issue #757).
      (let ((normalization-start
             (agent-shell-markdown--whitespace-normalization-start watermark)))
        (agent-shell-markdown--collapse-list-blank-lines normalization-start)
        ;; Frame rendered blocks with a blank line where they butt against
        ;; prose.  Runs outside the watermark narrow, since a block's lower
        ;; boundary sits behind the watermark by the time its successor
        ;; streams in.
        (agent-shell-markdown--pad-rendered-blocks normalization-start))
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
   (:inline-code-ranges . INLINE-CODE-RANGES))

SOURCE-BLOCKS are the fenced-block descriptors from
`agent-shell-markdown--source-blocks'.  INLINE-CODE-RANGES are
marker ranges covering inline `code' span bodies, computed with the
fenced blocks as avoid-ranges so backticks inside a fenced block are
not mistaken for an inline span.  See
`agent-shell-markdown-render-functions' for the meaning of each key.

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
          (cons :inline-code-ranges inline-ranges))))

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

(defconst agent-shell-markdown--escape-regexp
  (rx "\\" (group (any "!-/" ":-@" "[-`" "{-~")))
  "Matches a backslash-escaped ASCII-punctuation char, group 1 the char.
This is the CommonMark escapable set: a backslash before any ASCII
punctuation makes that punctuation literal and drops the backslash.
For example `\\*' matches with group 1 `*'.")

(defconst agent-shell-markdown--escape-placeholder ?\uE000
  "Private-use char standing in for an escaped char mid-render.
`agent-shell-markdown--encode-escapes' swaps each `\\X' for this char
so the styling passes treat the escaped delimiter as ordinary text;
`agent-shell-markdown--decode-escapes' swaps it back to X afterwards.")

(cl-defun agent-shell-markdown--encode-escapes (&key avoid-ranges)
  "Swap each backslash-escaped punctuation char for a placeholder char.
Deletes the backslash and replaces the escaped char with
`agent-shell-markdown--escape-placeholder', tagged with the literal
char on `agent-shell-markdown-escaped' and the original `\\X' markdown
on `agent-shell-markdown-source'.  The placeholder is not a markup
character, so the styling passes treat an escaped delimiter (like the
`*' in `**let\\***') as ordinary content rather than markup, and
`agent-shell-markdown--decode-escapes' restores the bare char after
they run.  Caller-owned properties on the escaped character are carried
onto the placeholder.  Escapes inside AVOID-RANGES (fenced or inline
code, where a backslash is literal) are left untouched.  Returns
non-nil on a change.

For example the buffer `**let vs let\\***' becomes `**let vs letP**'
\(P the placeholder tagged `*'), so the bold pass matches and the span
still round-trips to `**let vs let\\***' on copy."
  (let ((changed nil))
    (goto-char (point-min))
    (while (re-search-forward agent-shell-markdown--escape-regexp nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0))
            (avoid (agent-shell-markdown-in-avoid-range-p
                    (match-beginning 0) (match-end 0) avoid-ranges)))
        (if avoid
            (goto-char (cdr avoid))
          (let ((ch (char-after (match-beginning 1)))
                (source (agent-shell-markdown-reconstruct start end))
                (carried (agent-shell-markdown--carry-properties
                          (match-beginning 1))))
            (delete-region start end)
            (insert (propertize
                     (char-to-string agent-shell-markdown--escape-placeholder)
                     'agent-shell-markdown-escaped ch
                     'agent-shell-markdown-source source
                     'agent-shell-markdown-frozen t
                     'rear-nonsticky '(agent-shell-markdown-escaped
                                       agent-shell-markdown-frozen
                                       agent-shell-markdown-source)))
            (when carried
              (add-text-properties start (point) carried))
            (setq changed t)))))
    changed))

(defun agent-shell-markdown--decode-escapes ()
  "Restore placeholder chars from `agent-shell-markdown--encode-escapes'.
Each placeholder is swapped back to the literal char it stood for (read
off `agent-shell-markdown-escaped'), keeping the face and
`agent-shell-markdown-source' the passes left on it, so the visible
text is the bare char while copy-as-markdown still yields `\\X'.  Runs
after every styling pass.

For example a bold `let vs letP' (P the placeholder tagged `*') becomes
bold `let vs let*', still stashing `**let vs let\\***' for copy."
  (goto-char (point-min))
  (while (< (point) (point-max))
    (if-let* ((ch (get-text-property (point) 'agent-shell-markdown-escaped)))
        (let ((start (point))
              (props (text-properties-at (point))))
          ;; Replace the placeholder with the literal char, carrying the
          ;; face / source the passes left on it (delete+insert rather
          ;; than `subst-char-in-region', whose chars must share a byte
          ;; length the placeholder and its char do not).
          (delete-region start (1+ start))
          (insert (apply #'propertize (char-to-string ch) props))
          (remove-text-properties start (point)
                                  '(agent-shell-markdown-escaped nil)))
      (goto-char (or (next-single-property-change
                      (point) 'agent-shell-markdown-escaped nil (point-max))
                     (point-max))))))

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
          (let ((source (unless (get-text-property markup-start
                                                   'agent-shell-markdown-source)
                          (agent-shell-markdown-reconstruct
                           markup-start markup-end))))
            ;; Delete the two backticks where they stand rather than the
            ;; span as a whole: the `:inline-code-ranges' every later pass
            ;; avoids this body through are markers into it, and deleting
            ;; the span collapses them onto a single point.  The body would
            ;; then read as ordinary prose, rendering `[title](url)' inside
            ;; backticks as a link instead of the literal text asked for.
            (delete-region (1- markup-end) markup-end)
            (delete-region markup-start (1+ markup-start))
            (let ((end (- markup-end 2)))
              (add-face-text-property markup-start end 'agent-shell-markdown-inline-code)
              (add-text-properties markup-start end
                                   '(agent-shell-markdown-frozen t
                                                                 rear-nonsticky (agent-shell-markdown-frozen)))
              (when source
                (put-text-property markup-start end
                                   'agent-shell-markdown-source source))
              (goto-char end))))))))

(cl-defun agent-shell-markdown--link-markup-regexp (&key as-image?)
  "Return a regexp matching link (or image, when AS-IMAGE?) markup.

The destination accepts both the bare `(url)' form and the
CommonMark angle-bracketed `(<url>)' form, the latter allowing the
URL to contain spaces (e.g. `(</path/with spaces.png>)').

A bare destination may carry balanced parentheses, as CommonMark
allows and as Wikipedia URLs need (e.g.
`(https://en.wikipedia.org/wiki/Bender_(Futurama))'), so that the
inner `)' doesn't end the destination early.  An unbalanced one
still does, and has to be escaped or angle-bracketed.

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
             (group (one-or-more (or (not (any "(" ")"))
                                     (seq "(" (zero-or-more (not (any "(" ")"))) ")")))))
         ")")
   t))

(defun agent-shell-markdown--link-markup-url ()
  "Return the URL from the last `agent-shell-markdown--link-markup-regexp' match.
Reads capture group 2 (angle-bracketed form) when it participated,
otherwise group 3 (bare form)."
  (let ((group (if (match-beginning 2) 2 3)))
    (buffer-substring-no-properties (match-beginning group) (match-end group))))

(defun agent-shell-markdown--link-verb (url verb)
  "Return VERB, or the action opening URL described when VERB is nil.

Describes where invoking lands: \"open file\" for a local file (which
opens in Emacs) and \"open in browser\" otherwise.  Resolved on each
call rather than at render time, so a file appearing or going away
later doesn't leave the wording stale.

For example, URL \"/tmp/notes.org\" returns \"open file\" while it
exists, and \"open in browser\" once it doesn't."
  (or verb
      (if (agent-shell-markdown--parse-local-link url)
          "open file"
        "open in browser")))

(cl-defun agent-shell-markdown--apply-link-properties (&key start end url verb)
  "Make [START, END) a rendered link to URL.

Applies what every link the renderer produces carries: face
`agent-shell-markdown-link', a keymap opening URL on RET or a mouse
click, a hint naming that key, a `help-echo' saying the same to the
mouse, a hand pointer, and the target itself on
`agent-shell-markdown-url' -- which is what
`agent-shell-markdown-link-url-at-point' reads and what item
navigation stops on, so a link missing it would open on RET yet stay
invisible to both.

VERB names the action in both hints (see
`agent-shell-markdown--link-verb' for the default and when it
resolves).

For example, over the `docs' of a rendered \"[docs](https://gnu.org)\",
RET opens the URL, the echo area reads \"Press RET to open in
browser\", and hovering shows \"Open in browser\"."
  (let* ((open-action (lambda () (interactive)
                        (agent-shell-markdown--open-link url)))
         (link-map (agent-shell-markdown--make-ret-binding-map open-action)))
    (add-face-text-property start end 'agent-shell-markdown-link)
    (put-text-property start end 'keymap link-map)
    (agent-shell-markdown--put-hint-sensor
     start end
     (lambda ()
       (agent-shell-markdown--action-hint
        :action open-action
        :keymap link-map
        :verb (agent-shell-markdown--link-verb url verb))))
    ;; The mouse gets the same wording without a key in it, resolved on
    ;; hover like the echoed hint is.
    (put-text-property start end 'help-echo
                       (lambda (_window _object _pos)
                         (let ((text (agent-shell-markdown--link-verb url verb)))
                           (concat (upcase (substring text 0 1))
                                   (substring text 1)))))
    ;; A hand pointer when over is enough. No need for `mouse-face'.
    (put-text-property start end 'pointer 'hand)
    (put-text-property start end 'agent-shell-markdown-url url)))

(cl-defun agent-shell-markdown--replace-links (&key avoid-ranges)
  "Replace `[title](url)' markup with title faced as link.

The bracket/parenthesis markup is stripped; the title is left
with face `agent-shell-markdown-link' and a keymap text property that
opens the URL on RET or \\`mouse-1'.  Entering the title echoes that key
along with where it lands, since a local file opens in Emacs while
anything else goes to the browser (see
`agent-shell-markdown--action-hint').  Matches preceded by `!' (the
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
              (agent-shell-markdown--apply-link-properties
               :start markup-start :end end :url url)
              (when source
                (put-text-property markup-start end
                                   'agent-shell-markdown-source source))))))))))

(cl-defun agent-shell-markdown--linkify-urls (&key avoid-ranges)
  "Face bare URLs in prose as links, openable from the buffer.

Matches what `browse-url-button-regexp' calls a URL, so what counts
follows Emacs rather than a list kept here: `http'/`https' and other
schemes it knows (`ftp', `mailto', `file'), plus a scheme-less `www.'
host.  A bare host without either (`example.com') stays text, so
ordinary dotted words don't become links.

There is no markup to strip, so the URL text stays as it stands and
gains what any rendered link carries (see
`agent-shell-markdown--apply-link-properties'), including the target
on `agent-shell-markdown-url', so copy/export and item navigation see
it as a link too.

URLs inside any of AVOID-RANGES (fenced blocks, inline code) are left
alone, as is text an earlier pass already rendered: a link's target, an
image path shown in place (see
`agent-shell-markdown--replace-image-file-paths', where RET opens the
file rather than the URL), or an image displayed over text that reads
as a URL.

Trailing punctuation stays out of the URL, as do the brackets around
a parenthesised one, since matching is `browse-url-button-regexp'.

For example, the buffer \"see https://example.com now.\" keeps its
text, with \"https://example.com\" faced and openable."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward browse-url-button-regexp nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (if-let* ((avoid (agent-shell-markdown-in-avoid-range-p
                          start end avoid-ranges)))
            (goto-char (cdr avoid))
          (agent-shell-markdown--linkify-url start end))))))

(defun agent-shell-markdown--linkify-url (start end)
  "Give the URL on [START, END) the properties a rendered link carries.

A no-op when an earlier pass already spoke for that text: a link's or
fallback image's target, a `file://' image path rendered in place
\(which tags itself frozen, and whose RET opens the file rather than
the URL), or an image displayed over text that reads as a URL.

For example, over the URL in \"see https://example.com now\", RET
there opens it in the browser."
  (when-let* (((not (get-text-property start 'agent-shell-markdown-url)))
              ((not (get-text-property start 'agent-shell-markdown-frozen)))
              ((not (eq (car-safe (get-text-property start 'display)) 'image))))
    (agent-shell-markdown--apply-link-properties
     :start start :end end
     :url (buffer-substring-no-properties start end))))

(defconst agent-shell-markdown--file-reference-regexp
  (rx (one-or-more (any alnum "_./+~-"))
      ":" (one-or-more digit)
      (optional "-" (optional "L") (one-or-more digit)))
  "Regexp matching a file reference as an agent cites one in prose.

A path followed by a line, optionally a line range: \"foo.el:12\" or
\"foo.el:12-20\".  A cited column is left out of the match, so
\"foo.el:12:5\" links its line and leaves \":5\" as text.  Deliberately loose
about what a path looks like -- what makes a match a reference is that
it resolves to a file that exists, which
`agent-shell-markdown--linkify-file-reference' checks.")

(cl-defun agent-shell-markdown--linkify-file-references (&key avoid-ranges)
  "Face the file references in prose as links, openable from the buffer.

Agents cite their sources as `docs/audit.md:500' rather than as
markdown links, so the citation renders as plain text and there is no
way to reach the file it names.  Each one gains what any rendered link
carries (see `agent-shell-markdown--apply-link-properties'), including
a target `agent-shell-markdown--open-local-link' resolves, so RET opens
the file on that line and item navigation stops there.

References inside any of AVOID-RANGES are left alone, which callers
pass as the fenced blocks alone: a path inside one is sample content or
command output rather than a citation.  Inline `code' spans are
deliberately not avoided -- backticks are how agents write these
references, so skipping them would skip nearly every reference there
is.

For example, the buffer \"see `foo.el:12` now\" keeps its text, with
\"foo.el:12\" faced and openable."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward agent-shell-markdown--file-reference-regexp nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (if-let* ((avoid (agent-shell-markdown-in-avoid-range-p
                          start end avoid-ranges)))
            (goto-char (cdr avoid))
          (agent-shell-markdown--linkify-file-reference start end))))))

(defun agent-shell-markdown--linkify-file-reference (start end)
  "Give the reference on [START, END) the properties a rendered link carries.

A no-op when the text already carries a link\='s target, which is where
an earlier pass rendered a `[title](foo.el:12)' destination, or when
the path names no file that exists.

The body of an inline `code' span is tagged
`agent-shell-markdown-frozen', and a reference is usually inside one.
That tag keeps later passes from re-parsing text whose markup has
already been resolved; adding properties re-parses nothing, so it
doesn\='t apply here.

For example, over the reference in \"see foo.el:12 now\", RET opens
`foo.el' on line 12."
  (let ((reference (buffer-substring-no-properties start end)))
    (when (and (not (get-text-property start 'agent-shell-markdown-url))
               (agent-shell-markdown--parse-local-link reference))
      (agent-shell-markdown--apply-link-properties
       :start start :end end :url reference))))

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
file on RET or \\`mouse-1'.  Entering the image echoes that key and the
ones that resize it (see `agent-shell-markdown--image-hint'), since
a displayed image otherwise gives no sign that it acts.  Remote http
URLs are downloaded into IMAGE-CACHE-DIRECTORY first (see
`agent-shell-markdown--fetch-remote-image').

When a remote image can't be shown inline (no IMAGE-CACHE-DIRECTORY,
the download failed, or a non-graphical display), its markup is
replaced by a link -- the alt text, or the URL when alt is empty --
faced as `agent-shell-markdown-link' with a keymap that opens the
URL on RET or \\`mouse-1', echoing that key on entry as a rendered image
does.  Any other unresolvable markup is left
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
                 ;; How this image's max-width tracks the window, stored so
                 ;; `agent-shell-markdown-rerender-images' can re-size it: an
                 ;; explicit `{width=N%}' fraction (intrinsic to the markup),
                 ;; or the symbol `default' when it comes from
                 ;; `agent-shell-markdown-image-max-width' (re-read live on
                 ;; resize and on `agent-shell-markdown-image-scale-increase',
                 ;; so a changed setting applies).  Nil for a size the markup
                 ;; itself fixes in pixels.
                 (width-ratio (cond ((map-elt attributes :max-width-ratio))
                                    ((map-elt attributes :max-width) nil)
                                    (t 'default)))
                 (content-end (if attribute-match (cdr attribute-match) markup-end))
                 ;; Stash the original `![alt](url)' markup so
                 ;; `agent-shell-copy-as-markdown' round-trips the image back to
                 ;; source rather than yielding the bare alt placeholder (mirrors
                 ;; `--replace-links').  Guarded so a re-render doesn't overwrite
                 ;; an already-captured source.
                 (source (unless (get-text-property markup-start
                                                    'agent-shell-markdown-source)
                           (agent-shell-markdown-reconstruct markup-start content-end)))
                 ;; `![alt](url)' says an image is meant, so a bare
                 ;; `logo.png' resolves here where a bare path line's
                 ;; would not.
                 (path (agent-shell-markdown--resolve-image-url
                        url
                        :image-cache-directory image-cache-directory
                        :allow-bare-relative t)))
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
                (let* ((end (+ markup-start (length placeholder)))
                       (open-action (lambda () (interactive)
                                      (agent-shell-markdown-visit-file :file path)))
                       (image-keymap (agent-shell-markdown--make-ret-binding-map
                                      open-action)))
                  (put-text-property markup-start end 'display image)
                  (put-text-property markup-start end 'keymap image-keymap)
                  ;; Nothing about a displayed image says it's actionable, so
                  ;; echo what its keys do as the cursor enters.
                  (agent-shell-markdown--put-hint-sensor
                   markup-start end
                   (lambda ()
                     (agent-shell-markdown--image-hint
                      :action open-action
                      :keymap image-keymap)))
                  ;; The mouse gets the same wording without a key in it.
                  ;; Resizing is left out: it's keys-only.
                  (put-text-property markup-start end 'help-echo "Open image")
                  ;; A hand pointer when over is enough. No need for `mouse-face'.
                  (put-text-property markup-start end 'pointer 'hand)
                  (when source
                    (put-text-property markup-start end
                                       'agent-shell-markdown-source source))
                  (when width-ratio
                    (put-text-property markup-start end
                                       'agent-shell-markdown-image-width-ratio
                                       width-ratio)
                    (put-text-property
                     markup-start end 'agent-shell-markdown-image-window-width
                     (agent-shell-markdown--displayed-window-width))))))
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
                  ;; A link standing in for the image, so the hint says so
                  ;; and there's no resizing to offer.
                  (agent-shell-markdown--apply-link-properties
                   :start markup-start :end end :url url
                   :verb "open image in browser")
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
opens the file, whose key is echoed along with the resizing ones as
the cursor enters the image (see
`agent-shell-markdown--image-hint').  Lines inside any of
AVOID-RANGES are left untouched, as are unresolvable paths.

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
              (let* ((image (create-image
                             resolved nil nil
                             :max-width (agent-shell-markdown--image-max-width)))
                     (open-action (lambda () (interactive)
                                    (agent-shell-markdown-visit-file :file resolved)))
                     (image-keymap (agent-shell-markdown--make-ret-binding-map
                                    open-action)))
                (image-flush image)
                (put-text-property path-start path-end 'display image)
                (put-text-property path-start path-end 'keymap image-keymap)
                ;; Nothing about a displayed image says it's actionable, so
                ;; echo what its keys do as the cursor enters.
                (agent-shell-markdown--put-hint-sensor
                 path-start path-end
                 (lambda ()
                   (agent-shell-markdown--image-hint
                    :action open-action
                    :keymap image-keymap)))
                ;; The mouse gets the same wording without a key in it.
                ;; Resizing is left out: it's keys-only.
                (put-text-property path-start path-end 'help-echo "Open image")
                ;; A hand pointer when over is enough. No need for `mouse-face'.
                (put-text-property path-start path-end 'pointer 'hand)
                ;; A bare-path image is sized by the default
                ;; `agent-shell-markdown-image-max-width', so mark it
                ;; `default' for `agent-shell-markdown-rerender-images' to
                ;; re-read live.
                (put-text-property path-start path-end
                                   'agent-shell-markdown-image-width-ratio
                                   'default)
                (put-text-property
                 path-start path-end 'agent-shell-markdown-image-window-width
                 (agent-shell-markdown--displayed-window-width))
                (add-text-properties path-start path-end
                                     '(agent-shell-markdown-frozen t
                                                                   rear-nonsticky (agent-shell-markdown-frozen))))))))))))

(cl-defun agent-shell-markdown--style-dividers (&key avoid-ranges)
  "Render `---' / `***' / `___' horizontal-rule lines as styled rules.

Each line consisting of 3+ matching dash/star/underscore chars
\(optionally surrounded by spaces or tabs) gets a `display' text
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
                                       " "
                                       ;; Tag the copy glyph so item
                                       ;; navigation can land on the
                                       ;; affordance itself (see
                                       ;; `agent-shell-markdown--next-visible-source-block').
                                       (propertize
                                        "⧉"
                                        'agent-shell-markdown-source-block-copy t)))
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
                   (label-map (agent-shell-markdown--make-ret-binding-map
                               kill-action))
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
                           'pointer 'hand
                           'keymap label-map
                           'cursor-sensor-functions
                           (agent-shell-markdown--make-hint-sensor
                            (lambda ()
                              (agent-shell-markdown--action-hint
                               :action kill-action
                               :keymap label-map
                               :verb "copy")))
                           'agent-shell-markdown-frozen t
                           ;; `cursor-sensor-functions' included so the
                           ;; hint stops at the label rather than showing
                           ;; on the character after it.
                           'rear-nonsticky '(agent-shell-markdown-frozen
                                             cursor-sensor-functions)
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
  "Regexp matching a line that might still be streaming into a table row.

Lenient: anything starting with `|' (after optional leading
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

(defconst agent-shell-markdown--list-item-frontier-regexp
  (rx bol (zero-or-more (any " \t"))
      (or (any "-*+") (seq (one-or-more digit) (opt (any ".)"))))
      (zero-or-more (any " \t"))
      eos)
  "Regexp: a partial list marker at the streaming frontier (buffer end).
Matches a bare or half-typed marker whose trailing space has not
streamed in yet (`-', `*', `1', `1.' at buffer end), unlike
`agent-shell-markdown--list-item-pending-regexp' which requires the
space.  Anchored at `eos', so it matches only the still-incomplete
last line, not a mid-buffer `-foo' or a `---' divider.  Used to hold
off a list's bottom padding so a marker mid-stream does not get a
framing blank stranded above the item it becomes.")

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

LINE-START and LINE-END bound the line being rendered.  MARKER-START
and MARKER-END bound its list marker, and CONTENT-START is where the
item's text begins.

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

(defun agent-shell-markdown--list-line-at-p (pos)
  "Return non-nil when the line holding POS begins a rendered list item.
Tests the line's first char, which is where the bullet / number
glyph and its `agent-shell-markdown-list-rendered' tag sit, so this
holds even for an item rendered before the rest of its line streamed
in."
  (get-text-property (save-excursion (goto-char pos)
                                     (line-beginning-position))
                     'agent-shell-markdown-list-rendered))

(defun agent-shell-markdown--whitespace-normalization-start (watermark)
  "Return where whitespace normalization may start scanning for WATERMARK.

Normalization is the pair of passes adjusting the blank lines between
rendered blocks: `--collapse-list-blank-lines' removes the gaps inside a
list, and `--pad-rendered-blocks' adds one where a block butts against
prose.

Backs up two block boundaries from WATERMARK, giving up after 50 lines.
Either way, an unsatisfied look-back returns `point-min'.  See
`agent-shell-markdown--whitespace-normalization-boundary-p' for what
bounds a block.

Both passes run widened, so they otherwise scan from `point-min',
costing the whole accumulated body on every streamed chunk (issue
#757).  Content
further above is settled: framing is idempotent, and the passes rewrite
everything below where they start.

Boundaries are untagged blank lines because no rendered block spans one,
so `--pad-regions' cannot mistake the start for the middle of a block (a
fenced block's own blank lines are tagged, and are skipped past).
Starting on the boundary rather than below it also leaves
`--frame-block' a real line above to inspect.  A run of blank lines is
one boundary, not one per line, so a list spaced with several blank
lines still looks back past its previous item.

Two boundaries, not one: a gap between list items that has not been
collapsed yet is itself an untagged blank line, so the first boundary
above the watermark can be that gap, which sits inside the list.
Starting there, `--pad-regions' sees only the newest item, frames it as
a block of its own, and inserts a blank line splitting the list in two.
Two clears that gap and lands above the whole block, and two is enough
because only the newest gap is ever left un-collapsed.

The 50-line cap is a performance guard: a body holding no boundary at
all (an unbroken list, one long line) would otherwise pay a full
backward walk per chunk and then scan from `point-min' anyway, which is
slower than not looking back.

For example, with a list streaming in below prose (`|' marks the
watermark, on the last line):

  Intro line
             <- 2nd boundary, returned
  • A
  • B
             <- 1st boundary, the gap above the newest item
  |- C"
  (save-excursion
    (goto-char (min watermark (point-max)))
    (forward-line 0)
    (let ((remaining 2)
          (budget 50)
          (boundary (point-min)))
      (while (and (> (point) (point-min))
                  (> remaining 0)
                  (> budget 0))
        (setq budget (1- budget))
        (if (not (agent-shell-markdown--whitespace-normalization-boundary-p
                  (point)))
            (forward-line -1)
          (setq remaining (1- remaining))
          ;; Climb to the run's first line: a gap of several blank lines
          ;; is one boundary, and scanning has to start above the whole
          ;; gap rather than partway into it.
          (while (and (> (point) (point-min))
                      (agent-shell-markdown--whitespace-normalization-boundary-p
                       (line-beginning-position 0)))
            (forward-line -1))
          (setq boundary (point))
          (when (> remaining 0)
            (forward-line -1))))
      ;; Fewer boundaries above than asked for, or the budget ran out: the
      ;; look-back is not satisfied, so scan from the top rather than from a
      ;; boundary that may sit below content still due to settle.
      (if (> remaining 0)
          (point-min)
        boundary))))

(defun agent-shell-markdown--whitespace-normalization-boundary-p (pos)
  "Return non-nil when the line holding POS bounds a rendered block.
That is, a blank line carrying none of the tags
`agent-shell-markdown--pad-rendered-blocks' frames, so a fenced block's
own blank lines are not boundaries.  See
`agent-shell-markdown--whitespace-normalization-start'."
  (and (agent-shell-markdown--blank-line-at-p pos)
       (not (seq-some (lambda (property)
                        (get-text-property pos property))
                      '(agent-shell-markdown-source-block-rendered
                        agent-shell-markdown-table-source
                        agent-shell-markdown-list-rendered)))))

(defun agent-shell-markdown--collapse-list-blank-lines (&optional start)
  "Delete blank lines between two rendered list items so a list is tight.

Scans from START, or `point-min' when nil.

A list renders as one group no matter how the source spaced its
items: any blank run sitting directly between two
`agent-shell-markdown-list-rendered' lines is removed.  A blank
bordering non-list text (framing the list off from surrounding
prose) has a list item on only one side and is left in place, so
only the inter-item gaps go.  Runs before
`agent-shell-markdown--pad-rendered-blocks' so the joined items form
one block and get framed as a whole.

For example, the buffer:

  • One

  • Two

becomes:

  • One
  • Two"
  (let ((inhibit-field-text-motion t))
    (save-excursion
      (goto-char (or start (point-min)))
      (while (not (eobp))
        (if (agent-shell-markdown--blank-line-at-p (point))
            ;; At a blank run: find its extent, then delete it only when a
            ;; rendered list line sits on both sides.  After a delete point
            ;; sits on the following list line; either way point has moved
            ;; past the run, so the scan makes progress.
            (let ((blank-start (line-beginning-position)))
              (while (and (not (eobp))
                          (agent-shell-markdown--blank-line-at-p (point)))
                (forward-line 1))
              (when (and (> blank-start (point-min))
                         (agent-shell-markdown--list-line-at-p (1- blank-start))
                         (not (eobp))
                         (agent-shell-markdown--list-line-at-p (point)))
                (delete-region blank-start (point))))
          (forward-line 1))))))

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

(defun agent-shell-markdown--block-end (start property)
  "Return where PROPERTY stops for the block of lines beginning at START.
A block is the run of consecutive lines that each carry PROPERTY,
joined across their line terminators.  A line may carry PROPERTY only
partway (a list item rendered before the rest of its line streamed in
leaves the tail untagged); its whole line still counts, so the next
line is still folded in rather than framed apart.  The result is where
PROPERTY next stops, so a caller can resume scanning there.

For example, with the current buffer holding two PROPERTY-tagged list
lines (the first tagged only up to `(', the rest streamed in later):

  • one (rest)
  • two

called at `• one', this returns the position past `• two', not the
one partway through the first line."
  (let ((end (or (next-single-property-change start property nil (point-max))
                 (point-max))))
    (while (let ((line-end (if (and (< end (point-max))
                                    (not (eq (char-after end) ?\n)))
                               (save-excursion (goto-char end)
                                               (line-end-position))
                             end)))
             (and (< line-end (point-max))
                  (eq (char-after line-end) ?\n)
                  (get-text-property (1+ line-end) property)
                  (setq end (or (next-single-property-change
                                 (1+ line-end) property nil (point-max))
                                (point-max))))))
    end))

(defun agent-shell-markdown--pad-regions (property continues-p &optional start)
  "Frame every block of PROPERTY-tagged lines with blank lines.
CONTINUES-P is forwarded to `agent-shell-markdown--frame-block' to
gate the bottom gap.  A block spans consecutive PROPERTY lines (see
`agent-shell-markdown--block-end'), so a multi-line construct is
framed as a whole rather than split with a blank stranded inside.
Scans from START, or `point-min' when nil; START must sit outside a
tagged block, or its remainder would be framed as if it were one.
See `agent-shell-markdown--pad-rendered-blocks'."
  (save-excursion
    (let ((pos (or start (point-min))))
      (goto-char pos)
      (while (< pos (point-max))
        (if (get-text-property pos property)
            (let* ((end (agent-shell-markdown--block-end pos property))
                   ;; The top insert shifts END; a marker tracks it so the
                   ;; scan resumes just past the framed block.
                   (resume (copy-marker end)))
              (agent-shell-markdown--frame-block pos end continues-p)
              (setq pos (marker-position resume))
              (set-marker resume nil))
          (setq pos (or (next-single-property-change
                         pos property nil (point-max))
                        (point-max))))))))

(defun agent-shell-markdown--pad-rendered-blocks (&optional start)
  "Frame each rendered block with a blank line where it butts against prose.

Scans from START, or `point-min' when nil.

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
     'agent-shell-markdown-source-block-rendered nil start)
    (agent-shell-markdown--pad-regions
     'agent-shell-markdown-table-source
     (lambda () (looking-at-p
                 agent-shell-markdown--table-pending-line-regexp))
     start)
    (agent-shell-markdown--pad-regions
     'agent-shell-markdown-list-rendered
     (lambda ()
       (or (looking-at-p agent-shell-markdown--list-item-pending-regexp)
           (looking-at-p agent-shell-markdown--list-item-frontier-regexp)))
     start)))

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

A rendered table with no extension is skipped, since re-rendering
unchanged source is a no-op.  Tables inside any of AVOID-RANGES are
left untouched."
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
  "Return real pixel width of STR as WINDOW renders it.

Measured in a work buffer, never in WINDOW's own buffer: this runs
hundreds of times per table layout, and measuring in place meant
inserting a probe into what the user is reading, so anything that
exited non-locally in between stranded that probe as visible garbage.
WINDOW is still what STR is measured against — `buffer-text-pixel-size'
temporarily shows the work buffer there, so the window's frame font
applies.

`face-remapping-alist' comes across from WINDOW's buffer because that
is how `text-scale-mode' and `buffer-face-mode' take effect; without
it a text-scaled buffer measures at its unscaled width and every table
in it misaligns.  `display-line-numbers' and the two prefixes are
neutralized for the reason `string-pixel-width' neutralizes them: a
globally enabled line-number gutter would otherwise be counted into
the width (bug#59311).

For example, in a buffer whose font is 10 pixels wide,
\"MMMMMMMMMM\" measures 100, and 170 under `text-scale-mode' +3."
  (let ((remapping (buffer-local-value 'face-remapping-alist
                                       (window-buffer window))))
    (agent-shell-with-work-buffer
      (setq display-line-numbers nil
            line-prefix nil
            wrap-prefix nil)
      (setq-local face-remapping-alist remapping)
      (insert str)
      ;; STR carries the cell's own properties, prefixes included.
      (remove-text-properties (point-min) (point-max)
                              '(line-prefix nil wrap-prefix nil))
      (car (buffer-text-pixel-size nil window t)))))

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
approximation.  WINDOW supplies the font metrics for that pixel
path; without a live one, the `string-width' path is taken.

Mixing the two paths within a column (some rows ASCII-padded, some
pixel-padded) accumulates fractional drift on the right edge of the
column and visibly misaligns the vertical pipes between rows."
  (if (and window
           (window-live-p window)
           (fboundp 'window-text-pixel-size)
           (display-graphic-p)
           (or (not (string-match-p (rx bos (* ascii) eos) str))
               (agent-shell-markdown--text-has-face-p str)))
      ;; TODO: Make this fallback observable.  Discarding the error
      ;; means a broken pixel path silently degrades to char-width
      ;; alignment; stashing the last error in a defvar would be
      ;; enough to diagnose it.
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
\(text scaling, face remapping) get their own ratios.  Lazily
initialized.")

(defun agent-shell-markdown--table-face-width-ratio (face window)
  "Return pixel-width ratio of FACE-styled text vs unfaced text in WINDOW.
A ratio of 1.0 means FACE doesn't affect rendered char width.
Cached per face in the destination buffer.

Ratios are always positive floats, so nil from `gethash' reliably
means \"not cached yet\", no sentinel needed."
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
        ;; TODO: Make this fallback observable.  Discarding the error
        ;; means a broken pixel path silently degrades to char-width
        ;; alignment; stashing the last error in a defvar would be
        ;; enough to diagnose it.
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
pixel-accurate measurement, which needs a live WINDOW for its font
metrics.  Mixing the two paths within a column accumulates
fractional drift between rows and visibly misaligns the right-edge
pipes.

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
      ;; TODO: Make this fallback observable.  Discarding the error
      ;; means a broken pixel path silently degrades to char-width
      ;; alignment; stashing the last error in a defvar would be
      ;; enough to diagnose it.
      (condition-case nil
          (let* ((char-px (agent-shell-markdown--table-char-pixel-width window))
                 (target-px (* width char-px))
                 (content-px (agent-shell-markdown--table-measure-string str window))
                 (pad-px (- target-px content-px)))
            (if (<= pad-px 0)
                ;; Pixel measurement says the content already fills (or
                ;; overflows) its pixel budget.  WIDTH is a character
                ;; count computed for the column, though, so never emit a
                ;; cell narrower than that: fall back to ASCII padding as
                ;; a floor.  Keeps a transient bad measurement during
                ;; streaming (which would otherwise return the cell
                ;; unpadded and misalign the right border) degrading to
                ;; char-accurate alignment instead of none.  A genuinely
                ;; column-filling cell has `string-width' >= WIDTH, so the
                ;; fallback returns it unchanged.
                (agent-shell-markdown--pad-table-string-ascii :str str :width width)
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
  "Pad STR with plain spaces to reach WIDTH columns.

This is the ASCII / fallback path."
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
  "Return the minimum (longest-word) widths per column in PROCESSED-ROWS.

PROCESSED-ROWS is the `:processed-rows' entry from
`agent-shell-markdown--preprocess-table', and WINDOW is used for
measurement as in that function.  Called only when a table needs to be
allocated narrower than its natural total, see
`agent-shell-markdown--render-table-source'."
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
`read-only', application-specific tags like an `agent-shell' block
id) are also carried onto the rendered region, otherwise the
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
      (put-text-property table-start end 'fontified t)
      ;; A table echoes no hint of its own, but the blanket
      ;; `rear-nonsticky' above replaced what a link rendered inside a
      ;; cell left there.  Re-adding `cursor-sensor-functions' is what
      ;; stops that link's hint at its last character, rather than
      ;; answering one position past it.
      (agent-shell-markdown--add-rear-nonsticky
       table-start end 'cursor-sensor-functions))))

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
          ;; Preserve point (`agent-shell-markdown--render-table' moves point
          ;; and can be triggered at arbitrary times).
          (save-excursion
            (dolist (region (nreverse regions))
              (agent-shell-markdown--render-table
               (list (cons :source (nth 2 region))
                     (cons :start (marker-position (nth 0 region)))
                     (cons :end (marker-position (nth 1 region)))))
              (set-marker (nth 0 region) nil)
              (set-marker (nth 1 region) nil)))
          (restore-buffer-modified-p modified))))))

(defun agent-shell-markdown--displayed-window-width ()
  "Return the body pixel width of a window showing the current buffer, or nil.
Nil when the buffer is displayed in no window, so a caller can treat
that as \"size unknown\" and re-measure once it is shown."
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (window-body-width window t)))

(defun agent-shell-markdown--resize-image (start end max-width window-width)
  "Re-size the image on [START, END) to MAX-WIDTH pixels.
Creates a fresh image from the current one's file with the new
`:max-width' (keeping its `:max-height'), swaps it onto the `display'
property, and records WINDOW-WIDTH as the width it is now sized
against.  A no-op unless the region actually holds a file image.

An image already at MAX-WIDTH only gets WINDOW-WIDTH recorded: a
pixel-sized image sees the same MAX-WIDTH at every window width, so
re-creating and flushing it on each resize would be wasted work."
  (when-let* ((image (get-text-property start 'display))
              ((eq (car-safe image) 'image))
              (file (image-property image :file)))
    (when-let* (((not (eql max-width (image-property image :max-width))))
                (resized (create-image
                          file nil nil
                          :max-width max-width
                          :max-height (image-property image :max-height))))
      (image-flush resized)
      (put-text-property start end 'display resized))
    (put-text-property start end
                       'agent-shell-markdown-image-window-width window-width)))

(cl-defun agent-shell-markdown-rerender-images (&key force)
  "Re-size window-relative images whose stored window width no longer matches.

Each image sized against the window carries, on
`agent-shell-markdown-image-width-ratio', how its width tracks the
window: an explicit `{width=N%}' fraction, or the symbol `default'
when it comes from `agent-shell-markdown-image-max-width'.  It also
carries the window pixel width it was sized against on
`agent-shell-markdown-image-window-width' (see
`agent-shell-markdown--replace-images').  For only those images whose
stored width differs from the display, this recomputes `:max-width' at
the current width -- from the stored fraction, or, for `default', by
re-reading `agent-shell-markdown-image-max-width' live so a changed
setting applies.
Images the markup itself sizes in pixels carry no ratio and are left
untouched; an image first sized off-screen or at another width is
re-sized once shown at a different width.

With FORCE non-nil, every tracked image is recomputed even when the
window width hasn't changed, for when the change came from
`agent-shell-markdown-image-max-width' rather than the window (see
`agent-shell-markdown-image-scale-increase').

A no-op when the buffer isn't displayed or every image already matches
the current width, so it is safe to call on display and on resize."
  (when-let* ((window (get-buffer-window (current-buffer) t))
              (width (window-body-width window t)))
    ;; A re-size, not a content change: keep it off the undo list and
    ;; don't flip the buffer's modified flag.
    (let ((inhibit-read-only t)
          (buffer-undo-list t)
          (modified (buffer-modified-p)))
      (save-excursion
        (goto-char (point-min))
        (let (match)
          (while (setq match (text-property-search-forward
                              'agent-shell-markdown-image-width-ratio))
            (let* ((beg (prop-match-beginning match))
                   (ratio (get-text-property
                           beg 'agent-shell-markdown-image-width-ratio)))
              (when (or force
                        (not (eql width (get-text-property
                                         beg 'agent-shell-markdown-image-window-width))))
                (agent-shell-markdown--resize-image
                 beg (prop-match-end match)
                 (if (numberp ratio)
                     (round (* ratio width))
                   (agent-shell-markdown--image-max-width))
                 width))))))
      (restore-buffer-modified-p modified))))

(defun agent-shell-markdown-image-scale-increase ()
  "Widen the image at point, or every image in the buffer, by one step.

With point on an image, only that image widens.  Elsewhere, every
image in the buffer does.  Either way that includes the ones the
markdown sizes itself (`{width=300}' or `{width=50%}'): widening a
whole buffer that visibly skipped some of its images would be no use.

A buffer-wide step moves `agent-shell-markdown-image-max-width'
buffer-locally, for the images following it, and steps each remaining
image on its own size.  Nothing outside the buffer changes: not the
setting, not another buffer's images.

The step is 50 pixels for a pixel width or 0.1 of the window for a
ratio, so 300 becomes 350 and 0.4 becomes 0.5."
  (interactive)
  (agent-shell-markdown--scale-images :direction 1))

(defun agent-shell-markdown-image-scale-decrease ()
  "Narrow the image at point, or every image in the buffer, by one step.

Inverse of `agent-shell-markdown-image-scale-increase', stopping at
50 pixels or a 0.1 ratio."
  (interactive)
  (agent-shell-markdown--scale-images :direction -1))

(defun agent-shell-markdown-image-scale-reset ()
  "Drop sizes set by scaling, restoring the ones the images rendered at.

With point on an image, only that image is restored.  Elsewhere, every
image in the buffer is, and the buffer's own
`agent-shell-markdown-image-max-width' is dropped so the ones
following that setting go back to the configured width.

An image rendered at is the width its markdown asks for (`{width=300}'
or `{width=50%}'), or, for markdown that asks for none, that setting
again: the image resumes following it, so later scaling takes it along.

Undoes `agent-shell-markdown-image-scale-increase' and
`agent-shell-markdown-image-scale-decrease'.  Signals an error in a
buffer holding no image and carrying no scaling of its own, there
being nothing to reset."
  (interactive)
  (unless (or (agent-shell-markdown--reset-image-at-point)
              (agent-shell-markdown--reset-images))
    (user-error "No image to reset")))

(cl-defun agent-shell-markdown--scale-images (&key direction)
  "Step the image at point, or every image in the buffer, by DIRECTION.
DIRECTION is 1 to widen or -1 to narrow.  Falls back to the whole
buffer when point isn't on an image, stepping
`agent-shell-markdown-image-max-width' buffer-locally for the images
following it and each remaining image on its own size.  See
`agent-shell-markdown-image-scale-increase' for the step sizes."
  (unless (agent-shell-markdown--scale-image-at-point :direction direction)
    ;; Buffer-local, so scaling one buffer leaves the user's setting (and
    ;; every other buffer's images) alone.  Stepping reads the effective
    ;; value, so the first step starts from whatever they configured.
    (setq-local agent-shell-markdown-image-max-width
                (if (floatp agent-shell-markdown-image-max-width)
                    (agent-shell-markdown--step-ratio
                     agent-shell-markdown-image-max-width direction)
                  (agent-shell-markdown--step-pixels
                   agent-shell-markdown-image-max-width direction)))
    (agent-shell-markdown--scale-self-sized-images :direction direction)
    (agent-shell-markdown-rerender-images :force t)
    (message "Image width: %s"
             (agent-shell-markdown--describe-image-width
              agent-shell-markdown-image-max-width))))

(cl-defun agent-shell-markdown--scale-self-sized-images (&key direction)
  "Step by DIRECTION the images `agent-shell-markdown-image-max-width' misses.

Those are the ones carrying a size of their own rather than following
that setting: the ones the markdown sizes itself (`{width=300}',
`{width=50%}') and any already stepped with point on them.  A
buffer-wide step has to move each of them itself, or scaling the
buffer would visibly skip them.

A percentage steps its fraction, and is re-sized by the caller's
`agent-shell-markdown-rerender-images'; a pixel width has nothing
tracking it, so it is re-sized here.

A no-op when the buffer isn't displayed, there being no width to
resolve fractions against.

For example, with DIRECTION 1 a `{width=50%}' image becomes 60% of
the window and a `{width=300}' one 350 pixels."
  (when-let* ((window-width (agent-shell-markdown--displayed-window-width)))
    ;; A re-size, not a content change: keep it off the undo list and
    ;; don't flip the buffer's modified flag.
    (let ((modified (buffer-modified-p))
          (inhibit-read-only t)
          (buffer-undo-list t))
      (save-excursion
        (goto-char (point-min))
        (let (match)
          (while (setq match (text-property-search-forward
                              'display nil
                              (lambda (_value property)
                                (eq (car-safe property) 'image))))
            (let* ((start (prop-match-beginning match))
                   (end (prop-match-end match))
                   (ratio (get-text-property
                           start 'agent-shell-markdown-image-width-ratio)))
              ;; `default' images follow the setting stepped by the caller,
              ;; so they're left for its re-render.
              (cond ((numberp ratio)
                     (put-text-property start end
                                        'agent-shell-markdown-image-width-ratio
                                        (agent-shell-markdown--step-ratio
                                         ratio direction)))
                    ((null ratio)
                     (agent-shell-markdown--resize-image
                      start end
                      (agent-shell-markdown--step-pixels
                       (or (image-property (get-text-property start 'display)
                                           :max-width)
                           (agent-shell-markdown--image-max-width))
                       direction)
                      window-width)))))))
      (restore-buffer-modified-p modified))))

(cl-defun agent-shell-markdown--scale-image-at-point (&key direction)
  "Return non-nil after stepping the image at point by DIRECTION.
Nil when point isn't on an image, for the caller to fall back to
scaling the whole buffer.

An image tracking the window (an explicit `{width=N%}', or one
following `agent-shell-markdown-image-max-width' while that is a
ratio) steps its fraction and keeps tracking.  Any other image steps
its pixel width and stops tracking, a pixel width being fixed rather
than a fraction of the window.

Either way the image stops following
`agent-shell-markdown-image-max-width', so the size set here sticks
through window resizes and later scaling of the other images.

For example, with DIRECTION 1 a `{width=300}' image becomes 350
pixels, and a `{width=50%}' one 60% of the window."
  (when-let* ((pos (agent-shell-markdown--image-position-at-point))
              (window-width (agent-shell-markdown--displayed-window-width))
              (bounds (agent-shell-markdown--image-bounds pos)))
    ;; A re-size, not a content change: keep it off the undo list and
    ;; don't flip the buffer's modified flag.
    (let* ((modified (buffer-modified-p))
           (inhibit-read-only t)
           (buffer-undo-list t)
           (ratio (agent-shell-markdown--image-tracked-ratio pos))
           (stepped (if ratio
                        (agent-shell-markdown--step-ratio ratio direction)
                      (agent-shell-markdown--step-pixels
                       (or (image-property (get-text-property pos 'display) :max-width)
                           (agent-shell-markdown--image-max-width))
                       direction))))
      ;; Track the new fraction, or untrack a pixel width: left on
      ;; `default' it would snap back to
      ;; `agent-shell-markdown-image-max-width' on the next resize,
      ;; undoing the step.
      (if ratio
          (put-text-property (car bounds) (cdr bounds)
                             'agent-shell-markdown-image-width-ratio stepped)
        (remove-list-of-text-properties
         (car bounds) (cdr bounds) '(agent-shell-markdown-image-width-ratio)))
      (agent-shell-markdown--resize-image
       (car bounds) (cdr bounds)
       (if (floatp stepped) (round (* stepped window-width)) stepped)
       window-width)
      (restore-buffer-modified-p modified)
      (message "Image at point: %s"
               (agent-shell-markdown--describe-image-width stepped)))
    t))

(defun agent-shell-markdown--reset-image-at-point ()
  "Return non-nil after re-sizing the image at point to its rendered size.
Nil when point isn't on an image, for the caller to fall back to the
whole buffer."
  (when-let* ((pos (agent-shell-markdown--image-position-at-point))
              (window-width (agent-shell-markdown--displayed-window-width))
              (bounds (agent-shell-markdown--image-bounds pos)))
    ;; A re-size, not a content change: keep it off the undo list and
    ;; don't flip the buffer's modified flag.
    (let ((modified (buffer-modified-p))
          (inhibit-read-only t)
          (buffer-undo-list t))
      (message "Image at point reset to %s"
               (agent-shell-markdown--describe-image-width
                (agent-shell-markdown--reset-image
                 (car bounds) (cdr bounds) window-width)))
      (restore-buffer-modified-p modified))
    t))

(defun agent-shell-markdown--reset-images ()
  "Return non-nil after re-sizing every image in the buffer to its rendered size.

Also drops the buffer's own `agent-shell-markdown-image-max-width'
\(see `agent-shell-markdown--scale-images'), so images following that
setting go back to the width it is configured with.

Nil when the buffer holds no image and carries no scaling of its own,
there being nothing to reset, or when it isn't displayed, there being
no width to measure ratios against."
  (when-let* ((window-width (agent-shell-markdown--displayed-window-width)))
    ;; A re-size, not a content change: keep it off the undo list and
    ;; don't flip the buffer's modified flag.
    (let ((modified (buffer-modified-p))
          (inhibit-read-only t)
          (buffer-undo-list t)
          (scaled (local-variable-p 'agent-shell-markdown-image-max-width))
          (count 0))
      ;; Before re-sizing, so `default'-sized images resolve against the
      ;; configured width rather than this buffer's stepped one.
      (kill-local-variable 'agent-shell-markdown-image-max-width)
      (save-excursion
        (goto-char (point-min))
        (let (match)
          (while (setq match (text-property-search-forward
                              'display nil
                              (lambda (_value property)
                                (eq (car-safe property) 'image))))
            (agent-shell-markdown--reset-image
             (prop-match-beginning match) (prop-match-end match) window-width)
            (setq count (1+ count)))))
      (restore-buffer-modified-p modified)
      (cond ((> count 0)
             (message "Reset %d image%s" count (if (= count 1) "" "s"))
             t)
            (scaled
             (message "Image scaling reset")
             t)))))

(defun agent-shell-markdown--reset-image (start end window-width)
  "Re-size the image on [START, END) to its rendered size, returning that width.
WINDOW-WIDTH is the window body width to resolve ratios against.

Restores the sizing `agent-shell-markdown--replace-images' gave the
image: the width its stashed markdown asks for, or, absent one,
tracking `agent-shell-markdown-image-max-width' on `default' again.

For example, an image stashing `![a](a.png){width=300}' is re-sized
to 300 and 300 returned, whatever it had been scaled to; one stashing
`![a](a.png)' goes back to following the setting."
  (let* ((attributes (agent-shell-markdown--image-source-attributes start))
         ;; Mirrors how `agent-shell-markdown--replace-images' picks an
         ;; image's ratio, so a reset image is sized as first rendered.
         (ratio (cond ((map-elt attributes :max-width-ratio))
                      ((map-elt attributes :max-width) nil)
                      (t 'default)))
         (max-width (cond ((numberp ratio) (round (* ratio window-width)))
                          (ratio (agent-shell-markdown--image-max-width))
                          (t (map-elt attributes :max-width)))))
    (if ratio
        (put-text-property start end
                           'agent-shell-markdown-image-width-ratio ratio)
      (remove-list-of-text-properties
       start end '(agent-shell-markdown-image-width-ratio)))
    (agent-shell-markdown--resize-image start end max-width window-width)
    max-width))

(defun agent-shell-markdown--image-source-attributes (position)
  "Return the size attributes the markdown of the image at POSITION asks for.
Parses the `{width= height=}' block off the markup stashed on
`agent-shell-markdown-source', so \"![a](a.png){width=300}\" yields
\\='((:max-width . 300)).  Nil when the image carries no markup or its
markup carries no such block, that is when nothing but
`agent-shell-markdown-image-max-width' constrains its size."
  (when-let* ((source (get-text-property position 'agent-shell-markdown-source))
              ((string-match "{\\([^}\n]*\\)}\\'" source)))
    (agent-shell-markdown--parse-image-attributes (match-string 1 source))))

(defun agent-shell-markdown--next-visible-image ()
  "From point, return the start of the next visible image, or nil.

Images hidden inside collapsed text (their start is `invisible') are
skipped, navigating to one being a jump into text nothing shows.  So
is markup left unrendered, which carries no image to land on.

Mirrors `agent-shell-ui--next-visible-navigatable', for callers
folding images into their item navigation.

For example, with a collapsed body holding the only image ahead,
returns nil; expanded, it returns that image's position."
  (agent-shell-markdown--search-visible
   :property 'display
   :predicate (lambda (value) (eq (car-safe value) 'image))))

(defun agent-shell-markdown--previous-visible-image ()
  "From point, return the start of the previous visible image, or nil.
Skips images hidden inside collapsed text (see
`agent-shell-markdown--next-visible-image')."
  (agent-shell-markdown--search-visible
   :property 'display
   :predicate (lambda (value) (eq (car-safe value) 'image))
   :backwards t))

(defun agent-shell-markdown--next-visible-link ()
  "From point, return the start of the next visible rendered link, or nil.

Links are the `[title](url)' ones the renderer stamps with
`agent-shell-markdown-url' (see
`agent-shell-markdown-link-url-at-point').  Ones hidden inside
collapsed text are skipped, as for
`agent-shell-markdown--next-visible-image'.

For example, in \"see docs and more\" where `docs' renders a link,
returns the position of `docs'."
  (agent-shell-markdown--search-visible
   :property 'agent-shell-markdown-url))

(defun agent-shell-markdown--previous-visible-link ()
  "From point, return the start of the previous visible rendered link, or nil.
Skips links hidden inside collapsed text (see
`agent-shell-markdown--next-visible-link')."
  (agent-shell-markdown--search-visible
   :property 'agent-shell-markdown-url
   :backwards t))

(defun agent-shell-markdown--next-visible-source-block ()
  "From point, return the position of the next visible source block, or nil.

Returns the `⧉' glyph on the block's label, where RET copies the body,
so navigation lands on what can be acted on rather than on the block's
first line.  Blocks hidden inside collapsed text are skipped, as for
`agent-shell-markdown--next-visible-image'.

For example, in a buffer holding one rendered \"python ⧉\" block,
returns the position of its `⧉'."
  (agent-shell-markdown--search-visible
   :property 'agent-shell-markdown-source-block-copy))

(defun agent-shell-markdown--previous-visible-source-block ()
  "From point, return the position of the previous visible source block, or nil.
Skips blocks hidden inside collapsed text (see
`agent-shell-markdown--next-visible-source-block')."
  (agent-shell-markdown--search-visible
   :property 'agent-shell-markdown-source-block-copy
   :backwards t))

(cl-defun agent-shell-markdown--search-visible (&key property (predicate #'identity) backwards)
  "From point, return the start of the nearest visible PROPERTY run, or nil.

PREDICATE is called with the run's PROPERTY value and returns non-nil
for a run worth stopping at, defaulting to stopping at any run
carrying PROPERTY.  Searches forward, or backwards when BACKWARDS is
non-nil.  The run point is already on isn't a match, so repeated calls
advance rather than sticking.  Runs whose start is `invisible' are
skipped, being text nothing shows.

For example, with PROPERTY `display' and a PREDICATE matching image
values, returns the position of the next image, and with PROPERTY
`agent-shell-markdown-table-cell-start' alone the next table cell."
  (catch 'found
    ;; Skipping an invisible run terminates because the search leaves
    ;; point past the run it answered with (at its end going forward, at
    ;; its start going backwards), so each pass has less buffer left to
    ;; cover and eventually answers nil at either end.  Point moving is
    ;; the whole of it: wrap the search in a `save-excursion' and the
    ;; same run matches forever.
    (while t
      (let ((match (funcall (if backwards
                                #'text-property-search-backward
                              #'text-property-search-forward)
                            property nil
                            (lambda (_target run-value)
                              (funcall predicate run-value))
                            t)))
        (unless match
          (throw 'found nil))
        (unless (invisible-p (prop-match-beginning match))
          (throw 'found (prop-match-beginning match)))))))

(defun agent-shell-markdown--image-position-at-point ()
  "Return the position of the image displayed at point, or nil when there's none.
Point counts as on an image when the char it's on displays one or,
so a cursor resting just past an image still counts, when the char
before it does.

For example, with an image displayed over the char at 5, returns 5
with point at either 5 or 6, and nil with point at 7."
  (seq-find (lambda (position)
              (and (>= position (point-min))
                   (< position (point-max))
                   (eq (car-safe (get-text-property position 'display)) 'image)))
            (list (point) (1- (point)))))

(defun agent-shell-markdown--image-bounds (position)
  "Return the (START . END) bounds of the image displayed at POSITION.
The bounds span the text the image is displayed over (its alt text
or path), which is what carries the `display' property.

For example, with an image displayed over the two chars of its alt
text at the start of the buffer, returns (1 . 3)."
  (cons (or (previous-single-property-change (1+ position) 'display) (point-min))
        (or (next-single-property-change position 'display) (point-max))))

(defun agent-shell-markdown--image-tracked-ratio (position)
  "Return the window fraction the image at POSITION tracks, or nil.
That's its `{width=N%}' fraction, or, for an image following
`agent-shell-markdown-image-max-width', that setting when it is a
ratio.  Nil for an image sized in pixels, which doesn't track the
window.

For example, returns 0.5 for a `{width=50%}' image, 0.4 for one
following the setting while it holds 0.4, and nil for a `{width=300}'
one or one following the setting while it holds 300."
  (if (eq (get-text-property position 'agent-shell-markdown-image-width-ratio)
          'default)
      (when (floatp agent-shell-markdown-image-max-width)
        agent-shell-markdown-image-max-width)
    (get-text-property position 'agent-shell-markdown-image-width-ratio)))

(defun agent-shell-markdown--step-ratio (ratio direction)
  "Return RATIO stepped by 0.1 in DIRECTION, within 0.1 and the full window.
DIRECTION is 1 to widen or -1 to narrow, so 0.4 steps to 0.5 and
0.4 steps back to 0.3."
  ;; Re-round to a tenth so repeated steps stay on 0.1 boundaries
  ;; rather than drifting with float error.
  (min 1.0 (max 0.1 (/ (round (* 10 (+ ratio (* direction 0.1)))) 10.0))))

(defun agent-shell-markdown--step-pixels (pixels direction)
  "Return PIXELS stepped by 50 in DIRECTION, no narrower than 50.
DIRECTION is 1 to widen or -1 to narrow, so 300 steps to 350 and
300 steps back to 250."
  (max 50 (+ pixels (* direction 50))))

(defun agent-shell-markdown--describe-image-width (width)
  "Return WIDTH described for the echo area.
A ratio reads as a percentage of the window (0.5 as \"50% of
window\") and an integer as pixels (350 as \"350px\")."
  (if (floatp width)
      (format "%d%% of window" (round (* 100 width)))
    (format "%dpx" width)))

(defun agent-shell-markdown--carry-properties (pos)
  "Return a plist of properties at POS to carry across our delete+insert.

Filters out properties our rendering itself sets (`face',
`font-lock-face', `agent-shell-markdown-frozen',
`agent-shell-markdown-table-source', `agent-shell-markdown-source',
`rear-nonsticky', `keymap', `cursor-sensor-functions') so callers'
application-level properties such as `read-only' or `agent-shell'
block ids survive on the rendered output.

The three that are ours but not obviously so must not be carried,
since the first char's value would be spread uniformly across the
re-rendered region: `font-lock-face' (e.g. a table border) greys out
the per-cell styling, while `keymap' and `cursor-sensor-functions' (a
link's own, say) take RET and the hint away from a link rendered
inside a cell.  Each is re-established after the insert anyway."
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
                            rear-nonsticky
                            keymap
                            cursor-sensor-functions))
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
there's no cell in that direction, that is at the table's edges (its
last cell going forward, its first going backward) and outside a table."
  (if-let* ((cells (agent-shell-markdown-table--cell-starts))
            ;; Cells at or before point, the last of them being the one
            ;; point is in.  None of them when point sits ahead of the
            ;; first cell (on the table's top border, say), so
            ;; `:forward' lands on that first cell.
            (target (+ (seq-count (lambda (cell)
                                    (<= cell (point)))
                                  cells)
                       (if (eq direction :forward) 0 -2)))
            ((<= 0 target))
            (position (nth target cells)))
      (goto-char position)
    (user-error "No more cells left")))

(cl-defun agent-shell-markdown-table--entry-position (&key position from)
  "Return where navigating to POSITION lands, with point coming FROM.

That's the first cell of the table POSITION sits in, a table being
entered at its first cell whatever part of one navigation found (a link
rendered in its third cell, say).  POSITION comes back unchanged when
it's outside any table, and when it's inside the same table as FROM,
whose cells and the links in them navigation walks one by one.

For example, with POSITION on a link in the last cell of a table,
returns that table's first cell for a FROM ahead of the table, and the
link itself for a FROM already inside it."
  (or (when-let* ((table (save-excursion
                           (goto-char position)
                           (agent-shell-markdown-table--region-at-point)))
                  ((not (and (<= (car table) from)
                             (< from (cdr table))))))
        (agent-shell-markdown-table--first-cell position))
      position))

(defun agent-shell-markdown-table--first-cell (position)
  "Return the first navigable cell of the table at POSITION, or nil.
Nil when POSITION isn't inside a rendered table.

A table's entry point (see
`agent-shell-markdown-table--entry-position'), and where
`agent-shell-backward-up-item' returns point to from whichever cell
navigation walked into."
  (save-excursion
    (goto-char position)
    (seq-first (agent-shell-markdown-table--cell-starts))))

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
styling in buffers that fontify continuously (`comint', `shell-maker',
`agent-shell', etc.).  `font-lock-face' is the property reserved
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
        (let ((inhibit-message t))
          (delay-mode-hooks
            (funcall mode)
            (font-lock-ensure)))
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
  "Return a sparse keymap binding RET and \\`mouse-1' to FUN."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") fun)
    (define-key map [mouse-1] fun)
    (define-key map [remap self-insert-command] 'ignore)
    map))

(cl-defun agent-shell-markdown--action-key (&key action keymap)
  "Return the key running ACTION, as a `key-description' string.

Looks ACTION up in KEYMAP, or in the buffer's active keymaps when
KEYMAP is nil (which is where mode-level bindings like image resizing
live).  Skips mouse buttons, so the result is something the user can
press, and returns nil when ACTION has no such binding.

ACTION is anything `where-is-internal' takes as a definition, so a
per-image closure works as well as a command symbol.

For example, ACTION `agent-shell-image-scale-increase' returns \"+\"
in an `agent-shell-mode' buffer, and nil elsewhere."
  (when-let* ((keys (seq-remove (lambda (key)
                                  (mouse-event-p (seq-first key)))
                                (where-is-internal
                                 action (when keymap (list keymap))))))
    (key-description (seq-first keys))))

(cl-defun agent-shell-markdown--action-hint (&key action keymap verb)
  "Return \"Press KEY to VERB\", KEY being what runs ACTION in KEYMAP.

Returns nil where ACTION is bound to no key, so callers stay quiet
rather than name a key that does nothing.

For example, VERB \"copy\" returns \"Press RET to copy\" on a source
block's label."
  (when-let* ((key (agent-shell-markdown--action-key :action action
                                                     :keymap keymap)))
    (format "Press %s to %s" key verb)))

(defun agent-shell-markdown--put-hint-sensor (start end hint)
  "Echo HINT as the cursor enters [START, END).

Puts HINT's sensor (see `agent-shell-markdown--make-hint-sensor') and
marks `cursor-sensor-functions' rear-nonsticky across the span, which
is what stops the hint at the span's last character.

`cursor-sensor' reads the property with `get-pos-property' before
falling back to `get-char-property', and `get-pos-property' reports
what text inserted at a position would inherit rather than what sits
there.  A sticky value therefore still answers one position past the
end, showing a link's hint while point is on the space after it."
  (put-text-property start end 'cursor-sensor-functions
                     (agent-shell-markdown--make-hint-sensor hint))
  (agent-shell-markdown--add-rear-nonsticky start end 'cursor-sensor-functions))

(defun agent-shell-markdown--add-rear-nonsticky (start end property)
  "Mark PROPERTY rear-nonsticky between START and END.

Adds to whatever `rear-nonsticky' is already there rather than
replacing it, the span typically carrying others (a table's own, say)
whose stickiness would otherwise be dropped."
  (let ((pos start))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'rear-nonsticky nil end))
            (sticky (get-text-property pos 'rear-nonsticky)))
        ;; `t' already means every property is rear-nonsticky.
        (unless (eq sticky t)
          (put-text-property pos next 'rear-nonsticky
                             (cons property
                                   (delq property (copy-sequence sticky)))))
        (setq pos next)))))

(defun agent-shell-markdown--make-hint-sensor (hint)
  "Return a `cursor-sensor-functions' value echoing HINT on entry.

HINT is a function returning the text to echo, or nil to stay quiet.
It runs as the cursor enters the propertized text, so the hint names
the keys bound then rather than the ones bound at render time.
Echoing needs `cursor-sensor-mode' (which `agent-shell-ui-mode' turns
on).

For example, a HINT returning \"Press RET to copy\" echoes that line
as the cursor lands on a source block's label, while one returning
nil (its key is unbound there) leaves the echo area alone."
  (list (lambda (_window _old-pos sensor-action)
          (when (eq sensor-action 'entered)
            (when-let* ((text (funcall hint)))
              (message "%s" text))))))

(cl-defun agent-shell-markdown--image-hint (&key action keymap)
  "Return the hint text for an image ACTION opens via KEYMAP.

Names the key that opens the image, followed by whichever of the keys
that widen, narrow and reset it are bound here.  Resizing is bound at
mode level (see `agent-shell-viewport-view-mode-map'), so an image
carries no keys of its own for it -- the hint is what makes it
discoverable.  Returns nil when not even ACTION is bound.

Each resizing action is looked up under either name it goes by:
`agent-shell-mode' binds its own wrappers, so `+' still self-inserts
while typing a prompt, where the viewport binds the commands below
directly.

For example, this returns \"Press RET to open image, +/-/0 to
resize\" in a shell buffer, and \"Press RET to open image\" where
resizing isn't bound."
  (when-let* ((open-hint (agent-shell-markdown--action-hint :action action
                                                            :keymap keymap
                                                            :verb "open image")))
    (concat
     open-hint
     (when-let* ((scale-keys (seq-keep
                              (lambda (names)
                                (seq-some (lambda (name)
                                            (agent-shell-markdown--action-key
                                             :action name))
                                          names))
                              '((agent-shell-image-scale-increase
                                 agent-shell-markdown-image-scale-increase)
                                (agent-shell-image-scale-decrease
                                 agent-shell-markdown-image-scale-decrease)
                                (agent-shell-image-scale-reset
                                 agent-shell-markdown-image-scale-reset)))))
       (format ", %s to resize" (string-join scale-keys "/"))))))

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

(defun agent-shell-markdown-open-file (path)
  "Open PATH in the current window, returning the window showing it.

Reuses a window already showing PATH rather than displacing the current
one, a file link usually pointing at something already on screen.

The default `agent-shell-markdown-open-file-function'."
  (if-let* ((window (when (get-file-buffer path)
                      (get-buffer-window (get-file-buffer path)))))
      (select-window window)
    (find-file path)
    (selected-window)))

(defvar agent-shell-markdown-open-file-function #'agent-shell-markdown-open-file
  "Function opening the file a link points at.

Called with the file's path, having decided the file opens in Emacs
\(binary files the operating system handles never reach it).  It is
free to display the file however it likes, and must return the window
showing it, or nil when it displayed nothing.

That window is where point lands for a link naming a line, so a
function returning the buffer instead leaves point where the user
can't see it, and is rejected (see
`agent-shell-markdown-visit-file').

For example, to open beside the shell rather than over it:

  (setq agent-shell-markdown-open-file-function
        (lambda (path)
          (display-buffer (find-file-noselect path)
                          \\='(display-buffer-pop-up-window))))")

(cl-defun agent-shell-markdown-visit-file (&key file line-start line-end)
  "Visit FILE, selecting lines LINE-START to LINE-END when given.

Opens FILE through `agent-shell-markdown-open-file-function', which
owns where it is displayed.  Without LINE-START, that is all.  With
one, point lands on that line in the window that function returns, and
LINE-END selects through the end of its line.  Messages instead when
FILE no longer exists.

Where the link was followed from goes on xref's marker stack, so
\\[xref-go-back] comes back to it, as it does from any other jump.

Returns the window FILE was shown in, or nil when it wasn't shown.

For example, with FILE holding \"one\\ntwo\\nthree\\nfour\",
LINE-START 2 and LINE-END 3 leave \"two\\nthree\" as the region."
  (if (and line-start (not (and file (file-exists-p file))))
      (message "File not found")
    (when-let* ((origin (point-marker))
                (window (agent-shell-markdown--open-file file)))
      ;; Taken before opening, since by now point has moved to FILE, and
      ;; pushed only once FILE is shown: a link that opened nothing would
      ;; otherwise offer a way back to the spot the reader never left.
      (xref-push-marker-stack origin)
      (when line-start
        (with-selected-window window
          (goto-char (point-min))
          (forward-line (1- line-start))
          (when line-end
            (push-mark (save-excursion
                         (goto-char (point-min))
                         (forward-line (1- line-end))
                         (end-of-line)
                         (point))
                       t t))))
      window)))

(defun agent-shell-markdown--open-file (path)
  "Return the window PATH was opened in, or nil when none was shown.
Opens via `agent-shell-markdown-open-file-function', erroring when that
returns something other than a window, which would otherwise silently
leave point unmoved."
  (let ((window (funcall agent-shell-markdown-open-file-function path)))
    (unless (or (null window) (windowp window))
      (user-error "`agent-shell-markdown-open-file-function' must return the window showing the file (got %S)"
                  window))
    window))

(defun agent-shell-markdown--open-local-link (url)
  "Open URL as a local file link if possible.
Return non-nil if handled, nil otherwise.

Text/navigable files open in Emacs, jumping to the `#Lnnn' line when URL
carries one and selecting the lines when it carries a `#Lnnn-Lnnn'
range (see `agent-shell-markdown-visit-file').  Binary files (which
Emacs can't usefully display) instead prompt to open with the operating
system's default program, ignoring any line (a line number is
meaningless for binary)."
  (when-let* ((parsed (agent-shell-markdown--parse-local-link url)))
    (if (agent-shell-markdown--binary-file-p (map-elt parsed :file))
        (agent-shell-markdown--open-externally (map-elt parsed :file))
      (agent-shell-markdown-visit-file
       :file (map-elt parsed :file)
       :line-start (map-elt parsed :line-start)
       :line-end (map-elt parsed :line-end)))
    t))

(defconst agent-shell-markdown--lines-regexp
  (rx (one-or-more digit)
      (optional "-" (optional "L") (one-or-more digit)))
  "Regexp for the line part of a local link, a number or a range.
Matches \"10\", \"10-24\" and GitHub's \"10-L24\".")

(defun agent-shell-markdown--parse-lines (lines)
  "Parse LINES, the line part of a local link, into a (START . END) cons.

END is nil unless LINES names a range.

For example:

  \"10\"     => (10)
  \"10-24\"  => (10 . 24)
  \"10-L24\" => (10 . 24)"
  (when (string-match (rx bos (group (one-or-more digit))
                          (optional "-" (optional "L")
                                    (group (one-or-more digit)))
                          eos)
                      lines)
    (cons (string-to-number (match-string 1 lines))
          (when (match-string 2 lines)
            (string-to-number (match-string 2 lines))))))

(defun agent-shell-markdown--parse-local-link (url)
  "Parse URL as a local file link.

Return an alist with :file, :line-start and :line-end when URL points to
an existing local file, or nil otherwise.  Both line keys are nil when
URL names no line, and :line-end is nil unless it names a range.

For example, with the file part abbreviated to F:

  \"foo.el#L10\"          => ((:file . F) (:line-start . 10) (:line-end))
  \"foo.el#L10-L24\"      => ((:file . F) (:line-start . 10) (:line-end . 24))
  \"foo.el#L10-24\"       => ((:file . F) (:line-start . 10) (:line-end . 24))
  \"foo.el\"              => ((:file . F) (:line-start) (:line-end))
  \"file:bar.el:5-8\"     => ((:file . F) (:line-start . 5) (:line-end . 8))
  \"https://example.com\" => nil"
  (when-let* ((match
               (cond
                ((string-match
                  (rx bos "file://"
                      (group (+? anything))
                      (optional (or (seq "#L" (group (regexp agent-shell-markdown--lines-regexp)))
                                    (seq ":" (group (regexp agent-shell-markdown--lines-regexp)))))
                      eos)
                  url)
                 (cons (match-string 1 url)
                       (or (match-string 2 url) (match-string 3 url))))
                ((string-match
                  (rx bos "file:"
                      (group (not (any "/")) (+? anything))
                      (optional (or (seq "#L" (group (regexp agent-shell-markdown--lines-regexp)))
                                    (seq ":" (group (regexp agent-shell-markdown--lines-regexp)))))
                      eos)
                  url)
                 (cons (match-string 1 url)
                       (or (match-string 2 url) (match-string 3 url))))
                ((string-match
                  (rx bos
                      (group (? (optional "/") alpha ":/")
                             (one-or-more (not (any ":#"))))
                      "#L" (group (regexp agent-shell-markdown--lines-regexp))
                      eos)
                  url)
                 (cons (match-string 1 url) (match-string 2 url)))
                ((string-match
                  (rx bos
                      (group (? (optional "/") alpha ":/")
                             (one-or-more (not (any ":#"))))
                      ":" (group (regexp agent-shell-markdown--lines-regexp))
                      eos)
                  url)
                 (cons (match-string 1 url) (match-string 2 url)))
                ((not (string-empty-p url))
                 (cons url nil))))
              (filepath (expand-file-name (car match))))
    (when (file-exists-p filepath)
      (let ((lines (when (cdr match)
                     (agent-shell-markdown--parse-lines (cdr match)))))
        (list (cons :file filepath)
              (cons :line-start (car lines))
              (cons :line-end (cdr lines)))))))

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

(cl-defun agent-shell-markdown--resolve-image-url (url &key image-cache-directory allow-bare-relative)
  "Resolve image URL to an absolute local file path, or nil.
Handles http(s) URLs (downloaded into IMAGE-CACHE-DIRECTORY and cached via
`agent-shell-markdown--fetch-remote-image'; not fetched when
IMAGE-CACHE-DIRECTORY is nil), file:// URIs, absolute paths, and paths
starting with `~/', `./', or `../'.

ALLOW-BARE-RELATIVE additionally accepts a path naming no directory at
all, such as `logo.png', resolved against `default-directory'.  Off by
default, and only `![alt](url)' markup asks for it: that markup says an
image is meant, whereas a bare path line is just a line that happens to
end in an image extension, which prose does all the time.  A url with a
scheme (`data:', `mailto:') is never taken for a path.

For example, with URL \"logo.png\" and a logo.png in `default-directory',
ALLOW-BARE-RELATIVE nil returns nil while non-nil returns its absolute
path."
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
                        url)
                       ((and allow-bare-relative
                             (not (string-match-p (rx bos alpha
                                                      (zero-or-more (any alnum "+-."))
                                                      ":")
                                                  url)))
                        url)))
                (expanded (expand-file-name path))
                ((file-exists-p expanded)))
      expanded)))

(defun agent-shell-markdown--image-max-width ()
  "Return the maximum image width in pixels.
Resolves `agent-shell-markdown-image-max-width' which may be an integer
\(pixels) or a float between 0 and 1 (ratio of window body width)."
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

(defun agent-shell-markdown--image-attribute-ratio (value)
  "Return VALUE's window fraction when it is a percentage, else nil.
VALUE is a Pandoc size string such as `50%', `300', or `300px'.  A
percentage yields its fraction of the window (0.5 for `50%'); a
pixel or bare-number value yields nil, since it is a fixed size that
does not track the window.

For example, (agent-shell-markdown--image-attribute-ratio \"50%\")
returns 0.5 and (agent-shell-markdown--image-attribute-ratio \"300\")
returns nil."
  (when (string-match "\\`\\([0-9]+\\)%\\'" value)
    (/ (string-to-number (match-string 1 value)) 100.0)))

(defun agent-shell-markdown--parse-image-attributes (string)
  "Parse Pandoc-style image attributes from STRING (a `{...}' body).
Returns an alist.  For any `width='/`height=' entry it holds
`:max-width'/`:max-height' pixel values (resolved via
`agent-shell-markdown--image-attribute-pixels') and, when the entry
was a percentage, `:max-width-ratio'/`:max-height-ratio' with its
window fraction so the image can be re-sized when the window changes.
Other attributes (classes, ids) are ignored.

For example, (agent-shell-markdown--parse-image-attributes \"width=50%\")
returns \\='((:max-width . PIXELS) (:max-width-ratio . 0.5)), and
\"width=300\" returns \\='((:max-width . 300))."
  (let (attributes)
    (dolist (dimension '(width height))
      (when-let* (((string-match (format "\\_<%s[ \t]*=[ \t]*\\([^ \t}]+\\)"
                                         dimension)
                                 string))
                  (value (match-string 1 string)))
        (when-let* ((pixels (agent-shell-markdown--image-attribute-pixels
                             value dimension)))
          (push (cons (intern (format ":max-%s" dimension)) pixels)
                attributes))
        (when-let* ((ratio (agent-shell-markdown--image-attribute-ratio value)))
          (push (cons (intern (format ":max-%s-ratio" dimension)) ratio)
                attributes))))
    (nreverse attributes)))

(defun agent-shell-markdown--watermark-start ()
  "Return the position the next scan should start from.

Reads the `agent-shell-markdown-watermark' text property off the
first character.  When absent or out of range, returns
`point-min' (whole-buffer scan — the conservative default for the
first call or after the watermark anchor has been rewritten away).

The property holds the frontier as an offset from the character it
sits on, not as a buffer position, so it survives the text moving:
rendering anything above shifts every position below it, and a
stored position would then point somewhere else entirely (a scan
could start mid-markup and skip it).  An offset also keeps meaning
when callers shuttle the text into another buffer via
`agent-shell-markdown-convert'."
  (let ((stored (and (> (point-max) (point-min))
                     (get-text-property (point-min)
                                        'agent-shell-markdown-watermark))))
    (if (and (integerp stored)
             (>= stored 0)
             (<= stored (- (point-max) (point-min))))
        (+ (point-min) stored)
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
\(the last block whose `:block' end is still `point-max') is read
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
        ;; Stored as an offset from the char it sits on, so it still means
        ;; the same place after the text moves (see
        ;; `agent-shell-markdown--watermark-start').
        (put-text-property (point-min) (1+ (point-min))
                           'agent-shell-markdown-watermark
                           (- frontier (point-min)))))))

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
