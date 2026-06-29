;;; agent-shell-markdown-math.el --- Display-math rendering for agent-shell -*- lexical-binding: t -*-

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
;; Display-math support for `agent-shell-markdown': intercept LaTeX
;; display equations in agent output and overlay them with an image.
;;
;; Two block-level delimiter styles are recognized, toggled
;; independently via `agent-shell-markdown-math-delimiters':
;;
;;   bracket  `\[X\]'    (default; unambiguous)
;;   dollar   `$$X$$'    (default; safe because matched block-level only)
;;
;; Inline math `\(X\)' is recognized separately (toggle
;; `agent-shell-markdown-math-render-inline', default on) and typeset
;; in text style.  Inline `$X$' is intentionally not matched — a lone
;; `$' is too common in prose to be safe.
;;
;; The raw LaTeX is kept in the buffer (so copy / save round-trips the
;; source) and, on a graphical display, an equation image is layered on
;; top with a `display' text property.  A blank line can't appear inside
;; LaTeX display math, so a candidate block whose body would span one is
;; rejected — this bounds detection and stops a stray delimiter from
;; swallowing the rest of a streaming response.
;;
;; `agent-shell-markdown--style-math-blocks' is run as a pass by
;; `agent-shell-markdown-replace-markup'; the other functions support
;; it and the renderer's avoid-range / watermark bookkeeping.
;;
;; Equations are typeset by compiling a standalone LaTeX document to DVI
;; (`latex') and converting it to SVG (`dvisvgm') — the same toolchain
;; org-latex-preview uses.  Compilation is asynchronous and the SVG is
;; cached on disk by content (so each unique equation compiles at most
;; once); the image is overlaid when ready.  When the toolchain is
;; absent or `agent-shell-markdown-math-use-placeholder' is set, a
;; placeholder panel boxing the raw LaTeX is shown instead.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'color)
(require 'map)
(require 'org-faces)
(require 'seq)
(require 'svg)

(declare-function agent-shell-markdown--in-avoid-range-p "agent-shell-markdown")
(declare-function agent-shell--cache-dir "agent-shell")

(defface agent-shell-markdown-math
  '((t :inherit font-lock-constant-face))
  "Face applied to rendered display-math source.
On a graphical display the source is hidden behind an equation
image; this face is the fallback styling for the raw LaTeX shown
on a non-graphical display."
  :group 'agent-shell-markdown)

(defconst agent-shell-markdown--math-delimiters
  '((dollar . ("$$" . "$$"))
    (bracket . ("\\[" . "\\]")))
  "Map of display-math delimiter styles to their (OPEN . CLOSE) tokens.
`dollar' is `$$...$$'; `bracket' is `\\[...\\]'.  The keys of this
map are the values accepted in `agent-shell-markdown-math-delimiters'.")

(defconst agent-shell-markdown--math-inline-open "\\("
  "Opening delimiter for inline math (a literal backslash and paren).")

(defconst agent-shell-markdown--math-inline-close "\\)"
  "Closing delimiter for inline math (a literal backslash and paren).")

(defvar agent-shell-markdown-math-delimiters '(bracket dollar)
  "Display-math delimiter styles recognized when rendering markdown.

A list whose members are keys of
`agent-shell-markdown--math-delimiters':

  `bracket'  recognize `\\[...\\]'
  `dollar'   recognize `$$...$$'

The two are independent — add or drop one to toggle it.  An empty
list disables the delimiter styles (fenced math via
`agent-shell-markdown-math-fence-languages' is separate); the
master switch `agent-shell-markdown-render-math' disables
everything.

Both styles are matched only as block-level equations: the opener
must start its line (after optional indentation) and the closer
must be flush — either start or end its line.  Genuinely inline
display math is therefore not recognized (agents don't emit it,
and truly inline math should use `\\(...\\)' / `$...$', which are
left untouched).  That anchoring makes `$$' safe enough to enable
by default; set to \\='(bracket) to drop it if `$$' still
collides with your prose.")

(defvar agent-shell-markdown-render-math nil
  "Master switch for rendering display math in agent responses.

Nil (the default) disables math rendering entirely — delimiters
and fenced math blocks are left as plain text / ordinary code
blocks.  Set non-nil to opt in; what then gets recognized is
controlled by `agent-shell-markdown-math-delimiters' (`\\[...\\]'
on by default, `$$...$$' opt-in) and
`agent-shell-markdown-math-fence-languages' (`math' / `latex' /
`tex' fenced blocks, on by default).

Consumed as the default of the `render-math' keyword of
`agent-shell-markdown-replace-markup'.")

(defvar agent-shell-markdown-math-fence-languages '("math" "latex" "tex")
  "Fenced-code-block languages rendered as display math.

A fenced block whose info string is one of these (compared
case-insensitively), e.g.

  ```math
  E = mc^2
  ```

is typeset as an equation instead of shown as a code block — but
only when `agent-shell-markdown-render-math' is non-nil.  Several
agents emit `math'/`latex' fences (GitHub renders ```math as
display math), so this complements the `\\[...\\]' / `$$...$$'
delimiter styles.  Set to nil to leave such fences as code.")

(defvar agent-shell-markdown-math-render-inline t
  "When non-nil, recognize inline math `\\(...\\)' in agent responses.

Only effective when the master switch
`agent-shell-markdown-render-math' is non-nil.  Inline math is
typeset in text style (no `\\displaystyle') and overlaid in place,
so it sits within the surrounding line rather than on its own.

Unlike the block-level delimiters, `\\(...\\)' is matched anywhere
on a line (it is inline by nature), but its body may not cross a
line break — the closer must appear on the same line as the
opener, which bounds the match and keeps a stray `\\(' from
swallowing the rest of a streaming response.

Inline `$...$' is deliberately NOT recognized: a lone `$' is far
too common in prose, currency, and shell snippets to match safely.
Only the unambiguous `\\(...\\)' form is detected; `$...$' support
can be added later if agents prove to need it.")

(defvar agent-shell-markdown-math-use-placeholder nil
  "When non-nil, draw the placeholder panel instead of typesetting LaTeX.
Also used as the automatic fallback when the LaTeX toolchain
\(`agent-shell-markdown-math-latex-program' /
`agent-shell-markdown-math-dvisvgm-program') is unavailable.")

(defvar agent-shell-markdown-math-render-on-non-graphic nil
  "When non-nil, render equation images even on a non-graphical frame.

By default equations are only compiled when the selected frame is
graphical (`display-graphic-p').  In an Emacs daemon a buffer may
be rendered while a TTY frame is selected, yet later viewed in a
graphical frame; without this the equation would never have been
produced and stays raw text in the GUI too.

Set non-nil (typically in a daemon setup) to always compile the
SVG when the build supports it: it is ignored on a TTY frame (the
raw LaTeX shows) but appears as soon as a graphical frame views
the buffer.  The trade-off is that a purely terminal session then
spawns LaTeX compiles whose images it never displays.")

(defvar agent-shell-markdown-math-latex-program "latex"
  "Program that compiles a LaTeX document to DVI.")

(defvar agent-shell-markdown-math-dvisvgm-program "dvisvgm"
  "Program that converts DVI to SVG.")

(defvar agent-shell-markdown-math-font-scale 1.0
  "Size of rendered equations relative to the buffer font.

Equation images are scaled so LaTeX's 10pt body font maps onto the
buffer's font height; this multiplier rides on top of that match.
1.0 makes equation text the same size as the surrounding text;
greater than 1 enlarges, less than 1 shrinks.  Because the match is
recomputed from the current font, equations track the buffer font
across themes and faces (run `agent-shell-markdown-math-refresh'
after a pure font-size change — see its docstring).")

(defvar agent-shell-markdown-math--svg-px-per-pt nil
  "Cached pixels-per-point Emacs uses to render SVG images.
Measured once on a graphical frame by
`agent-shell-markdown--math-svg-px-per-pt' (so HiDPI / image
scaling is captured exactly); nil until then.")

(defvar agent-shell-markdown-math-preamble
  "\\documentclass[border=2pt]{standalone}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{xcolor}"
  "LaTeX preamble (everything before `\\begin{document}') for equations.
The `standalone' class crops the page tightly to the equation, so
no `preview' package is required.  `xcolor' is used to tint the
equation to match the buffer foreground.  Equations are typeset as
`\\displaystyle' inline math inside the document body.

See also `agent-shell-markdown-math-appended-preamble' for adding
extra packages without replacing this base.")

(defvar agent-shell-markdown-math-appended-preamble ""
  "Extra LaTeX code appended after `agent-shell-markdown-math-preamble'.
Use this to load additional packages (e.g. `\\\\usepackage{braket}',
`\\\\usepackage{physics}') without replacing the base preamble.
The value is folded into the cache key, so changing it
automatically invalidates cached SVGs.")

(defvar agent-shell-markdown-math-cache-directory nil
  "Directory for cached equation SVGs and scratch compiles.
When nil, agent-shell's shared cache directory is used (via
`agent-shell--cache-dir'), so equation SVGs persist across sessions
alongside agent-shell's other cached assets and each unique
equation compiles at most once ever.

That helper lives in `agent-shell.el', which is always loaded in a
real session.  The renderer's test harness loads this module
without `agent-shell.el'; set this variable there (or stub
`agent-shell--cache-dir') if a code path needs the directory.")

;; image-cache key = content key (sha1 of latex + color + scale + preamble +
;; inline) plus the display scale, via `--math-image-cache-key'.  Folding the
;; scale in lets images at different font sizes coexist, so a font change just
;; adds an entry (no cache clear) and sibling buffers' warm images survive.
;; The underlying SVG is still compiled at most once per content key (the disk
;; cache is font-independent); only the cheap `create-image' is per scale.
(defvar agent-shell-markdown-math--image-cache (make-hash-table :test 'equal)
  "In-memory map of image-cache key to rendered equation image.")

;; key -> list of (BUFFER START-MARKER END-MARKER) awaiting one in-flight
;; compile.  Dedupes concurrent compiles of the same equation and records
;; every region to overlay once the SVG is ready.
(defvar agent-shell-markdown-math--pending (make-hash-table :test 'equal)
  "In-memory map of cache key to regions awaiting an in-flight compile.")

(defvar-local agent-shell-markdown-math--rendered-appearance nil
  "The appearance signature this buffer's equations were rendered for.
A list (FOREGROUND BACKGROUND FONT-HEIGHT) — see
`agent-shell-markdown-math--current-appearance'.  Buffer-local:
each buffer tracks its own last-rendered appearance, so a refresh
can re-render just the affected buffer and leave the others to
re-render lazily when they are next displayed (see
`agent-shell-markdown-math--refresh-if-changed').  Updated whenever
an equation renders.")

(defvar-local agent-shell-markdown-math--present nil
  "Non-nil in a buffer that has rendered display-math regions.
Lets `agent-shell-markdown-math-refresh' visit only relevant buffers.")

(defun agent-shell-markdown-math--current-colors ()
  "Return the (FOREGROUND . BACKGROUND) equations should render for now.
Both are `#rrggbb' strings resolved from the `default' face of the
selected frame."
  (cons (agent-shell-markdown--svg-color 'default :foreground "#000000")
        (agent-shell-markdown--svg-color 'default :background "#ffffff")))

(defun agent-shell-markdown-math--current-appearance ()
  "Return the appearance signature equations should render for now.
A list (FOREGROUND BACKGROUND FONT-HEIGHT): the colors equations
are tinted with (see `agent-shell-markdown-math--current-colors')
and the buffer font pixel height they are sized to (nil off a
graphical frame).  Comparing this against
`agent-shell-markdown-math--rendered-appearance' detects a color
*or* font-size change so the lazy refresh can re-render."
  (let ((colors (agent-shell-markdown-math--current-colors)))
    (list (car colors) (cdr colors)
          (and (display-graphic-p) (ignore-errors (default-font-height))))))

(defun agent-shell-markdown--math-delimiter-flush-p (start end)
  "Return non-nil if the delimiter spanning START..END is flush on its line.
Flush means it begins the line (only whitespace before it) or ends
the line (only whitespace after it) — the shape display-math
delimiters take in practice."
  (or (save-excursion (goto-char start) (skip-chars-backward " \t") (bolp))
      (save-excursion (goto-char end) (skip-chars-forward " \t") (eolp))))

(defun agent-shell-markdown--math-blocks (&optional avoid-ranges)
  "Return display-math blocks in the current buffer.

Each element is a plist (:start S :end E :open O :close C): S..E
spans the whole delimited block (delimiters included), and O / C
are the opening / closing delimiter token lengths, so the LaTeX
body is the buffer text in [S+O, E-C).

Only the delimiter styles listed in
`agent-shell-markdown-math-delimiters' are recognized (`$$...$$'
and/or `\\[...\\]'), and only as BLOCK-LEVEL equations:

  - the opener must start its line (after optional indentation), and
  - the closer must be flush — start or end its line.

Genuinely inline display math is thus not matched; this keeps
prose / currency (`$$') from false-positiving.

Scanning resolves each opener immediately: from just after an
opener, look for the first of its matching flush closer or a blank
line (a paragraph break, which LaTeX display math can't contain).
A closer that is not flush is treated as body and the search
continues.

  - Flush closer first: a valid block, recorded; scanning resumes
    after the closer.
  - Blank line first (or no closer): the opener is a false
    positive, so scanning resumes just after the OPENER, so a real
    block on a later line is still found.
  - Neither yet (end of buffer): a still-streaming block extending
    to `point-max' with :close 0, so a genuine equation stays
    protected as the buffer grows — mirrors
    `agent-shell-markdown--source-block-ranges'.

A delimiter inside any of AVOID-RANGES (a sorted vector, typically
fenced code) is ignored — both openers and closers — so blocks
never overlap AVOID-RANGES.  Because openers are resolved one at a
time and bodies never cross a blank line, returned blocks never
overlap each other.

For example, with `bracket' enabled and buffer \"\\=\\[E=mc^2\\]\",
returns ((:start 1 :end 11 :open 2 :close 2))."
  (let* ((specs (seq-keep (lambda (style)
                            (map-elt agent-shell-markdown--math-delimiters style))
                          agent-shell-markdown-math-delimiters))
         (blocks '())
         (case-fold-search nil))
    (when specs
      (save-excursion
        (goto-char (point-min))
        ;; Opener anchored at line start (after optional indentation);
        ;; group 1 is the delimiter token itself.
        (let ((open-re (concat "^[ \t]*\\(" (regexp-opt (mapcar #'car specs))
                               "\\)")))
          (while (re-search-forward open-re nil t)
            (let* ((open-token (match-string-no-properties 1))
                   (open-start (match-beginning 1))
                   (open-end (match-end 1))
                   (avoid (agent-shell-markdown--in-avoid-range-p
                           open-start open-end avoid-ranges)))
              (if avoid
                  (goto-char (cdr avoid))
                (let* ((close-token (cdr (seq-find
                                          (lambda (spec)
                                            (string= open-token (car spec)))
                                          specs)))
                       ;; First flush closer or blank line after the body.
                       ;; A closer in an avoid-range (code) or not flush
                       ;; on its line is body — skip it and keep looking.
                       (hit (save-excursion
                              (goto-char open-end)
                              (let ((re (concat (regexp-quote close-token)
                                                "\\|\n[ \t]*\n"))
                                    (result nil))
                                (while (and (not result)
                                            (re-search-forward re nil t))
                                  (let ((mb (match-beginning 0))
                                        (me (match-end 0))
                                        (tok (match-string-no-properties 0)))
                                    (cond
                                     ((agent-shell-markdown--in-avoid-range-p
                                       mb me avoid-ranges)
                                      (goto-char
                                       (cdr (agent-shell-markdown--in-avoid-range-p
                                             mb me avoid-ranges))))
                                     ;; Blank line: paragraph-break terminator.
                                     ((string-match-p "\n" tok)
                                      (setq result (cons tok me)))
                                     ;; Flush closer: a real block end.
                                     ((agent-shell-markdown--math-delimiter-flush-p
                                       mb me)
                                      (setq result (cons tok me))))))
                                result))))
                  (cond
                   ;; Flush closer reached with no blank line before it.
                   ((and hit (string= (car hit) close-token))
                    (push (list :start open-start :end (cdr hit)
                                :open (length open-token)
                                :close (length close-token))
                          blocks)
                    (goto-char (cdr hit)))
                   ;; Blank line first: false-positive opener.  Resume
                   ;; right after the opener so a later real block is seen.
                   (hit (goto-char open-end))
                   ;; Neither yet: still-streaming open block.
                   (t (push (list :start open-start :end (point-max)
                                  :open (length open-token) :close 0)
                            blocks)
                      (goto-char (point-max)))))))))))
    (nreverse blocks)))

(defun agent-shell-markdown--math-block-ranges (&optional avoid-ranges)
  "Return list of (start . end) ranges covering display-math blocks.

Thin adapter over `agent-shell-markdown--math-blocks' for callers
that only need the protected spans (avoid-ranges, watermark
back-off).  AVOID-RANGES is forwarded.

For example, with `bracket' enabled and buffer \"\\=\\[E=mc^2\\]\",
returns ((1 . 11))."
  (mapcar (lambda (block)
            (cons (plist-get block :start) (plist-get block :end)))
          (agent-shell-markdown--math-blocks avoid-ranges)))

(defun agent-shell-markdown--math-inline-spans (&optional avoid-ranges)
  "Return inline-math spans `\\(...\\)' in the current buffer.

Each element is a plist (:start S :end E :open O :close C), with
the same shape as `agent-shell-markdown--math-blocks': S..E spans
the whole delimited span (delimiters included) and the LaTeX body
is the buffer text in [S+O, E-C).

Inline math is matched anywhere on a line, but only when the
closing `\\)' appears on the SAME line as the opening `\\(' — a
single-line bound that keeps a stray opener from swallowing the
buffer and means the renderer's start-of-last-line watermark
already covers the still-streaming case (no open-span bookkeeping
needed, unlike `agent-shell-markdown--math-blocks').

A delimiter inside any of AVOID-RANGES (a sorted vector, typically
fenced code, display math, or inline code) is ignored, and a
candidate whose body would overlap an avoid-range is rejected, so
returned spans never overlap AVOID-RANGES or each other.

For example, with buffer \"see \\=\\(E=mc^2\\=\\) here\", returns
\((:start 5 :end 15 :open 2 :close 2))."
  (let ((spans '())
        (open agent-shell-markdown--math-inline-open)
        (close agent-shell-markdown--math-inline-close)
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (let ((open-re (regexp-quote open))
            (close-re (regexp-quote close)))
        (while (re-search-forward open-re nil t)
          (let* ((open-start (match-beginning 0))
                 (open-end (match-end 0))
                 (avoid (agent-shell-markdown--in-avoid-range-p
                         open-start open-end avoid-ranges)))
            (if avoid
                (goto-char (cdr avoid))
              ;; Look for the closer on this line only; skip a closer
              ;; that sits inside an avoid-range (it is protected text).
              (let ((eol (line-end-position))
                    (close-end nil))
                (save-excursion
                  (goto-char open-end)
                  (while (and (not close-end)
                              (re-search-forward close-re eol t))
                    (let ((in (agent-shell-markdown--in-avoid-range-p
                               (match-beginning 0) (match-end 0) avoid-ranges)))
                      (if in
                          (goto-char (cdr in))
                        (setq close-end (match-end 0))))))
                (cond
                 ;; Clean closer on the line and nothing protected sits
                 ;; between the delimiters: a valid inline span.
                 ((and close-end
                       (not (seq-some
                             (lambda (range)
                               (and (< (car range) close-end)
                                    (> (cdr range) open-start)))
                             avoid-ranges)))
                  (push (list :start open-start :end close-end
                              :open (length open) :close (length close))
                        spans)
                  (goto-char close-end))
                 ;; No usable closer on the line (false positive, or the
                 ;; span is still streaming on the buffer's last line):
                 ;; resume just after the opener so a later real span is
                 ;; still found.  The start-of-last-line watermark re-scans
                 ;; an unclosed tail on the next chunk.
                 (t (goto-char open-end)))))))))
    (nreverse spans)))

(defun agent-shell-markdown--math-inline-ranges (&optional avoid-ranges)
  "Return list of (start . end) ranges covering inline-math spans.
Thin adapter over `agent-shell-markdown--math-inline-spans' for
callers that only need the protected spans.  AVOID-RANGES is
forwarded."
  (mapcar (lambda (span)
            (cons (plist-get span :start) (plist-get span :end)))
          (agent-shell-markdown--math-inline-spans avoid-ranges)))

(cl-defun agent-shell-markdown--style-inline-math (&key avoid-ranges)
  "Overlay inline-math spans `\\(...\\)' with a text-style equation image.

Mirrors `agent-shell-markdown--style-math-blocks' but for inline
`\\(...\\)' spans (see `agent-shell-markdown--math-inline-spans'):
the raw delimited text is kept and the region handed to
`agent-shell-markdown--apply-math-region' with INLINE non-nil, so
it is typeset in text style.  Spans inside AVOID-RANGES, or with an
empty body, are left untouched.

A span that lands on already-`agent-shell-markdown-frozen' text is
also skipped.  AVOID-RANGES alone can't catch this: an earlier
pass (notably inline code) may have rewritten its region in the
same call, collapsing the range markers we were handed — the live
`frozen' property is the reliable signal, so a backticked
`\\(x\\)' stays literal code."
  (dolist (span (agent-shell-markdown--math-inline-spans avoid-ranges))
    (when-let* ((start (plist-get span :start))
                ((not (get-text-property start 'agent-shell-markdown-frozen)))
                (end (plist-get span :end))
                (latex (string-trim
                        (buffer-substring-no-properties
                         (+ start (plist-get span :open))
                         (- end (plist-get span :close)))))
                ((not (string-empty-p latex))))
      (agent-shell-markdown--apply-math-region
       (current-buffer) start end latex t))))

(cl-defun agent-shell-markdown--style-math-blocks (&key avoid-ranges)
  "Overlay display-math blocks with an equation image.

Recognizes the delimiter styles in
`agent-shell-markdown-math-delimiters' (`$$...$$' and/or
`\\[...\\]').  For each complete block with a non-empty body, the
raw delimited text is left in the buffer (so copy / save
round-trips the LaTeX source) and the region is faced with
`agent-shell-markdown-math' and tagged
`agent-shell-markdown-frozen' so later passes and subsequent
streaming calls leave it alone.  The equation image is then
applied by `agent-shell-markdown--math-render' (immediately when
cached, otherwise once an async compile finishes).  Blocks inside
any of AVOID-RANGES (typically fenced code) are left untouched, as
is an empty block.

Adds only text properties (no insert / delete), so the block
positions returned by `agent-shell-markdown--math-blocks' stay
valid while iterating.

For example, with the buffer:

  \\[E=mc^2\\]

the `\\[E=mc^2\\]' text is kept but shows an equation image in its
place, faced `agent-shell-markdown-math' and frozen."
  (dolist (block (agent-shell-markdown--math-blocks avoid-ranges))
    ;; A still-open block (no closing delimiter yet) reports :close 0
    ;; and runs to `point-max'; leave it raw until the closer streams in.
    (when-let* ((close (plist-get block :close))
                ((> close 0))
                (start (plist-get block :start))
                (end (plist-get block :end))
                (latex (string-trim
                        (buffer-substring-no-properties
                         (+ start (plist-get block :open))
                         (- end close))))
                ((not (string-empty-p latex))))
      (agent-shell-markdown--apply-math-region (current-buffer) start end latex))))

(defun agent-shell-markdown--math-fence-language-p (lang)
  "Return non-nil if fenced-block language LANG renders as display math.
Compares LANG case-insensitively against
`agent-shell-markdown-math-fence-languages'.  LANG may be nil or
empty (a fence with no info string), which is not a math language."
  (and lang
       (not (string-empty-p lang))
       (member (downcase lang) agent-shell-markdown-math-fence-languages)
       t))

(defun agent-shell-markdown--apply-math-region (buffer start end latex &optional inline)
  "Mark BUFFER's START..END as math with source LATEX and render it.

Keeps the underlying text in place, faces the region
`agent-shell-markdown-math', tags it `agent-shell-markdown-frozen'
\(so later passes / streaming calls skip it) with LATEX stashed in
`agent-shell-markdown-math-source', then hands off to
`agent-shell-markdown--math-render' for the equation image.

INLINE non-nil typesets LATEX in text style (for `\\(...\\)'
inline math) rather than as a display equation; it is stashed in
`agent-shell-markdown-math-inline' so a later refresh re-renders in
the same style.

Shared by the delimiter pass (`--style-math-blocks'), the inline
pass (`--style-inline-math'), and the fenced-block path
\(`agent-shell-markdown--style-source-blocks', for ```math /
```latex)."
  (with-current-buffer buffer
    (setq agent-shell-markdown-math--present t)
    (add-face-text-property start end 'agent-shell-markdown-math)
    (add-text-properties
     start end
     `(help-echo ,latex
       agent-shell-markdown-math-source ,latex
       agent-shell-markdown-math-inline ,inline
       agent-shell-markdown-frozen t
       rear-nonsticky (agent-shell-markdown-frozen)))
    (agent-shell-markdown--math-render buffer start end latex inline)))

(defun agent-shell-markdown--svg-color (face attribute fallback)
  "Return FACE's ATTRIBUTE color as a `#rrggbb' string, or FALLBACK.

ATTRIBUTE is `:foreground' or `:background'.  FALLBACK is returned
when the attribute is unspecified or can't be resolved to RGB
\(e.g. on a terminal that reports symbolic colors).

For example:

  (agent-shell-markdown--svg-color \\='default :foreground \"#000000\")
  => \"#ffffff\"  ; on a dark theme"
  (let ((color (face-attribute face attribute nil 'default)))
    ;; `color-name-to-rgb' both returns nil for unknown names and
    ;; signals (e.g. on the "unspecified-fg" sentinel, or off a window
    ;; system) — guard both so we always fall back cleanly.
    (if-let* (((stringp color))
              (rgb (ignore-errors (color-name-to-rgb color))))
        (apply #'color-rgb-to-hex (append rgb '(2)))
      fallback)))

(defun agent-shell-markdown--math-renderable-p ()
  "Return non-nil when equation images should be produced.

Requires SVG image support in this Emacs build, plus either a
graphical selected frame or
`agent-shell-markdown-math-render-on-non-graphic' (the daemon /
mixed TTY+GUI case — the image is ignored on a TTY frame but shows
once a graphical frame views the buffer)."
  (and (image-type-available-p 'svg)
       (or (display-graphic-p)
           agent-shell-markdown-math-render-on-non-graphic)))

(defun agent-shell-markdown--latex-placeholder-image (latex)
  "Return a placeholder SVG image boxing the raw LATEX, or nil.

This does NOT typeset LATEX — it draws the source inside a
bordered panel.  Used when `agent-shell-markdown-math-use-placeholder'
is set or the LaTeX toolchain is unavailable, so math still has a
visible (if un-typeset) rendering.  Returns nil when equations
aren't renderable (see `agent-shell-markdown--math-renderable-p'),
so callers fall back to the raw text.

LATEX is the equation source with the surrounding delimiters
already stripped, e.g. \"E=mc^2\"."
  (when (agent-shell-markdown--math-renderable-p)
    (let* ((lines (split-string latex "\n"))
           ;; `frame-char-width' / `-height' give per-char pixel
           ;; dimensions on a graphical frame and stay robust off it
           ;; (unlike `default-font-width', which calls `font-info' and
           ;; errors with no live font).  Good enough for placeholder
           ;; sizing; real typesetting will set its own dimensions.
           (char-w (frame-char-width))
           (char-h (frame-char-height))
           (pad char-h)
           (badge-h char-h)
           (text-w (* char-w (apply #'max 1 (mapcar #'length lines))))
           (width (+ text-w (* 2 pad)))
           (height (+ badge-h (* char-h (length lines)) (* 2 pad)))
           (fg (agent-shell-markdown--svg-color 'default :foreground "#000000"))
           (border (agent-shell-markdown--svg-color
                    'font-lock-comment-face :foreground "#888888"))
           (panel (agent-shell-markdown--svg-color
                   'org-block :background "#f4f4f4"))
           (svg (svg-create width height)))
      (svg-rectangle svg 0 0 width height
                     :rx (/ char-h 2)
                     :fill panel
                     :stroke border
                     :stroke-width 1)
      (svg-text svg "tex"
                :x pad
                :y (* badge-h 0.85)
                :font-size (* badge-h 0.7)
                :font-style "italic"
                :fill border)
      (seq-do-indexed
       (lambda (line i)
         (svg-text svg (if (string-empty-p line) " " line)
                   :x pad
                   :y (+ badge-h pad (* char-h (1+ i)) (- (/ char-h 4)))
                   :font-family "monospace"
                   :font-size char-h
                   :fill fg))
       lines)
      (svg-image svg :scale 1.0 :ascent 'center))))

(defun agent-shell-markdown--math-tools-available-p ()
  "Return non-nil when the LaTeX-to-SVG toolchain is on the variable `exec-path'."
  (and (executable-find agent-shell-markdown-math-latex-program)
       (executable-find agent-shell-markdown-math-dvisvgm-program)))

(defun agent-shell-markdown--math-cache-dir ()
  "Return the equation cache directory, creating it if needed.
Honours `agent-shell-markdown-math-cache-directory', else
agent-shell's shared cache directory (`agent-shell--cache-dir'), so
the cache persists across sessions next to other cached assets."
  (let ((dir (or agent-shell-markdown-math-cache-directory
                 (agent-shell--cache-dir "markdown-math"))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun agent-shell-markdown--math-cache-key (latex &optional inline)
  "Return a stable cache key for LATEX.
The preamble is folded in so changing it invalidates the cache.
INLINE (text style vs display) is folded in too, since the same
LATEX renders differently in each.  The key names the on-disk SVG,
which is both font- AND color-independent (equations are compiled
with dvisvgm `--currentcolor', then sized and tinted at display
time — see `agent-shell-markdown--math-image-cache-key'), so
neither size nor color is part of this key."
  (secure-hash 'sha1 (format "%s\0%s%s%s"
                             latex
                             agent-shell-markdown-math-preamble
                             agent-shell-markdown-math-appended-preamble
                             (if inline "\0inline" ""))))

(defun agent-shell-markdown--math-svg-file (key)
  "Return the cache SVG path for KEY."
  (expand-file-name (concat key ".svg")
                    (agent-shell-markdown--math-cache-dir)))

(defun agent-shell-markdown--math-svg-px-per-pt ()
  "Return how many pixels Emacs renders one SVG point as.

Measured once from a reference SVG declared at a known point size
\(so HiDPI and `image-scaling-factor' are captured exactly — the
same factor applies to equation images, so it cancels in the size
ratio) and cached in `agent-shell-markdown-math--svg-px-per-pt'.
Falls back to 96/72 (the usual 96-DPI ratio) when not on a
graphical frame or measurement fails; the fallback is not cached,
so a later graphical frame can still measure."
  (or agent-shell-markdown-math--svg-px-per-pt
      (and (display-graphic-p)
           (ignore-errors
             (let* ((svg (concat "<svg xmlns='http://www.w3.org/2000/svg' "
                                 "width='100pt' height='100pt'>"
                                 "<rect width='100pt' height='100pt'/></svg>"))
                    (size (image-size (create-image svg 'svg t) t)))
               (setq agent-shell-markdown-math--svg-px-per-pt
                     (/ (cdr size) 100.0)))))
      (/ 96.0 72.0)))

(defun agent-shell-markdown--math-display-scale ()
  "Return the `create-image' :scale that sizes equations to the buffer font.

Maps the LaTeX document's 10pt body font (the `standalone' default,
compiled at dvisvgm scale 1, so 10pt of LaTeX = 10 SVG points) onto
the buffer's font pixel height, times
`agent-shell-markdown-math-font-scale'.  An equation's displayed
font height is (10 * px-per-pt * scale) px, so
scale = target * font-scale / (10 * px-per-pt).  Returns 1.0 when
the font height can't be determined (batch / non-graphical),
leaving the image at its natural size."
  (let ((target (and (display-graphic-p)
                     (ignore-errors (default-font-height)))))
    (if target
        (/ (* target agent-shell-markdown-math-font-scale)
           (* 10.0 (agent-shell-markdown--math-svg-px-per-pt)))
      1.0)))

(defun agent-shell-markdown--math-load-svg-image (file &optional scale color)
  "Return an SVG image from FILE, tinted COLOR and sized to the buffer font.
The on-disk SVG emits its default ink as the literal token
`currentColor' (dvisvgm `--currentcolor'); when COLOR (a `#rrggbb'
string) is given it is substituted in, so the equation matches the
buffer foreground without recompiling.  Scaled by SCALE (default
`agent-shell-markdown--math-display-scale') so the body font matches
the surrounding text, and centred vertically for inline display."
  (let ((data (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))))
    (when color
      (setq data (replace-regexp-in-string "currentColor" color data t t)))
    (create-image data 'svg t
                  :scale (or scale (agent-shell-markdown--math-display-scale))
                  :ascent 'center)))

(defun agent-shell-markdown--math-image-cache-key (key scale color)
  "Return the in-memory image-cache key for content KEY at SCALE and COLOR.
KEY names the font- and color-independent on-disk SVG; the cached
image object bakes in a display `:scale' and a tint COLOR, so the
in-memory key adds both.  Images at different font sizes or colors
coexist, so a font or theme change just creates a new entry — no
cache clearing, and a sibling buffer's warm images survive."
  (format "%s@%s@%s" key scale color))

(defun agent-shell-markdown--math-cached-image (key)
  "Return the rendered image for content KEY at the current font and color.
Checks the in-memory cache (keyed by KEY, the display scale, and the
buffer foreground via `agent-shell-markdown--math-image-cache-key',
so each size / color has its own image), else loads KEY's on-disk
SVG and caches a freshly scaled, tinted image.  Returns nil when the
SVG isn't on disk yet (its compile hasn't finished).  Reads the
scale and color from the current buffer / frame, so call it within
the target buffer to honour a buffer-local text scale."
  (let* ((scale (agent-shell-markdown--math-display-scale))
         (color (car (agent-shell-markdown-math--current-colors)))
         (image-key (agent-shell-markdown--math-image-cache-key key scale color)))
    (or (gethash image-key agent-shell-markdown-math--image-cache)
        (let ((file (agent-shell-markdown--math-svg-file key)))
          (when (file-exists-p file)
            (puthash image-key
                     (agent-shell-markdown--math-load-svg-image file scale color)
                     agent-shell-markdown-math--image-cache))))))

(defun agent-shell-markdown--math-overlay-image (buffer start end image)
  "Lay IMAGE over BUFFER's START..END as a `display' property.

START / END may be markers (async case) or integers (sync case).
No-ops when BUFFER is dead or the region is no longer valid (it
was edited or killed away).  Runs with `with-silent-modifications'
so an async overlay doesn't flag the buffer modified, and carries
the region's existing `line-prefix' / `wrap-prefix' so indentation
is preserved."
  (when (and image (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((s (if (markerp start) (marker-position start) start))
            (e (if (markerp end) (marker-position end) end)))
        (when (and s e (<= (point-min) s) (< s e) (<= e (point-max)))
          (with-silent-modifications
            (let ((line-prefix (get-text-property s 'line-prefix))
                  (wrap-prefix (get-text-property s 'wrap-prefix)))
              (put-text-property s e 'display image)
              (put-text-property s e 'mouse-face 'highlight)
              (when line-prefix
                (put-text-property s e 'line-prefix line-prefix))
              (when wrap-prefix
                (put-text-property s e 'wrap-prefix wrap-prefix)))))))))

(defun agent-shell-markdown--math-render (buffer start end latex &optional inline)
  "Render LATEX over BUFFER's START..END as an equation image.

Does nothing when equations aren't renderable (see
`agent-shell-markdown--math-renderable-p') — the raw faced text
stands in.  With `agent-shell-markdown-math-use-placeholder' set
or no LaTeX toolchain, overlays the placeholder panel.  Otherwise
overlays the cached SVG immediately when available, else schedules
an async compile (see `agent-shell-markdown--math-compile') that
overlays the result once ready — START / END are captured as
markers so the overlay lands even after more output streams in.

INLINE non-nil typesets LATEX in text style instead of display
style; it feeds both the cache key and the compile, so inline and
display renders of the same source don't collide in the cache.
Color is not baked in here — `agent-shell-markdown--math-cached-image'
tints the color-independent SVG to the buffer foreground at display
time."
  (when (agent-shell-markdown--math-renderable-p)
    ;; Record the appearance (colors + font height) this render is for,
    ;; so a later theme / frame / font change can detect the difference
    ;; and re-render (a color change re-tints; it no longer recompiles).
    (let ((appearance (agent-shell-markdown-math--current-appearance)))
      (setq agent-shell-markdown-math--rendered-appearance appearance)
      (cond
       ((or agent-shell-markdown-math-use-placeholder
            (not (agent-shell-markdown--math-tools-available-p)))
        (agent-shell-markdown--math-overlay-image
         buffer start end
         (agent-shell-markdown--latex-placeholder-image latex)))
       (t
        (let* ((key (agent-shell-markdown--math-cache-key latex inline))
               (image (agent-shell-markdown--math-cached-image key)))
          (if image
              (agent-shell-markdown--math-overlay-image buffer start end image)
            (agent-shell-markdown--math-schedule
             key latex buffer
             (copy-marker start) (copy-marker end) inline))))))))

(defun agent-shell-markdown--math-schedule (key latex
                                                buffer start end &optional inline)
  "Queue BUFFER's START..END for KEY and start a compile if none is running.

KEY identifies the equation; LATEX and INLINE are forwarded to
`agent-shell-markdown--math-compile' for the render.  Multiple
regions sharing KEY (the same equation rendered more than once) are
coalesced onto a single in-flight compile; all are overlaid when it
finishes."
  (let ((pending (gethash key agent-shell-markdown-math--pending)))
    (puthash key (cons (list buffer start end) pending)
             agent-shell-markdown-math--pending)
    (unless pending
      (agent-shell-markdown--math-compile key latex inline))))

(defun agent-shell-markdown--math-compile-failed (key latex dir)
  "Handle a failed LaTeX compile for KEY with source LATEX.
DIR is the scratch directory containing the build log.  The log
is copied to a persistent file in the math cache directory, and a
warning is emitted with a clickable link to it."
  (let* ((log-src (expand-file-name "equation.log" dir))
         (log-dst (expand-file-name (concat key ".log")
                                    (agent-shell-markdown--math-cache-dir)))
         (snippet (truncate-string-to-width latex 60 nil nil t)))
    (when (file-exists-p log-src)
      (copy-file log-src log-dst t))
    (display-warning
     'agent-shell-markdown-math
     (format "LaTeX compile failed for: %s\nSee log: %s"
             snippet
             (if (file-exists-p log-dst) log-dst "(no log available)"))
     :warning)
    (when (file-exists-p log-dst)
      (with-current-buffer "*Warnings*"
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (save-excursion
            (when (search-backward log-dst nil t)
              (make-text-button (point) (+ (point) (length log-dst))
                                'action (lambda (_) (find-file log-dst))
                                'help-echo "Open LaTeX log"))))))))

(defun agent-shell-markdown--math-compile (key latex &optional inline)
  "Asynchronously compile LATEX to the color-independent cache SVG for KEY.

Writes a standalone LaTeX document, runs
`agent-shell-markdown-math-latex-program' then
`agent-shell-markdown-math-dvisvgm-program' in a scratch
directory, and on success caches the SVG and overlays it onto
every region queued for KEY (see
`agent-shell-markdown--math-schedule').  On failure the log is
saved and a warning emitted (see
`agent-shell-markdown--math-compile-failed'); the queued regions
keep their raw faced text.  The scratch directory is removed when
the process exits.

No color is baked in: the equation's default ink is emitted as the
literal `currentColor' (dvisvgm `--currentcolor'), so the SVG is
color-independent and is tinted to the buffer foreground at display
time (`agent-shell-markdown--math-load-svg-image').  A theme change
therefore re-tints from cache without recompiling.

Future optimization: a precompiled-preamble `.fmt' (mylatexformat)
would cut per-equation latency, but plain compilation keeps this
portable; it can slot in here without changing callers."
  (let* ((dir (make-temp-file "agent-shell-markdown-math" t))
         (tex (expand-file-name "equation.tex" dir))
         (dvi (expand-file-name "equation.dvi" dir))
         (svg (agent-shell-markdown--math-svg-file key))
         (cleanup (lambda () (ignore-errors (delete-directory dir t)))))
    (with-temp-file tex
      (insert agent-shell-markdown-math-preamble "\n"
              (if (string-empty-p agent-shell-markdown-math-appended-preamble)
                  ""
                (concat agent-shell-markdown-math-appended-preamble "\n"))
              "\\begin{document}\n"
              ;; Display math is typeset `\displaystyle' (full-size sums /
              ;; fractions / integrals); inline `\(...\)' is left in text
              ;; style so it sits compactly within the surrounding line.
              ;; No `\color' — `--currentcolor' below turns the default
              ;; (black) ink into the `currentColor' token, tinted at display.
              (format "$%s%s$\n"
                      (if inline "" "\\displaystyle ")
                      latex)
              "\\end{document}\n"))
    ;; Compile at dvisvgm scale 1: the SVG is vector (glyphs are outline
    ;; paths via --no-fonts), so the scale doesn't affect quality, and the
    ;; displayed size is set later by `--math-display-scale'.  Fixing it at 1
    ;; means the SVG carries the equation's natural point dimensions.
    ;; `--currentcolor' rewrites the default ink to the `currentColor' token
    ;; so the file is color-independent (tinted at display time).
    (let ((command
           (format "cd %s && %s -interaction=nonstopmode -halt-on-error %s && %s --no-fonts --exact-bbox --currentcolor --scale=1 -o %s %s"
                   (shell-quote-argument dir)
                   (shell-quote-argument agent-shell-markdown-math-latex-program)
                   (shell-quote-argument tex)
                   (shell-quote-argument agent-shell-markdown-math-dvisvgm-program)
                   (shell-quote-argument svg)
                   (shell-quote-argument dvi))))
      (condition-case err
          (set-process-sentinel
           (start-process-shell-command "agent-shell-markdown-math" nil command)
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (if (and (eq (process-status process) 'exit)
                        (zerop (process-exit-status process))
                        (file-exists-p svg))
                   (dolist (region (gethash key agent-shell-markdown-math--pending))
                     (let ((buffer (nth 0 region))
                           (start (nth 1 region))
                           (end (nth 2 region)))
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (agent-shell-markdown--math-overlay-image
                            buffer start end
                            (agent-shell-markdown--math-cached-image key))))))
                 (agent-shell-markdown--math-compile-failed key latex dir))
               (remhash key agent-shell-markdown-math--pending)
               (funcall cleanup))))
        (error
         ;; Couldn't even spawn the process — drop the queue and clean up.
         (remhash key agent-shell-markdown-math--pending)
         (funcall cleanup)
         (signal (car err) (cdr err)))))))

(defun agent-shell-markdown-math--refresh-buffer (buffer)
  "Re-render every display-math region in BUFFER for the current colors.
Each `agent-shell-markdown-math-source' region is handed back to
`agent-shell-markdown--math-render', which recomputes the cache key
\(so a foreground change yields a fresh image and an unchanged one
is reused from cache)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (let ((pos (point-min)))
          (while (setq pos (text-property-not-all
                            pos (point-max)
                            'agent-shell-markdown-math-source nil))
            (let ((latex (get-text-property
                          pos 'agent-shell-markdown-math-source))
                  (inline (get-text-property
                           pos 'agent-shell-markdown-math-inline))
                  (end (or (next-single-property-change
                            pos 'agent-shell-markdown-math-source nil (point-max))
                           (point-max))))
              (agent-shell-markdown--math-render buffer pos end latex inline)
              (setq pos end))))))))

(defun agent-shell-markdown-math-refresh (&optional buffer)
  "Re-render displayed equations for the current colors and font.
With BUFFER, re-render only that buffer; otherwise (the interactive
default) every buffer that has rendered equations.  Call after a
theme, appearance, or font-size change so equation images pick up
the new colors and size.

The pixels-per-point calibration is dropped so it is re-measured
\(e.g. after a display / scaling change); images are then rebuilt at
the current font scale from the on-disk SVGs — cheap, no LaTeX
recompile unless the color also changed.  The in-memory image cache
is keyed per display scale (see
`agent-shell-markdown--math-image-cache-key'), so a new size just
adds entries and a sibling buffer's warm images survive — no clear
needed.  Each re-rendered buffer records its new appearance via
`agent-shell-markdown--math-render', so unchanged buffers stay fast
and untouched buffers refresh lazily when next displayed."
  (interactive)
  (setq agent-shell-markdown-math--svg-px-per-pt nil)
  (dolist (buf (if buffer
                   (list buffer)
                 (seq-filter
                  (lambda (b)
                    (buffer-local-value 'agent-shell-markdown-math--present b))
                  (buffer-list))))
    (agent-shell-markdown-math--refresh-buffer buf)))

(defun agent-shell-markdown-math--maybe-refresh (&rest _)
  "Re-render equations if the appearance changed since they were rendered.
Hooked to buffer display (`window-buffer-change-functions'), theme
enabling (`enable-theme-functions'), and buffer zoom
\(`text-scale-mode-hook').  A no-op when math rendering is off;
otherwise the actual comparison and refresh are deferred to the next
idle moment, by which point a freshly applied theme / text scale is
fully in effect (and rapid repeat triggers collapse, since the first
refresh updates the recorded appearance)."
  (when agent-shell-markdown-render-math
    (run-at-time 0 nil #'agent-shell-markdown-math--refresh-if-changed)))

(defun agent-shell-markdown-math--refresh-if-changed ()
  "Re-render the current buffer's equations if its appearance changed.
Acts only on the current buffer — the one the firing hook just made
relevant (displayed, themed, or zoomed) — so making one chat visible
never re-renders the others; each refreshes lazily when it is itself
displayed.  The appearance signature folds in both colors and the
buffer font height (see
`agent-shell-markdown-math--current-appearance'), so a font-size
change is picked up just like a color change."
  (when (and agent-shell-markdown-render-math
             agent-shell-markdown-math--present
             (not (equal (agent-shell-markdown-math--current-appearance)
                         agent-shell-markdown-math--rendered-appearance)))
    (agent-shell-markdown-math-refresh (current-buffer))))

;; Re-render lazily, when an equation buffer is next displayed, rather than
;; eagerly on every event that might recolor the default face.
;; `window-buffer-change-functions' is the workhorse: whatever changed the
;; colors (a theme, a macOS/Linux system light/dark toggle on any platform,
;; a graphical client attaching to a daemon after a TTY render), we notice
;; the next time the buffer is shown in a window and repaint if stale.
;; `enable-theme-functions' (Emacs 29+, cross-platform) covers the one case
;; display alone misses: a theme enabled while the buffer stays visible and
;; untouched.  `text-scale-mode-hook' covers the other: a buffer-local zoom
;; (`text-scale-adjust', C-x C-+/-) changes the font height without a
;; display or theme event, so without this hook equations only re-size on
;; the next buffer switch.  All three go through the same appearance-changed
;; check (colors + font height), so they're cheap no-ops when nothing
;; changed (or math rendering is off).
(add-hook 'window-buffer-change-functions
          #'agent-shell-markdown-math--maybe-refresh)
(add-hook 'enable-theme-functions #'agent-shell-markdown-math--maybe-refresh)
(add-hook 'text-scale-mode-hook #'agent-shell-markdown-math--maybe-refresh)

(provide 'agent-shell-markdown-math)

;;; agent-shell-markdown-math.el ends here
