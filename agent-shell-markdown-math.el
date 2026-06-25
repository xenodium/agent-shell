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
;; Two delimiter styles are recognized, toggled independently via
;; `agent-shell-markdown-math-delimiters':
;;
;;   bracket  `\[X\]'    (default; unambiguous)
;;   dollar   `$$X$$'    (opt-in; can clash with prose / currency)
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

(defvar agent-shell-markdown-math-scale 1.4
  "`dvisvgm' output scale for rendered equations.
Larger values produce bigger images.")

(defvar agent-shell-markdown-math-preamble
  "\\documentclass[border=2pt]{standalone}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{xcolor}"
  "LaTeX preamble (everything before `\\begin{document}') for equations.
The `standalone' class crops the page tightly to the equation, so
no `preview' package is required.  `xcolor' is used to tint the
equation to match the buffer foreground.  Equations are typeset as
`\\displaystyle' inline math inside the document body.")

(defvar agent-shell-markdown-math-cache-directory nil
  "Directory for cached equation SVGs and scratch compiles.
When nil, a subdirectory of the variable `temporary-file-directory'
is used.  Point this at a persistent location to keep the cache
across sessions (each unique equation then compiles at most once
ever).")

;; key (sha1 of latex + color + scale + preamble) -> image object.  An
;; equation is compiled at most once per key; subsequent renders (same
;; session, or any buffer) reuse the cached image.
(defvar agent-shell-markdown-math--image-cache (make-hash-table :test 'equal)
  "In-memory map of cache key to rendered equation image.")

;; key -> list of (BUFFER START-MARKER END-MARKER) awaiting one in-flight
;; compile.  Dedupes concurrent compiles of the same equation and records
;; every region to overlay once the SVG is ready.
(defvar agent-shell-markdown-math--pending (make-hash-table :test 'equal)
  "In-memory map of cache key to regions awaiting an in-flight compile.")

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

(defun agent-shell-markdown--apply-math-region (buffer start end latex)
  "Mark BUFFER's START..END as display math with source LATEX and render it.

Keeps the underlying text in place, faces the region
`agent-shell-markdown-math', tags it `agent-shell-markdown-frozen'
\(so later passes / streaming calls skip it) with LATEX stashed in
`agent-shell-markdown-math-source', then hands off to
`agent-shell-markdown--math-render' for the equation image.

Shared by the delimiter pass (`--style-math-blocks') and the
fenced-block path (`agent-shell-markdown--style-source-blocks',
for ```math / ```latex)."
  (with-current-buffer buffer
    (add-face-text-property start end 'agent-shell-markdown-math)
    (add-text-properties
     start end
     `(help-echo ,latex
       agent-shell-markdown-math-source ,latex
       agent-shell-markdown-frozen t
       rear-nonsticky (agent-shell-markdown-frozen)))
    (agent-shell-markdown--math-render buffer start end latex)))

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
Honours `agent-shell-markdown-math-cache-directory', else a
subdirectory of the variable `temporary-file-directory'."
  (let ((dir (or agent-shell-markdown-math-cache-directory
                 (expand-file-name "agent-shell-markdown-math"
                                   temporary-file-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun agent-shell-markdown--math-cache-key (latex color scale)
  "Return a stable cache key for LATEX rendered in COLOR at SCALE.
The preamble is folded in so changing it invalidates the cache."
  (secure-hash 'sha1 (format "%s\0%s\0%s\0%s"
                             latex color scale
                             agent-shell-markdown-math-preamble)))

(defun agent-shell-markdown--math-svg-file (key)
  "Return the cache SVG path for KEY."
  (expand-file-name (concat key ".svg")
                    (agent-shell-markdown--math-cache-dir)))

(defun agent-shell-markdown--math-load-svg-image (file)
  "Return an SVG image created from FILE, centred for inline display."
  (create-image file 'svg nil :scale 1.0 :ascent 'center))

(defun agent-shell-markdown--math-cached-image (key)
  "Return the rendered image for KEY from memory or disk, or nil.
A disk hit is promoted into the in-memory cache."
  (or (gethash key agent-shell-markdown-math--image-cache)
      (let ((file (agent-shell-markdown--math-svg-file key)))
        (when (file-exists-p file)
          (puthash key (agent-shell-markdown--math-load-svg-image file)
                   agent-shell-markdown-math--image-cache)))))

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

(defun agent-shell-markdown--math-render (buffer start end latex)
  "Render LATEX over BUFFER's START..END as an equation image.

Does nothing when equations aren't renderable (see
`agent-shell-markdown--math-renderable-p') — the raw faced text
stands in.  With `agent-shell-markdown-math-use-placeholder' set
or no LaTeX toolchain, overlays the placeholder panel.  Otherwise
overlays the cached SVG immediately when available, else schedules
an async compile (see `agent-shell-markdown--math-compile') that
overlays the result once ready — START / END are captured as
markers so the overlay lands even after more output streams in."
  (when (agent-shell-markdown--math-renderable-p)
    (cond
     ((or agent-shell-markdown-math-use-placeholder
          (not (agent-shell-markdown--math-tools-available-p)))
      (agent-shell-markdown--math-overlay-image
       buffer start end
       (agent-shell-markdown--latex-placeholder-image latex)))
     (t
      (let* ((color (agent-shell-markdown--svg-color 'default :foreground "#000000"))
             (scale agent-shell-markdown-math-scale)
             (key (agent-shell-markdown--math-cache-key latex color scale))
             (image (agent-shell-markdown--math-cached-image key)))
        (if image
            (agent-shell-markdown--math-overlay-image buffer start end image)
          (agent-shell-markdown--math-schedule
           key latex color scale buffer
           (copy-marker start) (copy-marker end))))))))

(defun agent-shell-markdown--math-schedule (key latex color scale
                                                buffer start end)
  "Queue BUFFER's START..END for KEY and start a compile if none is running.

KEY identifies the equation; LATEX, COLOR, and SCALE are forwarded
to `agent-shell-markdown--math-compile' for the render.  Multiple
regions sharing KEY (the same equation rendered more than once)
are coalesced onto a single in-flight compile; all are overlaid
when it finishes."
  (let ((pending (gethash key agent-shell-markdown-math--pending)))
    (puthash key (cons (list buffer start end) pending)
             agent-shell-markdown-math--pending)
    (unless pending
      (agent-shell-markdown--math-compile key latex color scale))))

(defun agent-shell-markdown--math-compile (key latex color scale)
  "Asynchronously compile LATEX (in COLOR, at SCALE) to the cache SVG for KEY.

Writes a standalone LaTeX document, runs
`agent-shell-markdown-math-latex-program' then
`agent-shell-markdown-math-dvisvgm-program' in a scratch
directory, and on success caches the SVG and overlays it onto
every region queued for KEY (see
`agent-shell-markdown--math-schedule').  On failure the queued
regions keep their raw faced text.  The scratch directory is
removed when the process exits.

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
              "\\begin{document}\n"
              (format "{\\color[HTML]{%s}$\\displaystyle %s$}\n"
                      (upcase (substring color 1)) latex)
              "\\end{document}\n"))
    (let ((command
           (format "cd %s && %s -interaction=nonstopmode -halt-on-error %s && %s --no-fonts --exact-bbox --scale=%s -o %s %s"
                   (shell-quote-argument dir)
                   (shell-quote-argument agent-shell-markdown-math-latex-program)
                   (shell-quote-argument tex)
                   (shell-quote-argument agent-shell-markdown-math-dvisvgm-program)
                   scale
                   (shell-quote-argument svg)
                   (shell-quote-argument dvi))))
      (condition-case err
          (set-process-sentinel
           (start-process-shell-command "agent-shell-markdown-math" nil command)
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (let ((image (when (and (eq (process-status process) 'exit)
                                       (zerop (process-exit-status process))
                                       (file-exists-p svg))
                              (agent-shell-markdown--math-load-svg-image svg))))
                 (when image
                   (puthash key image agent-shell-markdown-math--image-cache)
                   (dolist (region (gethash key agent-shell-markdown-math--pending))
                     (agent-shell-markdown--math-overlay-image
                      (nth 0 region) (nth 1 region) (nth 2 region) image))))
               (remhash key agent-shell-markdown-math--pending)
               (funcall cleanup))))
        (error
         ;; Couldn't even spawn the process — drop the queue and clean up.
         (remhash key agent-shell-markdown-math--pending)
         (funcall cleanup)
         (signal (car err) (cdr err)))))))

(provide 'agent-shell-markdown-math)

;;; agent-shell-markdown-math.el ends here
