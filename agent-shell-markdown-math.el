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
;; Image rendering is currently a PLACEHOLDER (it boxes the raw LaTeX);
;; `agent-shell-markdown--latex-to-image' is the seam for real LaTeX
;; compilation.

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

(defvar agent-shell-markdown-math-delimiters '(bracket)
  "Display-math delimiter styles recognized when rendering markdown.

A list whose members are keys of
`agent-shell-markdown--math-delimiters':

  `bracket'  recognize `\\[...\\]'
  `dollar'   recognize `$$...$$'

The two are independent — add or drop one to toggle it.  An empty
list disables math rendering entirely (as does passing
`:render-math nil' to `agent-shell-markdown-replace-markup').

Defaults to `bracket' only: `\\[...\\]' is unambiguous, whereas
`dollar' can false-positive on prose or currency like \"it cost
$$$\".  Opt into `$$...$$' with:

  (setq agent-shell-markdown-math-delimiters \\='(bracket dollar))")

(defun agent-shell-markdown--math-blocks (&optional avoid-ranges)
  "Return display-math blocks in the current buffer.

Each element is a plist (:start S :end E :open O :close C): S..E
spans the whole delimited block (delimiters included), and O / C
are the opening / closing delimiter token lengths, so the LaTeX
body is the buffer text in [S+O, E-C).

Only the delimiter styles listed in
`agent-shell-markdown-math-delimiters' are recognized (`$$...$$'
and/or `\\[...\\]').

Scanning resolves each opener immediately: from just after an
opener we look for the first of its matching closer or a blank
line (a paragraph break, which LaTeX display math can't contain).

  - Closer first: a valid block, recorded; scanning resumes after
    the closer.
  - Blank line first: the opener is a false positive, so scanning
    resumes just after the OPENER (not past the blank line), so
    real blocks sitting between a stray opener and the blank line
    are still found.
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
        (let ((open-re (regexp-opt (mapcar #'car specs))))
          (while (re-search-forward open-re nil t)
            (let* ((open-token (match-string-no-properties 0))
                   (open-start (match-beginning 0))
                   (open-end (point))
                   (avoid (agent-shell-markdown--in-avoid-range-p
                           open-start open-end avoid-ranges)))
              (if avoid
                  (goto-char (cdr avoid))
                (let* ((close-token (cdr (seq-find
                                          (lambda (spec)
                                            (string= open-token (car spec)))
                                          specs)))
                       ;; First closer or blank line at or after the body.
                       ;; A closer inside an avoid-range isn't real — skip
                       ;; past that range and keep looking.
                       (hit (save-excursion
                              (goto-char open-end)
                              (let ((re (concat (regexp-quote close-token)
                                                "\\|\n[ \t]*\n"))
                                    (result nil))
                                (while (and (not result)
                                            (re-search-forward re nil t))
                                  (if-let* ((av (agent-shell-markdown--in-avoid-range-p
                                                 (match-beginning 0) (point)
                                                 avoid-ranges)))
                                      (goto-char (cdr av))
                                    (setq result
                                          (cons (match-string-no-properties 0)
                                                (point)))))
                                result))))
                  (cond
                   ;; Matching closer reached with no blank line before it.
                   ((and hit (string= (car hit) close-token))
                    (push (list :start open-start :end (cdr hit)
                                :open (length open-token)
                                :close (length close-token))
                          blocks)
                    (goto-char (cdr hit)))
                   ;; Blank line first: false-positive opener.  Resume
                   ;; right after the opener so inner real blocks are seen.
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
round-trips the LaTeX source) and, on a graphical display, an
image of the equation is layered over it via a `display' text
property.  The whole region is faced with
`agent-shell-markdown-math' and tagged
`agent-shell-markdown-frozen' so later passes and subsequent
streaming calls leave it alone.  Blocks inside any of AVOID-RANGES
\(typically fenced code) are left untouched, as is an empty block.

Adds only text properties (no insert / delete), so the block
positions returned by `agent-shell-markdown--math-blocks' stay
valid while iterating.

Image rendering is currently a PLACEHOLDER: it boxes the raw
LaTeX rather than typesetting it.  Real compilation is meant to
slot into `agent-shell-markdown--latex-to-image' without touching
this pass.

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
      (let ((image (agent-shell-markdown--latex-to-image latex))
            (line-prefix (get-text-property start 'line-prefix))
            (wrap-prefix (get-text-property start 'wrap-prefix)))
        (add-face-text-property start end 'agent-shell-markdown-math)
        (when image
          (put-text-property start end 'display image)
          (put-text-property start end 'mouse-face 'highlight)
          (when line-prefix
            (put-text-property start end 'line-prefix line-prefix))
          (when wrap-prefix
            (put-text-property start end 'wrap-prefix wrap-prefix)))
        (add-text-properties
         start end
         `(help-echo ,latex
           agent-shell-markdown-math-source ,latex
           agent-shell-markdown-frozen t
           rear-nonsticky (agent-shell-markdown-frozen)))))))

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

(defun agent-shell-markdown--latex-to-image (latex)
  "Return a PLACEHOLDER SVG image for LATEX, or nil.

This does NOT typeset LATEX.  It draws the raw source inside a
bordered panel so the interception / overlay pipeline can be
exercised ahead of real LaTeX compilation (which is meant to
replace this function's body).  Returns nil when no graphical
display is available, so callers fall back to the raw text.

LATEX is the equation source with the surrounding `$$'
delimiters already stripped, e.g. \"E=mc^2\"."
  (when (display-graphic-p)
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

(provide 'agent-shell-markdown-math)

;;; agent-shell-markdown-math.el ends here
