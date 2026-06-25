;;; agent-shell-markdown-math-tests.el --- Tests for agent-shell-markdown-math -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run via:
;;
;;   emacs -batch -l ert -l tests/agent-shell-markdown-math-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory
                                     (or load-file-name buffer-file-name))))

;; Loads `agent-shell-markdown', which in turn requires
;; `agent-shell-markdown-math'.  The math passes run through the public
;; `agent-shell-markdown-convert', so the tests exercise the renderer
;; integration rather than the module in isolation.
(load-file (expand-file-name "../agent-shell-markdown.el"
                             (file-name-directory
                              (or load-file-name buffer-file-name))))

(defmacro agent-shell-markdown-math-tests--enabled (&rest body)
  "Evaluate BODY with math rendering enabled.
`agent-shell-markdown-render-math' (the master switch) defaults to
nil, so every test that expects equations to render must opt in;
this binds it for the default delimiter set (`bracket')."
  (declare (indent 0) (debug t))
  `(let ((agent-shell-markdown-render-math t))
     ,@body))

(defmacro agent-shell-markdown-math-tests--with-dollar (&rest body)
  "Evaluate BODY with math rendering on and `$$...$$' enabled.
`agent-shell-markdown-math-delimiters' defaults to `bracket' only,
so the dollar-delimited tests opt into `dollar' (and the master
`agent-shell-markdown-render-math' switch) through this one binding."
  (declare (indent 0) (debug t))
  `(let ((agent-shell-markdown-render-math t)
         (agent-shell-markdown-math-delimiters '(dollar bracket)))
     ,@body))

(ert-deftest agent-shell-markdown-convert-display-math-protects-markup ()
  ;; A complete `$$...$$' block is faced `agent-shell-markdown-math'
  ;; as a single run; the LaTeX source is kept literal (no bold /
  ;; italic / subscript processing of its interior).  On the
  ;; non-graphical batch display no equation image is overlaid, so
  ;; `--deconstruct' sees only the math face.
  (agent-shell-markdown-math-tests--with-dollar
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "$$ **x** $$"))
                   '(("$$ **x** $$" (agent-shell-markdown-math)))))))

(ert-deftest agent-shell-markdown-convert-display-math-block ()
  ;; Multi-line block form: the whole `$$\\n...\\n$$' region is one
  ;; math-faced run.
  (agent-shell-markdown-math-tests--with-dollar
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "$$
E=mc^2
$$"))
                   '(("$$
E=mc^2
$$" (agent-shell-markdown-math)))))))

(ert-deftest agent-shell-markdown-convert-open-math-protects-rest ()
  ;; An unclosed `$$' protects the rest of the buffer as still
  ;; streaming, just like an open fence: markup after it is left raw
  ;; until the closing `$$' arrives.
  (agent-shell-markdown-math-tests--with-dollar
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "before **b**
$$
streaming **not bold**"))
                   '(("before " nil)
                     ("b" (agent-shell-markdown-bold))
                     ("
$$
streaming **not bold**" nil))))))

(ert-deftest agent-shell-markdown-convert-display-math-in-fenced-block-untouched ()
  ;; A `$$' inside a fenced code block is body text, not display
  ;; math: it must not get the math face (even with `dollar' enabled).
  (agent-shell-markdown-math-tests--with-dollar
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "```
$$ x $$
```"))
                   '(("
" (agent-shell-markdown-source-block))
                     ("snippet ⧉" (agent-shell-markdown-source-block-language))
                     ("

$$ x $$

" (agent-shell-markdown-source-block)))))))

(ert-deftest agent-shell-markdown-convert-bracket-math-protects-markup ()
  ;; A complete block-level `\\[...\\]' is recognized as display math
  ;; and faced `agent-shell-markdown-math', keeping its interior literal.
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "\\[ **x** \\]"))
                   '(("\\[ **x** \\]" (agent-shell-markdown-math)))))))

(ert-deftest agent-shell-markdown-convert-open-bracket-math-protects-rest ()
  ;; A line-start `\\[' with no closer yet protects the rest of the
  ;; buffer until the closing `\\]' arrives, mirroring open `$$' /
  ;; open fences.
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "before **b**
\\[ streaming **not bold**"))
                   '(("before " nil)
                     ("b" (agent-shell-markdown-bold))
                     ("
\\[ streaming **not bold**" nil))))))

(ert-deftest agent-shell-markdown-convert-math-delimiters-independent ()
  ;; `agent-shell-markdown-math-delimiters' toggles each style
  ;; independently: with only `bracket' enabled, a block-level
  ;; `\\[...\\]' renders while `$$...$$' is left as plain text (and its
  ;; `**' inside is processed as ordinary markup).
  (let ((agent-shell-markdown-render-math t)
        (agent-shell-markdown-math-delimiters '(bracket)))
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "$$ **b** $$
\\[ **d** \\]"))
                   '(("$$ " nil)
                     ("b" (agent-shell-markdown-bold))
                     (" $$
" nil)
                     ("\\[ **d** \\]" (agent-shell-markdown-math)))))))

(ert-deftest agent-shell-markdown-convert-math-delimiters-disabled ()
  ;; An empty `agent-shell-markdown-math-delimiters' disables the
  ;; delimiter styles even with the master switch on: delimiters are
  ;; plain text and inner markup is processed normally.
  (let ((agent-shell-markdown-render-math t)
        (agent-shell-markdown-math-delimiters '()))
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "x \\[ **y** \\] z"))
                   '(("x \\[ " nil)
                     ("y" (agent-shell-markdown-bold))
                     (" \\] z" nil))))))

(ert-deftest agent-shell-markdown-convert-math-no-cross-delimiter-nesting ()
  ;; A `$$' inside a `\\[...\\]' block is body, not a delimiter: the
  ;; block runs to the matching `\\]' and is one math-faced run.
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "\\[ a $$ b \\]"))
                   '(("\\[ a $$ b \\]" (agent-shell-markdown-math)))))))

(ert-deftest agent-shell-markdown-convert-math-multiline-body ()
  ;; LaTeX allows newlines (but not blank lines) inside display math,
  ;; so `\\[\\nE=mc^2\\n\\]' renders as one block.
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "\\[
E=mc^2
\\]"))
                   '(("\\[
E=mc^2
\\]" (agent-shell-markdown-math)))))))

(ert-deftest agent-shell-markdown-convert-math-blank-line-rejected ()
  ;; A blank line can't appear inside LaTeX display math, so a block
  ;; whose body would span one is rejected (left as plain text rather
  ;; than mis-rendered as a single equation).
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "\\[
E=mc^2

extra
\\]"))
                   '(("\\[
E=mc^2

extra
\\]" nil))))))

(ert-deftest agent-shell-markdown-convert-math-stray-opener-recovers-real-blocks ()
  ;; A stray line-start opener that never closes before a blank line is
  ;; a false positive; scanning resumes just after it, so a real
  ;; block-level equation on a later line is still rendered (rather than
  ;; the stray opener swallowing it).
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "\\[ stray opener

\\[ E=mc^2 \\]"))
                   '(("\\[ stray opener

" nil)
                     ("\\[ E=mc^2 \\]" (agent-shell-markdown-math)))))))

(ert-deftest agent-shell-markdown-convert-math-inline-opener-ignored ()
  ;; An opener not at line start is inline, not block-level display
  ;; math: it is left as plain text and inner markup is processed.
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert "see \\[ **x** \\] here"))
                   '(("see \\[ " nil)
                     ("x" (agent-shell-markdown-bold))
                     (" \\] here" nil))))))

(ert-deftest agent-shell-markdown-convert-math-non-flush-closer-rejected ()
  ;; A line-start opener whose closer sits mid-line (text follows on the
  ;; same line) is not a flush block; it is rejected and left as plain
  ;; text rather than rendered.
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert "\\[ E=mc^2 \\] and text"))
                   '(("\\[ E=mc^2 \\] and text" nil))))))

(ert-deftest agent-shell-markdown-convert-math-dollar-on-by-default ()
  ;; `dollar' is part of the default `agent-shell-markdown-math-delimiters'
  ;; now that block-level anchoring makes it safe, so `$$...$$' renders
  ;; without opting in (only the master switch need be on here).
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert "$$ E=mc^2 $$"))
                   '(("$$ E=mc^2 $$" (agent-shell-markdown-math)))))))

(ert-deftest agent-shell-markdown-convert-fenced-math-renders ()
  ;; A ```math fence renders as display math: the fences are stripped
  ;; and the body is math-faced (the trailing newline stays plain).
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert "```math
E=mc^2
```"))
                   '(("E=mc^2" (agent-shell-markdown-math))
                     ("
" nil))))))

(ert-deftest agent-shell-markdown-convert-fenced-latex-renders ()
  ;; A ```latex fence is also treated as display math.
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert "```latex
E=mc^2
```"))
                   '(("E=mc^2" (agent-shell-markdown-math))
                     ("
" nil))))))

(ert-deftest agent-shell-markdown-convert-fenced-non-math-stays-code ()
  ;; A non-math language fence is unaffected by math rendering — it
  ;; still renders as a code block (no math face), even with the
  ;; master switch on.
  (agent-shell-markdown-math-tests--enabled
    (let ((runs (agent-shell-markdown--deconstruct
                 (agent-shell-markdown-convert "```python
x = 1
```"))))
      (should-not (seq-some (lambda (run)
                              (memq 'agent-shell-markdown-math (cadr run)))
                            runs))
      (should (seq-some (lambda (run)
                          (memq 'agent-shell-markdown-source-block (cadr run)))
                        runs)))))

(ert-deftest agent-shell-markdown-convert-math-master-switch-off ()
  ;; With the master switch off (the default), `\\[...\\]' is plain
  ;; text and its inner markup is processed normally (here `**x**'
  ;; becomes bold) — math rendering is fully gated.
  (let ((agent-shell-markdown-render-math nil))
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert "before \\[ **x** \\] after"))
                   '(("before \\[ " nil)
                     ("x" (agent-shell-markdown-bold))
                     (" \\] after" nil))))))

(ert-deftest agent-shell-markdown-convert-fenced-math-as-code-when-off ()
  ;; With the master switch off, a ```math fence is left as an ordinary
  ;; code block (source-block face), not a math equation.
  (let* ((agent-shell-markdown-render-math nil)
         (runs (agent-shell-markdown--deconstruct
                (agent-shell-markdown-convert "```math
E=mc^2
```"))))
    (should-not (seq-some (lambda (run)
                            (memq 'agent-shell-markdown-math (cadr run)))
                          runs))
    (should (seq-some (lambda (run)
                        (memq 'agent-shell-markdown-source-block (cadr run)))
                      runs))))

(ert-deftest agent-shell-markdown-math-renderable-p-honors-non-graphic-opt-in ()
  ;; Renderability requires SVG build support, and then either a
  ;; graphical frame or the non-graphic opt-in (for daemon use).
  (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) t)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (let ((agent-shell-markdown-math-render-on-non-graphic nil))
        (should-not (agent-shell-markdown--math-renderable-p)))
      (let ((agent-shell-markdown-math-render-on-non-graphic t))
        (should (agent-shell-markdown--math-renderable-p))))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (let ((agent-shell-markdown-math-render-on-non-graphic nil))
        (should (agent-shell-markdown--math-renderable-p)))))
  ;; No SVG support in the build => never renderable, even with the opt-in.
  (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) nil))
            ((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
    (let ((agent-shell-markdown-math-render-on-non-graphic t))
      (should-not (agent-shell-markdown--math-renderable-p)))))

(ert-deftest agent-shell-markdown-math-refresh-buffer-revisits-each-region ()
  ;; A refresh hands every `agent-shell-markdown-math-source' region
  ;; back to --math-render (with its own latex), so a theme change
  ;; re-tints all equations.  Stub --math-render to record the visits.
  (let ((calls '()))
    (cl-letf (((symbol-function 'agent-shell-markdown--math-render)
               (lambda (_buffer start end latex &optional _inline)
                 (push (list start end latex) calls))))
      (with-temp-buffer
        (insert "first eq then second eq")
        ;; Two separate math-source regions with distinct latex.
        (put-text-property 1 6 'agent-shell-markdown-math-source "A")
        (put-text-property 15 21 'agent-shell-markdown-math-source "B")
        (agent-shell-markdown-math--refresh-buffer (current-buffer))))
    (should (equal (nreverse calls)
                   '((1 6 "A") (15 21 "B"))))))

(ert-deftest agent-shell-markdown-math-cache-key-distinguishes-inputs ()
  ;; The cache key must be stable for identical inputs and differ when the
  ;; equation, colour, or scale changes — otherwise cached SVGs collide or
  ;; never hit.  (Pure function; no TeX or graphical display needed.)
  (let ((base (agent-shell-markdown--math-cache-key "E=mc^2" "#000000" 1.4)))
    (should (equal base (agent-shell-markdown--math-cache-key "E=mc^2" "#000000" 1.4)))
    (should-not (equal base (agent-shell-markdown--math-cache-key "E=mc^3" "#000000" 1.4)))
    (should-not (equal base (agent-shell-markdown--math-cache-key "E=mc^2" "#ffffff" 1.4)))
    (should-not (equal base (agent-shell-markdown--math-cache-key "E=mc^2" "#000000" 2.0)))))

(ert-deftest agent-shell-markdown-math-cache-key-folds-in-preamble ()
  ;; Changing the preamble must invalidate the cache (different output).
  (let ((base (agent-shell-markdown--math-cache-key "E=mc^2" "#000000" 1.4)))
    (let ((agent-shell-markdown-math-preamble "\\documentclass{minimal}"))
      (should-not (equal base (agent-shell-markdown--math-cache-key
                               "E=mc^2" "#000000" 1.4))))))

(ert-deftest agent-shell-markdown-convert-inline-math-protects-markup ()
  ;; Inline `\\(...\\)' is matched anywhere on a line (not just block
  ;; level) and faced `agent-shell-markdown-math', keeping its interior
  ;; literal (here `**x**' is not turned bold).
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert "a \\( **x** \\) b"))
                   '(("a " nil)
                     ("\\( **x** \\)" (agent-shell-markdown-math))
                     (" b" nil))))))

(ert-deftest agent-shell-markdown-convert-inline-math-multiple-per-line ()
  ;; Several inline spans on one line each render independently.
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert "\\(a\\) and \\(b\\)"))
                   '(("\\(a\\)" (agent-shell-markdown-math))
                     (" and " nil)
                     ("\\(b\\)" (agent-shell-markdown-math)))))))

(ert-deftest agent-shell-markdown-convert-inline-math-in-inline-code-untouched ()
  ;; A `\\(...\\)' inside an inline-code span is literal code, not math:
  ;; it keeps the inline-code face and gets no math face.
  (agent-shell-markdown-math-tests--enabled
    (let ((runs (agent-shell-markdown--deconstruct
                 (agent-shell-markdown-convert "`\\(x\\)`"))))
      (should-not (seq-some (lambda (run)
                              (memq 'agent-shell-markdown-math (cadr run)))
                            runs))
      (should (seq-some (lambda (run)
                          (memq 'agent-shell-markdown-inline-code (cadr run)))
                        runs)))))

(ert-deftest agent-shell-markdown-convert-inline-math-in-fenced-block-untouched ()
  ;; A `\\(...\\)' inside a fenced code block is body text, not math.
  (agent-shell-markdown-math-tests--enabled
    (let ((runs (agent-shell-markdown--deconstruct
                 (agent-shell-markdown-convert "```
\\(x\\)
```"))))
      (should-not (seq-some (lambda (run)
                              (memq 'agent-shell-markdown-math (cadr run)))
                            runs)))))

(ert-deftest agent-shell-markdown-convert-inline-math-closer-must-be-same-line ()
  ;; The closer must be on the opener's line; a `\\(' whose `\\)' is on a
  ;; later line is not matched (left as plain text), which bounds the
  ;; single-line inline form.
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert "\\(
x
\\)"))
                   '(("\\(
x
\\)" nil))))))

(ert-deftest agent-shell-markdown-convert-inline-math-streaming-tail ()
  ;; An unclosed `\\(' on the buffer's last line is left raw (the closer
  ;; may still stream in); the start-of-last-line watermark re-scans it.
  (agent-shell-markdown-math-tests--enabled
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert "text \\(E=mc^2"))
                   '(("text \\(E=mc^2" nil))))))

(ert-deftest agent-shell-markdown-convert-inline-math-can-be-disabled ()
  ;; `agent-shell-markdown-math-render-inline' nil disables `\\(...\\)'
  ;; even with the master switch on: delimiters are plain and inner
  ;; markup (`**x**') is processed normally.
  (let ((agent-shell-markdown-render-math t)
        (agent-shell-markdown-math-render-inline nil))
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert "a \\( **x** \\) b"))
                   '(("a \\( " nil)
                     ("x" (agent-shell-markdown-bold))
                     (" \\) b" nil))))))

(ert-deftest agent-shell-markdown-convert-inline-math-master-switch-off ()
  ;; With the master switch off, inline `\\(...\\)' is plain text and
  ;; inner markup is processed (here `**x**' becomes bold).
  (let ((agent-shell-markdown-render-math nil))
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert "a \\( **x** \\) b"))
                   '(("a \\( " nil)
                     ("x" (agent-shell-markdown-bold))
                     (" \\) b" nil))))))

(ert-deftest agent-shell-markdown-convert-inline-math-tags-source-and-inline ()
  ;; Inline math stashes its source and the inline flag, while display
  ;; math stashes its source with the flag nil — so a refresh re-renders
  ;; each in the right style.
  (agent-shell-markdown-math-tests--enabled
    (let ((inline (agent-shell-markdown-convert "a \\(x\\) b"))
          (display (agent-shell-markdown-convert "\\[ x \\]")))
      (should (equal (get-text-property 2 'agent-shell-markdown-math-source inline)
                     "x"))
      (should (get-text-property 2 'agent-shell-markdown-math-inline inline))
      (should (equal (get-text-property 0 'agent-shell-markdown-math-source display)
                     "x"))
      (should-not (get-text-property 0 'agent-shell-markdown-math-inline display)))))

(ert-deftest agent-shell-markdown-math-cache-key-distinguishes-inline ()
  ;; Inline and display renders of the same source must not collide:
  ;; the inline flag changes the key, while the default (display) key is
  ;; unchanged from the no-flag form (so existing caches stay valid).
  (should (equal (agent-shell-markdown--math-cache-key "x" "#000000" 1.4)
                 (agent-shell-markdown--math-cache-key "x" "#000000" 1.4 nil)))
  (should-not (equal (agent-shell-markdown--math-cache-key "x" "#000000" 1.4)
                     (agent-shell-markdown--math-cache-key "x" "#000000" 1.4 t))))

(ert-deftest agent-shell-markdown-math-cache-dir-uses-agent-shell-cache ()
  ;; By default the equation cache lives under agent-shell's shared cache
  ;; directory (so SVGs persist across sessions next to other cached
  ;; assets).  `agent-shell.el' isn't loaded by this renderer-only test
  ;; harness, so stub the helper — exactly the seam the production code
  ;; relies on agent-shell.el providing in a real session.
  (let ((agent-shell-markdown-math-cache-directory nil)
        (tmp (make-temp-file "asm-cache" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--cache-dir)
                   (lambda (&rest components)
                     (apply #'file-name-concat tmp components))))
          (should (equal (agent-shell-markdown--math-cache-dir)
                         (file-name-concat tmp "markdown-math"))))
      (delete-directory tmp t))))

(ert-deftest agent-shell-markdown-math-cache-dir-honors-explicit-override ()
  ;; An explicit `agent-shell-markdown-math-cache-directory' wins over the
  ;; shared default and is created on demand.
  (let* ((parent (make-temp-file "asm-cache-override" t))
         (dir (file-name-concat parent "eqs"))
         (agent-shell-markdown-math-cache-directory dir))
    (unwind-protect
        (progn
          (should (equal (agent-shell-markdown--math-cache-dir) dir))
          (should (file-directory-p dir)))
      (delete-directory parent t))))

(ert-deftest agent-shell-markdown-math-display-scale-is-1-when-non-graphical ()
  ;; Off a graphical frame (batch, the daemon-prerender path) the font
  ;; height is unknown, so the image is left at natural size — this is
  ;; why the batch render paths and the rest of the suite are unaffected.
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (should (equal (agent-shell-markdown--math-display-scale) 1.0))))

(ert-deftest agent-shell-markdown-math-svg-px-per-pt-falls-back-uncached ()
  ;; Without a graphical frame the calibration returns the 96/72 fallback
  ;; and must NOT cache it, so a later graphical frame can still measure.
  (let ((agent-shell-markdown-math--svg-px-per-pt nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (should (equal (agent-shell-markdown--math-svg-px-per-pt) (/ 96.0 72.0)))
      (should-not agent-shell-markdown-math--svg-px-per-pt))))

(ert-deftest agent-shell-markdown-math-display-scale-matches-font ()
  ;; The display scale maps LaTeX's 10pt body font onto the buffer font
  ;; height: scale = target * font-scale / (10 * math-scale * px-per-pt).
  ;; Stub the graphical inputs so the arithmetic is checked deterministically.
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
            ((symbol-function 'default-font-height) (lambda (&rest _) 28))
            ((symbol-function 'agent-shell-markdown--math-svg-px-per-pt)
             (lambda () 2.0)))
    (let ((agent-shell-markdown-math-scale 1.4)
          (agent-shell-markdown-math-font-scale 1.0))
      (should (equal (agent-shell-markdown--math-display-scale)
                     (/ 28.0 (* 10.0 1.4 2.0)))))
    ;; Doubling font-scale doubles the displayed size.
    (let* ((agent-shell-markdown-math-scale 1.4)
           (agent-shell-markdown-math-font-scale 1.0)
           (base (agent-shell-markdown--math-display-scale))
           (agent-shell-markdown-math-font-scale 2.0))
      (should (equal (agent-shell-markdown--math-display-scale) (* 2 base))))
    ;; The compile scale cancels: changing it leaves on-screen size unchanged.
    (let ((agent-shell-markdown-math-font-scale 1.0))
      (should (equal (let ((agent-shell-markdown-math-scale 1.4))
                       (* agent-shell-markdown-math-scale
                          (agent-shell-markdown--math-display-scale)))
                     (let ((agent-shell-markdown-math-scale 3.0))
                       (* agent-shell-markdown-math-scale
                          (agent-shell-markdown--math-display-scale))))))))

(ert-deftest agent-shell-markdown-math-appearance-tracks-color-and-font ()
  ;; The appearance signature folds in both the colors and the buffer font
  ;; height, so the lazy refresh detects a font-size change as well as a
  ;; color change.  Stub the graphical inputs.
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
            ((symbol-function 'agent-shell-markdown--svg-color)
             (lambda (_face attr _fallback)
               (if (eq attr :foreground) "#111111" "#eeeeee"))))
    (cl-letf (((symbol-function 'default-font-height) (lambda (&rest _) 20)))
      (let ((a (agent-shell-markdown-math--current-appearance)))
        (should (equal a '("#111111" "#eeeeee" 20)))
        ;; Same colors, larger font => different signature => would refresh.
        (cl-letf (((symbol-function 'default-font-height) (lambda (&rest _) 28)))
          (should-not (equal a (agent-shell-markdown-math--current-appearance))))))))

(ert-deftest agent-shell-markdown-math-refresh-if-changed-detects-font ()
  ;; `--refresh-if-changed' triggers a refresh when only the font height
  ;; moved (colors unchanged), and targets just the current buffer (the
  ;; one the hook made relevant) rather than every buffer.
  (let ((agent-shell-markdown-render-math t)
        (refreshed nil))
    (with-temp-buffer
      (setq agent-shell-markdown-math--present t)
      ;; Buffer was last rendered at font height 20; same colors.
      (setq agent-shell-markdown-math--rendered-appearance
            '("#000000" "#ffffff" 20))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'agent-shell-markdown--svg-color)
                 (lambda (_face attr _fallback)
                   (if (eq attr :foreground) "#000000" "#ffffff")))
                ((symbol-function 'default-font-height) (lambda (&rest _) 30))
                ((symbol-function 'agent-shell-markdown-math-refresh)
                 (lambda (&optional buffer) (setq refreshed (or buffer t)))))
        (agent-shell-markdown-math--refresh-if-changed))
      ;; Refreshed, and scoped to this buffer.
      (should (eq refreshed (current-buffer))))))

(ert-deftest agent-shell-markdown-math-refresh-if-changed-skips-non-present ()
  ;; In a buffer with no equations (`--present' nil), a hook firing is a
  ;; no-op even if the appearance differs — nothing to re-render.
  (let ((agent-shell-markdown-render-math t)
        (refreshed nil))
    (with-temp-buffer
      (setq agent-shell-markdown-math--present nil)
      (setq agent-shell-markdown-math--rendered-appearance nil)
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'agent-shell-markdown--svg-color)
                 (lambda (_face attr _fallback)
                   (if (eq attr :foreground) "#000000" "#ffffff")))
                ((symbol-function 'default-font-height) (lambda (&rest _) 30))
                ((symbol-function 'agent-shell-markdown-math-refresh)
                 (lambda (&optional _buffer) (setq refreshed t))))
        (agent-shell-markdown-math--refresh-if-changed))
      (should-not refreshed))))

(ert-deftest agent-shell-markdown-math-image-cache-key-includes-scale ()
  ;; The in-memory image-cache key folds in the display scale, so the same
  ;; equation at two font sizes maps to two distinct entries.
  (should (equal (agent-shell-markdown--math-image-cache-key "K" 0.8)
                 (agent-shell-markdown--math-image-cache-key "K" 0.8)))
  (should-not (equal (agent-shell-markdown--math-image-cache-key "K" 0.8)
                     (agent-shell-markdown--math-image-cache-key "K" 1.5))))

(ert-deftest agent-shell-markdown-math-image-cache-coexists-per-scale ()
  ;; The same on-disk SVG cached at two display scales yields two distinct
  ;; image objects that coexist: the first stays warm after the second is
  ;; created (so a sibling buffer's images survive a font change — no clear).
  (let ((tmp (make-temp-file "asm-svg" nil ".svg")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "<svg xmlns='http://www.w3.org/2000/svg' "
                    "width='10pt' height='10pt'>"
                    "<rect width='10' height='10'/></svg>"))
          (clrhash agent-shell-markdown-math--image-cache)
          (cl-letf (((symbol-function 'agent-shell-markdown--math-svg-file)
                     (lambda (_key) tmp)))
            (let (img1 img2)
              (cl-letf (((symbol-function 'agent-shell-markdown--math-display-scale)
                         (lambda () 0.8)))
                (setq img1 (agent-shell-markdown--math-cached-image "K")))
              (cl-letf (((symbol-function 'agent-shell-markdown--math-display-scale)
                         (lambda () 1.5)))
                (setq img2 (agent-shell-markdown--math-cached-image "K")))
              (should img1)
              (should img2)
              ;; Two coexisting entries, one per scale.
              (should (= 2 (hash-table-count
                            agent-shell-markdown-math--image-cache)))
              ;; The first is still served from cache (warm, not evicted).
              (cl-letf (((symbol-function 'agent-shell-markdown--math-display-scale)
                         (lambda () 0.8)))
                (should (eq img1 (agent-shell-markdown--math-cached-image "K"))))
              ;; Each image carries its own scale.
              (should (equal (image-property img1 :scale) 0.8))
              (should (equal (image-property img2 :scale) 1.5)))))
      (delete-file tmp))))

(provide 'agent-shell-markdown-math-tests)

;;; agent-shell-markdown-math-tests.el ends here
