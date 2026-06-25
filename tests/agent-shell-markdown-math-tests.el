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
               (lambda (_buffer start end latex)
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

(provide 'agent-shell-markdown-math-tests)

;;; agent-shell-markdown-math-tests.el ends here
