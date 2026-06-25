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

(defmacro agent-shell-markdown-math-tests--with-dollar (&rest body)
  "Evaluate BODY with `$$...$$' display math enabled.
`agent-shell-markdown-math-delimiters' defaults to `bracket' only,
so the dollar-delimited tests opt into `dollar' through this one
binding rather than repeating it in each test."
  (declare (indent 0) (debug t))
  `(let ((agent-shell-markdown-math-delimiters '(dollar bracket)))
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
                     "before $$ **x** $$ after"))
                   '(("before " nil)
                     ("$$ **x** $$" (agent-shell-markdown-math))
                     (" after" nil))))))

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
  ;; A complete `\\[...\\]' block is recognized as display math and
  ;; faced `agent-shell-markdown-math', keeping its interior literal.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "before \\[ **x** \\] after"))
                 '(("before " nil)
                   ("\\[ **x** \\]" (agent-shell-markdown-math))
                   (" after" nil)))))

(ert-deftest agent-shell-markdown-convert-open-bracket-math-protects-rest ()
  ;; An unclosed `\\[' protects the rest of the buffer until the
  ;; closing `\\]' arrives, mirroring open `$$' / open fences.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "before **b** \\[ streaming **not bold**"))
                 '(("before " nil)
                   ("b" (agent-shell-markdown-bold))
                   (" \\[ streaming **not bold**" nil)))))

(ert-deftest agent-shell-markdown-convert-math-delimiters-independent ()
  ;; `agent-shell-markdown-math-delimiters' toggles each style
  ;; independently: with only `bracket' enabled, `\\[...\\]' renders
  ;; while `$$...$$' is left as plain text (and its `**' inside is
  ;; processed as ordinary markup).
  (let ((agent-shell-markdown-math-delimiters '(bracket)))
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "a $$ **b** $$ c \\[ **d** \\] e"))
                   '(("a $$ " nil)
                     ("b" (agent-shell-markdown-bold))
                     (" $$ c " nil)
                     ("\\[ **d** \\]" (agent-shell-markdown-math))
                     (" e" nil))))))

(ert-deftest agent-shell-markdown-convert-math-delimiters-disabled ()
  ;; An empty `agent-shell-markdown-math-delimiters' disables math
  ;; rendering entirely: delimiters are plain text and inner markup
  ;; is processed normally.
  (let ((agent-shell-markdown-math-delimiters '()))
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "x \\[ **y** \\] z"))
                   '(("x \\[ " nil)
                     ("y" (agent-shell-markdown-bold))
                     (" \\] z" nil))))))

(ert-deftest agent-shell-markdown-convert-math-no-cross-delimiter-nesting ()
  ;; A `$$' inside a `\\[...\\]' block is body, not a delimiter: the
  ;; block runs to the matching `\\]' and is one math-faced run.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "\\[ a $$ b \\]"))
                 '(("\\[ a $$ b \\]" (agent-shell-markdown-math))))))

(ert-deftest agent-shell-markdown-convert-math-multiline-body ()
  ;; LaTeX allows newlines (but not blank lines) inside display math,
  ;; so `\\[\\nE=mc^2\\n\\]' renders as one block.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "\\[
E=mc^2
\\]"))
                 '(("\\[
E=mc^2
\\]" (agent-shell-markdown-math))))))

(ert-deftest agent-shell-markdown-convert-math-blank-line-rejected ()
  ;; A blank line can't appear inside LaTeX display math, so a block
  ;; whose body would span one is rejected (left as plain text rather
  ;; than mis-rendered as a single equation).
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "\\[
E=mc^2

extra
\\]"))
                 '(("\\[
E=mc^2

extra
\\]" nil)))))

(ert-deftest agent-shell-markdown-convert-math-stray-opener-recovers-real-blocks ()
  ;; A stray opener that never closes before a blank line is a false
  ;; positive, but scanning resumes just after it — so a real block
  ;; sitting between the stray opener and the blank line is still
  ;; rendered (rather than swallowed and lost).
  (agent-shell-markdown-math-tests--with-dollar
    (should (equal (agent-shell-markdown--deconstruct
                    (agent-shell-markdown-convert
                     "\\[ oops and $$E=mc^2$$ here

next"))
                   '(("\\[ oops and " nil)
                     ("$$E=mc^2$$" (agent-shell-markdown-math))
                     (" here

next" nil))))))

(provide 'agent-shell-markdown-math-tests)

;;; agent-shell-markdown-math-tests.el ends here
