;;; agent-shell-markdown-tests.el --- Tests for agent-shell-markdown -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run via:
;;
;;   emacs -batch -l ert -l tests/agent-shell-markdown-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'ert)

(load-file (expand-file-name "../agent-shell-markdown.el"
                             (file-name-directory
                              (or load-file-name buffer-file-name))))

(ert-deftest agent-shell-markdown-convert-bold ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "hello **world**"))
                 '(("hello " nil)
                   ("world" (agent-shell-markdown-bold))))))

(ert-deftest agent-shell-markdown-convert-bold-underscore ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "hello __world__"))
                 '(("hello " nil)
                   ("world" (agent-shell-markdown-bold))))))

(ert-deftest agent-shell-markdown-convert-italic ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "hello *world*"))
                 '(("hello " nil)
                   ("world" (agent-shell-markdown-italic))))))

(ert-deftest agent-shell-markdown-convert-italic-underscore ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "hello _world_"))
                 '(("hello " nil)
                   ("world" (agent-shell-markdown-italic))))))

(ert-deftest agent-shell-markdown-convert-multiple ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "_my_ **text**"))
                 '(("my" (agent-shell-markdown-italic))
                   (" " nil)
                   ("text" (agent-shell-markdown-bold))))))

(ert-deftest agent-shell-markdown-convert-italic-wrapping-bold ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "_**my text**_"))
                 '(("my text" (agent-shell-markdown-bold agent-shell-markdown-italic))))))

(ert-deftest agent-shell-markdown-convert-bold-wrapping-italic ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "**_my text_**"))
                 '(("my text" (agent-shell-markdown-italic agent-shell-markdown-bold))))))

(ert-deftest agent-shell-markdown-convert-bold-with-inner-italic ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "**outer _both_ outer**"))
                 '(("outer " (agent-shell-markdown-bold))
                   ("both" (agent-shell-markdown-bold agent-shell-markdown-italic))
                   (" outer" (agent-shell-markdown-bold))))))

(ert-deftest agent-shell-markdown-convert-italic-with-inner-bold ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "_outer **both** outer_"))
                 '(("outer " (agent-shell-markdown-italic))
                   ("both" (agent-shell-markdown-bold agent-shell-markdown-italic))
                   (" outer" (agent-shell-markdown-italic))))))

(ert-deftest agent-shell-markdown-convert-no-markup ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "no markup here"))
                 '(("no markup here" nil)))))

(ert-deftest agent-shell-markdown-convert-empty ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert ""))
                 '())))

(ert-deftest agent-shell-markdown-convert-inline-code-protects-markup ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "before **b** and `**not bold**` after"))
                 '(("before " nil)
                   ("b" (agent-shell-markdown-bold))
                   (" and " nil)
                   ("**not bold**" (agent-shell-markdown-inline-code))
                   (" after" nil)))))

(ert-deftest agent-shell-markdown-convert-inline-code ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "a `code` b"))
                 '(("a " nil)
                   ("code" (agent-shell-markdown-inline-code))
                   (" b" nil)))))

(ert-deftest agent-shell-markdown-convert-strikethrough ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "a ~~b~~ c"))
                 '(("a " nil)
                   ("b" (agent-shell-markdown-strikethrough))
                   (" c" nil)))))

(ert-deftest agent-shell-markdown-convert-strikethrough-wrapping-bold ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "~~**bold-strike**~~"))
                 '(("bold-strike" (agent-shell-markdown-bold agent-shell-markdown-strikethrough))))))

(ert-deftest agent-shell-markdown-convert-header-level-1 ()
  ;; Header rendering requires a trailing newline to complete; an
  ;; eob-only header is treated as still streaming and left raw.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "# Title\n"))
                 '(("Title" (agent-shell-markdown-header-1))
                   ("\n" nil)))))

(ert-deftest agent-shell-markdown-convert-header-level-3 ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "### Title\n"))
                 '(("Title" (agent-shell-markdown-header-3))
                   ("\n" nil)))))

(ert-deftest agent-shell-markdown-convert-header-with-bold ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "## **Big** title\n"))
                 '(("Big" (agent-shell-markdown-header-2 agent-shell-markdown-bold))
                   (" title" (agent-shell-markdown-header-2))
                   ("\n" nil)))))

(ert-deftest agent-shell-markdown-convert-fenced-block-protects-markup ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "before **b**
```
**not bold**
_not italic_
```
after **b2**"))
                 '(("before " nil)
                   ("b" (agent-shell-markdown-bold))
                   ("
" nil)
                   ("snippet ⧉" (agent-shell-markdown-source-block-language))
                   ("

**not bold**
_not italic_
after " nil)
                   ("b2" (agent-shell-markdown-bold))))))

(ert-deftest agent-shell-markdown-convert-open-fence-protects-rest ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "before **b**
```
streaming **not bold**"))
                 '(("before " nil)
                   ("b" (agent-shell-markdown-bold))
                   ("
```
streaming **not bold**" nil)))))

(ert-deftest agent-shell-markdown-convert-open-inline-code-protects-rest-of-line ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "before **b** and `streaming *not italic*"))
                 '(("before " nil)
                   ("b" (agent-shell-markdown-bold))
                   (" and `streaming *not italic*" nil)))))

(ert-deftest agent-shell-markdown-convert-incomplete-bold-untouched ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "complete **b** and incomplete **par"))
                 '(("complete " nil)
                   ("b" (agent-shell-markdown-bold))
                   (" and incomplete **par" nil)))))

(ert-deftest agent-shell-markdown-convert-link ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "see [docs](https://example.com) please"))
                 '(("see " nil)
                   ("docs" (agent-shell-markdown-link))
                   (" please" nil)))))

(ert-deftest agent-shell-markdown-convert-link-with-bold-inside-untouched ()
  ;; Bold inside link title is left literal (mirrors markdown-overlays:
  ;; bold regex requires whitespace/BOL before `**', and `[' isn't either).
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "[**bold**](url)"))
                 '(("**bold**" (agent-shell-markdown-link))))))

(ert-deftest agent-shell-markdown-convert-link-after-image-not-confused ()
  ;; `[X](Y)' inside `![X](Y)' must not be treated as a link.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "![alt](missing.png)"))
                 '(("![alt](missing.png)" nil)))))

(ert-deftest agent-shell-markdown-convert-image-unresolvable-untouched ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "see ![alt](/no/such/file.png) end"))
                 '(("see ![alt](/no/such/file.png) end" nil)))))

(ert-deftest agent-shell-markdown-convert-link-in-fenced-block-untouched ()
  ;; The `[b](v)' inside fences stays literal — it isn't re-processed
  ;; as a link.  Body chars carry the `agent-shell-markdown-frozen'
  ;; tag (which `--deconstruct' doesn't surface).
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "before [a](u)
```
[b](v)
```
after [c](w)"))
                 '(("before " nil)
                   ("a" (agent-shell-markdown-link))
                   ("
" nil)
                   ("snippet ⧉" (agent-shell-markdown-source-block-language))
                   ("

[b](v)
after " nil)
                   ("c" (agent-shell-markdown-link))))))

(ert-deftest agent-shell-markdown-convert-source-block-no-language ()
  ;; Plain fenced block (no language): fences deleted, a "snippet ⧉"
  ;; header is inserted directly above the body as real buffer text
  ;; (no display property), and the body chars carry the
  ;; `agent-shell-markdown-frozen' tag (not surfaced by
  ;; `--deconstruct').
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "```
body
```"))
                 '(("snippet ⧉" (agent-shell-markdown-source-block-language))
                   ("

body
" nil)))))

(ert-deftest agent-shell-markdown-convert-source-block-language-label ()
  ;; Every fence renders with an actionable label inserted as real
  ;; buffer text directly above the body — "LANG ⧉" when a language
  ;; is declared, "snippet ⧉" otherwise.  No display property, no
  ;; overlays, no bg panel.  RET or mouse-1 anywhere on the label
  ;; kills the body to the kill ring.
  (let* ((with-lang (agent-shell-markdown-convert "```python
print(\"hi\")
```
"))
         (no-lang (agent-shell-markdown-convert "```
body
```
")))
    (should (string-prefix-p "python ⧉\n\nprint("
                             (substring-no-properties with-lang)))
    (should (string-prefix-p "snippet ⧉\n\nbody"
                             (substring-no-properties no-lang)))
    ;; Label face + actionable props on both the first name char and
    ;; the ⧉ glyph.
    (dolist (i '(0 7))
      (should (eq (get-text-property i 'face with-lang)
                  'agent-shell-markdown-source-block-language))
      (should (eq (get-text-property i 'mouse-face with-lang)
                  'highlight))
      (should (keymapp (get-text-property i 'keymap with-lang))))))

(ert-deftest agent-shell-markdown-convert-source-block-nested-fences ()
  ;; A 4-backtick outer fence wraps inner 3-backtick fences as
  ;; literal body — the inner ```python ... ``` is *not* re-rendered
  ;; as a code block.  Mirrors CommonMark's variable-width fence
  ;; rule: a closer must match the opener's backtick count, and a
  ;; shorter run inside is part of the body.  Face buckets vary by
  ;; env (markdown-mode's font-lock highlights ``` markup when the
  ;; mode is loadable; in bare batch it's not), so the contract is
  ;; asserted on the rendered text, not on the face cascade.
  (let ((rendered (substring-no-properties
                   (agent-shell-markdown-convert
                    "````markdown
```python
print(\"hi\")
```
````"))))
    (should (equal rendered "markdown ⧉

```python
print(\"hi\")
```
"))))

(ert-deftest agent-shell-markdown-convert-source-block-with-language ()
  ;; `emacs-lisp' source block: fences deleted, an "emacs-lisp ⧉"
  ;; header is inserted as buffer text, then the body chars get the
  ;; language's `font-lock' faces.  In batch the keyword `if' is
  ;; faced; the rest of the body stays unfaced (no bg panel).
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "```emacs-lisp
(if t nil)
```"))
                 '(("emacs-lisp ⧉" (agent-shell-markdown-source-block-language))
                   ("

(" nil)
                   ("if" (font-lock-keyword-face))
                   (" t nil)
" nil)))))

(ert-deftest agent-shell-markdown-convert-source-block-body-tagged ()
  ;; Body chars carry `agent-shell-markdown-frozen t' so subsequent calls
  ;; treat them as an avoid-range (streaming-safe).  Body in the
  ;; rendered output is "**not bold**" followed by a newline — the
  ;; chars before that trailing newline are tagged; the newline
  ;; itself is not.
  (let ((s (agent-shell-markdown-convert "```
**not bold**
```")))
    (should (eq t (get-text-property 0 'agent-shell-markdown-frozen s)))
    (should (eq t (get-text-property 5 'agent-shell-markdown-frozen s)))
    (should (null (get-text-property (1- (length s)) 'agent-shell-markdown-frozen s)))))

(ert-deftest agent-shell-markdown-convert-inline-code-body-tagged ()
  ;; Inline code body chars are also `agent-shell-markdown-frozen t'-tagged
  ;; so a stray "**X**" inside backticks stays literal on re-runs.
  (let ((s (agent-shell-markdown-convert "a `**not bold**` b")))
    (should (eq t (get-text-property 2 'agent-shell-markdown-frozen s)))
    (should (eq t (get-text-property 13 'agent-shell-markdown-frozen s)))
    (should (null (get-text-property 0 'agent-shell-markdown-frozen s)))))

(ert-deftest agent-shell-markdown-source-block-streamed-in-chunks ()
  ;; Real-world LLM streaming: a fenced code block arrives in small
  ;; chunks that split the opening fence, the language line, body
  ;; chars, and the closing fence.  After every chunk the renderer
  ;; is called.  Once the closing fence lands, the final buffer
  ;; should show the inserted "python ⧉" label above the body, with
  ;; no raw fence markers remaining.
  (with-temp-buffer
    (dolist (chunk '("``" "`p" "yt" "hon\n"
                     "pri" "nt(" "\"hi\")\n"
                     "ra" "ise " "Sys" "temExit\n"
                     "``" "`\n"))
      (goto-char (point-max))
      (insert chunk)
      (agent-shell-markdown-replace-markup))
    (should (equal (substring-no-properties (buffer-string))
                   "python ⧉

print(\"hi\")
raise SystemExit
"))))

(ert-deftest agent-shell-markdown-source-block-body-protected-across-calls ()
  ;; Streaming: render a block, then append more markdown and re-render.
  ;; The previously-rendered body (`agent-shell-markdown-frozen t') must stay
  ;; literal — its `**not bold**' must not turn into bold X on the
  ;; second pass, while newly-appended `**real bold**' does.
  (with-temp-buffer
    (insert "```
**not bold**
```")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-max))
    (insert "
**real bold**")
    (agent-shell-markdown-replace-markup)
    (should (equal (agent-shell-markdown--deconstruct (buffer-string))
                   '(("snippet ⧉" (agent-shell-markdown-source-block-language))
                     ("

**not bold**

" nil)
                     ("real bold" (agent-shell-markdown-bold)))))))

(ert-deftest agent-shell-markdown-inline-code-body-protected-across-calls ()
  ;; Streaming counterpart for inline code: after the backticks
  ;; are gone, body chars must not be re-bolded on a second pass.
  (with-temp-buffer
    (insert "a `**not bold**` b")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-max))
    (insert " **real bold**")
    (agent-shell-markdown-replace-markup)
    (should (equal (agent-shell-markdown--deconstruct (buffer-string))
                   '(("a " nil)
                     ("**not bold**" (agent-shell-markdown-inline-code))
                     (" b " nil)
                     ("real bold" (agent-shell-markdown-bold)))))))

(ert-deftest agent-shell-markdown-convert-divider-dashes ()
  ;; A `---' line gets a `display' property and `agent-shell-markdown-frozen'
  ;; tag.  The chars themselves stay in the buffer beneath the display.
  (let ((s (agent-shell-markdown-convert "above
---
below")))
    (should (eq t (get-text-property 6 'agent-shell-markdown-frozen s)))
    (should (get-text-property 6 'display s))))

(ert-deftest agent-shell-markdown-convert-divider-stars ()
  (let ((s (agent-shell-markdown-convert "above
***
below")))
    (should (eq t (get-text-property 6 'agent-shell-markdown-frozen s)))
    (should (get-text-property 6 'display s))))

(ert-deftest agent-shell-markdown-convert-divider-underscores ()
  (let ((s (agent-shell-markdown-convert "above
___
below")))
    (should (eq t (get-text-property 6 'agent-shell-markdown-frozen s)))
    (should (get-text-property 6 'display s))))

(ert-deftest agent-shell-markdown-convert-divider-not-matched-with-text ()
  ;; `*** hello ***' is not a divider — has other content on the line.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "*** hello ***"))
                 '(("*** hello ***" nil)))))

(ert-deftest agent-shell-markdown-convert-image-file-path-unresolvable-untouched ()
  ;; Path doesn't exist (and batch mode has no graphics anyway), so
  ;; the line is left untouched.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "before
/no/such/img.png
after"))
                 '(("before
/no/such/img.png
after" nil)))))

(ert-deftest agent-shell-markdown-convert-table-basic ()
  ;; A complete table is replaced by its prettified rendering and the
  ;; inserted chars carry `agent-shell-markdown-frozen' so subsequent calls
  ;; skip them.  (Rendering shape is covered more thoroughly by the
  ;; `-output-*' tests.)
  (let ((s (agent-shell-markdown-convert "| A | B |
|---|---|
| 1 | 2 |")))
    (should (equal (substring-no-properties s)
                   "│ A │ B │
├───┼───┤
│ 1 │ 2 │"))
    (should (eq t (get-text-property 0 'agent-shell-markdown-frozen s)))))

(ert-deftest agent-shell-markdown-convert-table-without-separator-renders ()
  ;; A separator row (`|---|---|') is optional.  Two or more `|...|'
  ;; rows are enough to render — without a separator, all rows are
  ;; treated as data (no header styling, no separator border in the
  ;; output).
  (should (equal (substring-no-properties
                  (agent-shell-markdown-convert "| a | b |
| hello | world |"))
                 "│ a     │ b     │
│ hello │ world │")))

(ert-deftest agent-shell-markdown-convert-table-cell-uses-bold ()
  ;; Bold inside a cell is processed by the main pass; the rendered
  ;; table preserves the bold face on \"Alice\".
  (let* ((s (agent-shell-markdown-convert "| Name | Role |
|------|------|
| **Alice** | Engineer |"))
         (alice-pos (string-match "Alice" s)))
    (should alice-pos)
    (should (eq 'agent-shell-markdown-bold (get-text-property alice-pos 'face s)))))

(ert-deftest agent-shell-markdown-convert-table-skips-frozen-cell-pipe ()
  ;; `| `a|b` | c |' — inline-code body contains a `|', which our
  ;; inline-code styling tags `agent-shell-markdown-frozen'.  The cell parser
  ;; should treat that pipe as part of the cell rather than a
  ;; separator, yielding 2 cells (not 3).
  (let* ((s (agent-shell-markdown-convert "| `a|b` | c |
|---|---|
| x | y |"))
         (header-line (car (split-string s "
")))
         ;; In a 2-column rendering, count the leading-pipe + col-pipe
         ;; + trailing-pipe = 3 borders. (For 3 cols there would be 4.)
         (pipe-count (length (seq-filter (lambda (c) (eq c ?│))
                                         header-line))))
    (should (eq 3 pipe-count))))

(ert-deftest agent-shell-markdown-convert-table-output-plain ()
  ;; End-to-end multi-line input → multi-line output comparison.
  ;; Checks the rendered text only (no text-property assertions).
  (should (equal (substring-no-properties
                  (agent-shell-markdown-convert
                   "| A | B |
|---|---|
| 1 | 2 |"))
                 "│ A │ B │
├───┼───┤
│ 1 │ 2 │")))

(ert-deftest agent-shell-markdown-convert-table-output-with-bold ()
  ;; Bold markup inside cells is stripped by the main pipeline before
  ;; the table is rendered, so the rendered string contains \"Alice\"
  ;; (the `**...**' is gone) and columns are sized for the stripped
  ;; content.  Compares text only.
  (should (equal (substring-no-properties
                  (agent-shell-markdown-convert
                   "| Name | Role |
|------|------|
| **Alice** | Engineer |
| Bob | Manager |"))
                 "│ Name  │ Role     │
├───────┼──────────┤
│ Alice │ Engineer │
│ Bob   │ Manager  │")))

(ert-deftest agent-shell-markdown-convert-table-output-wraps-one-cell ()
  ;; When the table's natural width exceeds the target, the widest
  ;; column shrinks and its content wraps at word boundaries.
  ;; Mocks `agent-shell-markdown--display-width' to 30 so the result is
  ;; deterministic.  Other columns stay at natural width.
  (let ((agent-shell-markdown-table-max-width-fraction 1.0))
    (cl-letf (((symbol-function 'agent-shell-markdown--display-width)
               (lambda () 30)))
      (should (equal (substring-no-properties
                      (agent-shell-markdown-convert
                       "| A | B |
|---|---|
| short | this is a much longer description |"))
                     "│ A     │ B                  │
├───────┼────────────────────┤
│ short │ this is a much     │
│       │ longer description │")))))

(ert-deftest agent-shell-markdown-convert-table-output-wraps-both-cells ()
  ;; Both columns shrink and wrap when both are too wide.  Column
  ;; widths are allocated proportionally to their natural width.
  (let ((agent-shell-markdown-table-max-width-fraction 1.0))
    (cl-letf (((symbol-function 'agent-shell-markdown--display-width)
               (lambda () 30)))
      (should (equal (substring-no-properties
                      (agent-shell-markdown-convert
                       "| Header A | Header B |
|---|---|
| first quite long content | second cell also long enough |"))
                     "│ Header A    │ Header B    │
├─────────────┼─────────────┤
│ first       │ second      │
│ quite long  │ cell also   │
│ content     │ long enough │")))))

(ert-deftest agent-shell-markdown-mirrors-face-to-font-lock-face ()
  ;; Faces are mirrored to `font-lock-face' so our styling survives
  ;; `font-lock-mode' re-fontification in comint / shell-maker buffers.
  (let* ((s (agent-shell-markdown-convert "hello **world**"))
         (world-pos (string-match "world" s)))
    (should (eq 'agent-shell-markdown-bold (get-text-property world-pos 'face s)))
    (should (eq 'agent-shell-markdown-bold
                (get-text-property world-pos 'font-lock-face s)))
    ;; Composed faces (`(bold italic)') mirror as the same list.
    (let* ((composed (agent-shell-markdown-convert "_**X**_"))
           (x-pos (string-match "X" composed)))
      (should (equal '(agent-shell-markdown-bold agent-shell-markdown-italic)
                     (get-text-property x-pos 'face composed)))
      (should (equal '(agent-shell-markdown-bold agent-shell-markdown-italic)
                     (get-text-property x-pos 'font-lock-face composed))))))

(ert-deftest agent-shell-markdown-table-preserves-caller-text-properties ()
  ;; Caller-set text properties (here: a custom symbol) at the
  ;; table's start position must survive the render's delete+insert,
  ;; so callers can keep using text-property scans to bracket regions
  ;; — e.g., agent-shell uses `agent-shell-ui-state' to find blocks.
  (with-temp-buffer
    (insert "| A | B |
|---|---|
| 1 | 2 |")
    (put-text-property (point-min) (point-max) 'agent-shell-ui-state 'my-block)
    (agent-shell-markdown-replace-markup)
    ;; Every char in the rendered output should carry the tag.
    (should (eq 'my-block
                (get-text-property (point-min) 'agent-shell-ui-state)))
    (should (eq 'my-block
                (get-text-property (1- (point-max)) 'agent-shell-ui-state)))))

(ert-deftest agent-shell-markdown-table-extends-on-streamed-rows ()
  ;; First render a 3-row table.  Then append a 4th data row to the
  ;; buffer (simulating an LLM streaming more content) and re-render.
  ;; The renderer should see the stashed source on the already-rendered
  ;; region, combine it with the new ASCII row, and emit a single
  ;; 4-row table with recomputed column widths.  Trailing newlines on
  ;; each row signal completeness — the renderer defers rendering of a
  ;; trailing row that isn't yet `\\n'-terminated, since a streaming
  ;; chunk may have ended mid-row.
  (with-temp-buffer
    (insert "| Col | Width |
|---|---|
| 1 | 2 |
")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-max))
    (insert "| three | four |
")
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string))
                   "│ Col   │ Width │
├───────┼───────┤
│ 1     │ 2     │
│ three │ four  │
"))))

(ert-deftest agent-shell-markdown-table-folds-mid-stream-continuation ()
  ;; A streamed chunk may end mid-row (chunk boundary splits a
  ;; row's cells).  Each render commits the latest chars to a
  ;; prettified table.  The next chunk's continuation chars (no
  ;; leading newline — they extend the current last row) get folded
  ;; back into the rendered table's last source row, so the final
  ;; render shows all rows with consistent column widths and no
  ;; orphan raw markdown stuck on a `│' line.
  (with-temp-buffer
    ;; Chunk 1: 3-row table.  The last row is intentionally short
    ;; (4 cells; header has 5) with no trailing newline — the chunk
    ;; boundary fell mid-row.
    (insert "| # | Name | Role | Country | Status |
|---|---|---|---|---|
| 1 | Alice | Engineer | USA |")
    (agent-shell-markdown-replace-markup)
    ;; Chunk 2: the continuation of row 1 (the missing `Status'
    ;; cell — note it starts with a space, not a newline) plus a
    ;; complete row 2.
    (goto-char (point-max))
    (insert " Active |
| 2 | Bob | Designer | UK | Historical |
")
    (agent-shell-markdown-replace-markup)
    ;; All rows render as a single 4-row table with the continuation
    ;; folded into row 1.  Column widths are consistent.
    (should (equal (substring-no-properties (buffer-string))
                   "│ # │ Name  │ Role     │ Country │ Status     │
├───┼───────┼──────────┼─────────┼────────────┤
│ 1 │ Alice │ Engineer │ USA     │ Active     │
│ 2 │ Bob   │ Designer │ UK      │ Historical │
"))))

(ert-deftest agent-shell-markdown-table-inside-open-fence-stays-raw ()
  ;; A table inside a fenced block whose closing fence hasn't
  ;; streamed in yet must NOT get table-rendered.  Otherwise the
  ;; rendered table would survive when the closing fence finally
  ;; arrives and the source-block pass strips the fences — the
  ;; user would see a styled table where they asked for verbatim
  ;; code.
  (with-temp-buffer
    (insert "```
| A | B |
|---|---|
| 1 | 2 |
")
    (agent-shell-markdown-replace-markup)
    ;; The pipes stay as ASCII `|', not unicode `│' — the table
    ;; renderer respected the open-fence range.
    (should (string-match-p "| A | B |" (buffer-string)))
    (should-not (string-match-p "│" (buffer-string)))))

(ert-deftest agent-shell-markdown-table-renders-final-row-without-trailing-newline ()
  ;; A complete table whose last row isn't terminated by `\n' (e.g.
  ;; the final chunk of a streaming response) must still render —
  ;; callers like agent-shell narrow to the body section, which
  ;; excludes the trailing `\n', so even when streaming has stopped
  ;; the row would appear unterminated within the narrow.
  (with-temp-buffer
    (insert "| Name | Age |
|---|---|
| Alice | 28 |
| Bob | 35 |")
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string))
                   "│ Name  │ Age │
├───────┼─────┤
│ Alice │ 28  │
│ Bob   │ 35  │"))))

(ert-deftest agent-shell-markdown-table-renders-with-field-boundaries ()
  ;; Callers (e.g. agent-shell) tag body chars with the `field' text
  ;; property.  Streamed chunks may not propagate `field' onto inter-
  ;; row newlines uniformly, creating field boundaries inside the table
  ;; source.  `forward-line' / `line-end-position' are field-aware by
  ;; default, so without protection the parsers would stop at those
  ;; boundaries and render some rows as empty `││'.
  (with-temp-buffer
    (insert "| Name | Age |
|---|---|
| Alice | 28 |
| Bob | 35 |
| Carol | 42 |
")
    ;; Strip `field' from the inter-row newlines while leaving it on
    ;; the row content — mimics the agent-shell streaming-chunk shape
    ;; that triggered the original bug.
    (put-text-property (point-min) (point-max) 'field 'output)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (remove-text-properties (1- (point)) (point) '(field nil))))
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string))
                   "│ Name  │ Age │
├───────┼─────┤
│ Alice │ 28  │
│ Bob   │ 35  │
│ Carol │ 42  │
"))))

(ert-deftest agent-shell-markdown-pad-table-string-accepts-force-pixel ()
  ;; `--pad-table-string' grew a `:force-pixel' keyword so the per-line
  ;; padding can be pinned to the pixel path for every wrapped line of
  ;; a non-ASCII cell.  This pins the keyword in the signature — if it
  ;; gets dropped, the row-renderer (which always passes it) would
  ;; error with "Keyword argument :force-pixel not one of ...".
  ;; In batch (no graphic display) the pixel path is unreachable, so
  ;; both `:force-pixel t' and `:force-pixel nil' fall back to the
  ;; ASCII path and produce the same string.  The test just guards the
  ;; signature and the ASCII-path result.
  (should (equal "hi  "
                 (agent-shell-markdown--pad-table-string
                  :str "hi" :width 4 :force-pixel nil)))
  (should (equal "hi  "
                 (agent-shell-markdown--pad-table-string
                  :str "hi" :width 4 :force-pixel t))))

(ert-deftest agent-shell-markdown-pad-table-string-empty-line ()
  ;; Empty continuation lines (the `""' a row-renderer hands to padding
  ;; when a cell wraps fewer times than the row's max) must always
  ;; render as exactly WIDTH spaces.  The pixel path used to be skipped
  ;; for empty strings via the row-renderer's caller-side guard — pin
  ;; the contract here so the guard stays in place.
  (should (equal "    "
                 (agent-shell-markdown--pad-table-string
                  :str "" :width 4)))
  (should (equal "    "
                 (agent-shell-markdown--pad-table-string
                  :str "" :width 4 :force-pixel t))))

(ert-deftest agent-shell-markdown-table-wrap-text-respects-vs-16-width ()
  ;; `⚠' alone has `string-width' 1, but `⚠\\uFE0F' (`⚠️') renders 2
  ;; cells (VS-16 forces emoji presentation).  `string-width' reports
  ;; 8 for `⚠️ Review' while the rendered width is 9 — without VS-16
  ;; awareness the wrap function lets it fit in a 8-col column and the
  ;; rendered cell overflows by 1, pushing every subsequent pipe right.
  ;; By contrast `❌ Killed' (`string-width' 9) wraps in the same
  ;; column, producing visibly asymmetric misalignment.  Both should
  ;; wrap.
  (should (equal '("⚠️" "Review")
                 (agent-shell-markdown--table-wrap-text "⚠️ Review" 8)))
  (should (equal '("❌" "Killed")
                 (agent-shell-markdown--table-wrap-text "❌ Killed" 8)))
  ;; Both fit at col 9 (matching their rendered widths).
  (should (equal '("⚠️ Review")
                 (agent-shell-markdown--table-wrap-text "⚠️ Review" 9)))
  (should (equal '("❌ Killed")
                 (agent-shell-markdown--table-wrap-text "❌ Killed" 9))))

(ert-deftest agent-shell-markdown-text-has-face-p ()
  ;; Detects whether a string carries a `face' property anywhere —
  ;; the trigger for routing table cell measurement through the
  ;; pixel-accurate path when a theme's inline-code/bold/etc. face
  ;; renders at a different pixel width than `string-width' reports.
  (should-not (agent-shell-markdown--text-has-face-p "plain"))
  (should (agent-shell-markdown--text-has-face-p
           (propertize "styled" 'face 'bold)))
  ;; Mid-string face is detected too — not just at position 0.
  (should (agent-shell-markdown--text-has-face-p
           (concat "a" (propertize "b" 'face 'bold) "c"))))

(ert-deftest agent-shell-markdown-table-wrap-text-accepts-window ()
  ;; `--table-wrap-text' grew an optional WINDOW arg so wrap decisions
  ;; can factor in face-induced pixel widening (themes that style
  ;; inline-code with a wider font would otherwise let an N-char wrap
  ;; line overflow an N-cell column in pixel terms and push the right
  ;; pipe out of line).  Pin the signature here and the no-window
  ;; behaviour (existing char-width path, unchanged).  In batch the
  ;; pixel path is unreachable, so a non-nil WINDOW falls back to the
  ;; char-width path and produces the same wrap as the 2-arg call.
  (should (equal '("hello" "world")
                 (agent-shell-markdown--table-wrap-text "hello world" 5)))
  (should (equal '("hello" "world")
                 (agent-shell-markdown--table-wrap-text
                  "hello world" 5 nil))))

(ert-deftest agent-shell-markdown-table-wrap-char-width-accepts-window ()
  ;; `--table-wrap-char-width' grew an optional WINDOW arg so styled
  ;; chars can scale by the face's measured pixel-width ratio.  Pin
  ;; the signature and the no-window behaviour (char-width / VS-16
  ;; correction unchanged).
  (should (= 1 (agent-shell-markdown--table-wrap-char-width "a" 0)))
  (should (= 1 (agent-shell-markdown--table-wrap-char-width "a" 0 nil)))
  ;; VS-16 still gets its width-1 attribution under both signatures.
  (should (= 1 (agent-shell-markdown--table-wrap-char-width "⚠️" 1)))
  (should (= 1 (agent-shell-markdown--table-wrap-char-width "⚠️" 1 nil))))

(ert-deftest agent-shell-markdown-table-apply-height-scaling-short-circuits ()
  ;; ASCII-only strings skip the per-char height measurement loop and
  ;; pass through unchanged (the costly `window-text-pixel-size'
  ;; measurement is only worth it when there are non-ASCII glyphs
  ;; that might render taller than the default line height).  In
  ;; non-graphic display (`display-graphic-p' nil — batch / TTY) the
  ;; whole pass is a no-op since the measurement APIs aren't available.
  (let ((input "Auth System"))
    (should (equal input
                   (agent-shell-markdown--table-apply-height-scaling input))))
  ;; In batch (no graphic display), even non-ASCII passes through
  ;; unchanged.  The function still returns a string.
  (should (stringp
           (agent-shell-markdown--table-apply-height-scaling "⚠️ Review"))))

(ert-deftest agent-shell-markdown-table-next-cell-walks-cells-in-order ()
  ;; Cells walk row-by-row, skipping the separator, and signal
  ;; `user-error' at the table boundary.
  (with-temp-buffer
    (insert "| A | B |
|---|---|
| 1 | 2 |
")
    (agent-shell-markdown-replace-markup)
    ;; Point at A.
    (goto-char (point-min))
    (search-forward "A")
    (backward-char)
    (agent-shell-markdown-table-next-cell)
    (should (eq (char-after) ?B))
    (agent-shell-markdown-table-next-cell)
    (should (eq (char-after) ?1))
    (agent-shell-markdown-table-next-cell)
    (should (eq (char-after) ?2))
    (should-error (agent-shell-markdown-table-next-cell) :type 'user-error)))

(ert-deftest agent-shell-markdown-table-previous-cell-walks-cells-in-reverse ()
  (with-temp-buffer
    (insert "| A | B |
|---|---|
| 1 | 2 |
")
    (agent-shell-markdown-replace-markup)
    ;; Point at 2.
    (goto-char (point-min))
    (search-forward "2")
    (backward-char)
    (agent-shell-markdown-table-previous-cell)
    (should (eq (char-after) ?1))
    (agent-shell-markdown-table-previous-cell)
    (should (eq (char-after) ?B))
    (agent-shell-markdown-table-previous-cell)
    (should (eq (char-after) ?A))
    (should-error (agent-shell-markdown-table-previous-cell) :type 'user-error)))

(ert-deftest agent-shell-markdown-table-next-cell-skips-wrapped-continuation ()
  ;; A wrapped row spans multiple physical lines; only the first
  ;; line carries navigable cells.  Continuation lines (with the
  ;; remainder of wrapped content in some cells, padding in others)
  ;; must not register as separate cells.
  (let ((agent-shell-markdown-table-max-width-fraction 1.0))
    (cl-letf (((symbol-function 'agent-shell-markdown--display-width)
               (lambda () 30)))
      (with-temp-buffer
        (insert "| A | B |
|---|---|
| short | this is a much longer description |
")
        (agent-shell-markdown-replace-markup)
        ;; The rendered table has the data row wrapped to 2 physical
        ;; lines.  There should be exactly 4 navigable cells: A, B
        ;; (header), short, "this is a much" (the data row's first
        ;; line — but logically one cell, "this is a much longer
        ;; description").
        (goto-char (point-min))
        (search-forward "A")
        (backward-char)
        (agent-shell-markdown-table-next-cell)
        (should (eq (char-after) ?B))
        (agent-shell-markdown-table-next-cell)
        (should (looking-at-p "short"))
        (agent-shell-markdown-table-next-cell)
        (should (looking-at-p "this is a much"))
        ;; The continuation line "longer description" is NOT a cell.
        (should-error (agent-shell-markdown-table-next-cell) :type 'user-error)))))

(ert-deftest agent-shell-markdown-table-next-cell-errors-outside-table ()
  (with-temp-buffer
    (insert "not a table at all")
    (goto-char (point-min))
    (should-error (agent-shell-markdown-table-next-cell) :type 'user-error)
    (should-error (agent-shell-markdown-table-previous-cell) :type 'user-error)))

(ert-deftest agent-shell-markdown-convert-table-in-fenced-block-untouched ()
  ;; A table inside a fenced block stays untouched (source-block body
  ;; is frozen, so table detection skips it — and source-block fences
  ;; are themselves deleted, but the body chars stay literal).
  (let ((s (agent-shell-markdown-convert "```
| A | B |
|---|---|
| 1 | 2 |
```")))
    (should (string-match-p "| A | B |" s))
    (should (not (string-match-p "│" s)))))

(ert-deftest agent-shell-markdown-convert-everything ()
  (should (equal
           (agent-shell-markdown--deconstruct
            (agent-shell-markdown-convert
             "# Top

Some **bold** and _italic_ with ~~strike~~ done.

---

## Sub with **mixed _both_ end**

A [link](https://example.com) and `code`.

```
**not bold**
```

![alt](/missing).

| A | B |
|---|---|
| 1 | 2 |"))
           '(("Top" (agent-shell-markdown-header-1))
             ("

Some " nil)
             ("bold" (agent-shell-markdown-bold))
             (" and " nil)
             ("italic" (agent-shell-markdown-italic))
             (" with " nil)
             ("strike" (agent-shell-markdown-strikethrough))
             (" done.

---

" nil)
             ("Sub with " (agent-shell-markdown-header-2))
             ("mixed " (agent-shell-markdown-header-2 agent-shell-markdown-bold))
             ("both" (agent-shell-markdown-header-2 agent-shell-markdown-bold agent-shell-markdown-italic))
             (" end" (agent-shell-markdown-header-2 agent-shell-markdown-bold))
             ("

A " nil)
             ("link" (agent-shell-markdown-link))
             (" and " nil)
             ("code" (agent-shell-markdown-inline-code))
             (".

" nil)
             ("snippet ⧉" (agent-shell-markdown-source-block-language))
             ("

**not bold**

![alt](/missing).

" nil)
             ("│" (agent-shell-markdown-table-border))
             (" A " (agent-shell-markdown-table-header))
             ("│" (agent-shell-markdown-table-border))
             (" B " (agent-shell-markdown-table-header))
             ("│" (agent-shell-markdown-table-border))
             ("
" nil)
             ("├───┼───┤" (agent-shell-markdown-table-border))
             ("
" nil)
             ("│" (agent-shell-markdown-table-border))
             (" 1 " nil)
             ("│" (agent-shell-markdown-table-border))
             (" 2 " nil)
             ("│" (agent-shell-markdown-table-border))))))

(ert-deftest agent-shell-markdown-watermark-skips-prefix-on-streamed-append ()
  ;; After a render, the prefix carries the watermark text property and
  ;; the next render — narrowed to (watermark, point-max) — must not
  ;; revisit the rendered prefix.  Verify by injecting a sentinel
  ;; `font-lock-face' at point-min after the first render; the mirror
  ;; pass on the second render would overwrite it if the prefix were
  ;; re-scanned, but with the watermark in place it stays put.
  (with-temp-buffer
    (insert "**hello**\n")
    (agent-shell-markdown-replace-markup)
    (put-text-property (point-min) (1+ (point-min))
                       'font-lock-face 'agent-shell-markdown-test-sentinel)
    (goto-char (point-max))
    (insert "**world**\n")
    (agent-shell-markdown-replace-markup)
    (should (eq (get-text-property (point-min) 'font-lock-face)
                'agent-shell-markdown-test-sentinel))
    ;; And the newly-streamed bold still rendered normally.
    (should (string-match-p "^hello\nworld\n$"
                            (substring-no-properties (buffer-string))))))

(ert-deftest agent-shell-markdown-yank-strips-properties ()
  ;; Rendered chars carry a `yank-handler' that strips every text
  ;; property on paste — display overrides, internal markers, faces,
  ;; keymaps — so a copy/paste into another buffer gives plain chars,
  ;; not our implementation cruft.
  (with-temp-buffer
    (insert "**bold** and `code`\n")
    (agent-shell-markdown-replace-markup)
    (kill-new (buffer-substring (point-min) (point-max))))
  (with-temp-buffer
    (yank)
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (should-not (text-properties-at pos))
        (setq pos (1+ pos))))))

(ert-deftest agent-shell-markdown-convert-blockquote-single-level ()
  ;; `> text\n' keeps the `>' in the buffer (source round-trips) but
  ;; shows `▌' as a display override.  The line content carries the
  ;; blockquote face.
  (let ((s (agent-shell-markdown-convert "> hello\n")))
    (should (equal (substring-no-properties s) "> hello\n"))
    (should (equal (get-text-property 0 'display s)
                   (propertize "▌"
                              'face 'agent-shell-markdown-blockquote)))
    (should (eq (get-text-property 2 'face s)
                'agent-shell-markdown-blockquote))
    (should (eq (get-text-property 0 'agent-shell-markdown-frozen s) t))))

(ert-deftest agent-shell-markdown-convert-blockquote-multi-level ()
  ;; Each leading `>' gets its own bar — `>> ' shows two, `>>> '
  ;; shows three.  Whitespace between `>'s is preserved.
  (let ((s (agent-shell-markdown-convert ">> level 2\n")))
    (should (equal (get-text-property 0 'display s)
                   (propertize "▌"
                              'face 'agent-shell-markdown-blockquote)))
    (should (equal (get-text-property 1 'display s)
                   (propertize "▌"
                              'face 'agent-shell-markdown-blockquote))))
  (let ((s (agent-shell-markdown-convert ">>> level 3\n")))
    (dolist (i '(0 1 2))
      (should (equal (get-text-property i 'display s)
                     (propertize "▌"
                                'face 'agent-shell-markdown-blockquote))))))

(ert-deftest agent-shell-markdown-convert-blockquote-with-bold ()
  ;; Inline markup inside a blockquote still renders — bold runs
  ;; before blockquote, and the blockquote face composes on top so
  ;; the bold text ends up with both faces.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "> hello **world**\n"))
                 '(("> hello " (agent-shell-markdown-blockquote))
                   ("world" (agent-shell-markdown-blockquote
                             agent-shell-markdown-bold))
                   ("\n" nil)))))

(ert-deftest agent-shell-markdown-blockquote-waits-for-newline-across-chunks ()
  ;; A blockquote line streamed across two chunks (`> hel' then `lo\n')
  ;; must not render until the line completes — otherwise `> hel'
  ;; would face only `hel' and leave the rest plain on the next call.
  (with-temp-buffer
    (insert "> hel")
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string)) "> hel"))
    (should-not (get-text-property (point-min) 'display))
    (goto-char (point-max))
    (insert "lo\n")
    (agent-shell-markdown-replace-markup)
    (should (equal (get-text-property (point-min) 'display)
                   (propertize "▌"
                              'face 'agent-shell-markdown-blockquote)))
    (should (eq (get-text-property (+ (point-min) 2) 'face)
                'agent-shell-markdown-blockquote))))

(ert-deftest agent-shell-markdown-blockquote-inside-fence-stays-raw ()
  ;; A `>'-prefixed line inside a fenced code block must not be
  ;; styled as a blockquote — the source-block range is in
  ;; avoid-ranges.  The `>' carries the source-block's
  ;; `agent-shell-markdown-frozen' tag and no blockquote face.
  (let* ((s (agent-shell-markdown-convert "```
> not a quote
```
"))
         (quote-pos (string-match "> not a quote"
                                  (substring-no-properties s))))
    (should quote-pos)
    (should (eq t (get-text-property quote-pos 'agent-shell-markdown-frozen s)))
    (should-not (eq (get-text-property quote-pos 'face s)
                    'agent-shell-markdown-blockquote))))

(ert-deftest agent-shell-markdown-header-waits-for-newline-across-chunks ()
  ;; A header split across two chunks (chunk 1 = `# He', chunk 2 =
  ;; `llo World\\n') must not render eagerly on chunk 1 — the
  ;; trailing-newline gate keeps `# He' raw, and chunk 2's render
  ;; faces the entire `Hello World' once the line completes.
  (with-temp-buffer
    (insert "# He")
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string)) "# He"))
    (goto-char (point-max))
    (insert "llo World\n")
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string))
                   "Hello World\n"))
    (dotimes (i (length "Hello World"))
      (should (eq (get-text-property (+ (point-min) i) 'face)
                  'agent-shell-markdown-header-1)))))

(ert-deftest agent-shell-markdown-frozen-region-skips-header-pass ()
  ;; Callers (eg. `agent-shell--format-diff-as-text') tag pre-rendered
  ;; content with `agent-shell-markdown-frozen t' so it displays verbatim.
  ;; The header pass must respect that tag — a diff context line like
  ;; ` # Foo' must not be rewritten as an H1.  See PR #597.
  (with-temp-buffer
    (insert (propertize "@@ -1,2 +1,2 @@\n # Test Document Title\n-old\n+new\n"
                        'agent-shell-markdown-frozen t))
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string))
                   "@@ -1,2 +1,2 @@\n # Test Document Title\n-old\n+new\n"))))

(ert-deftest agent-shell-markdown-header-preserves-caller-text-properties ()
  ;; The header pass deletes the matched `#…\n' and re-inserts the
  ;; faced title plus a fresh `\n'.  The inserted newline must carry
  ;; the caller's text properties — otherwise it punches a hole in any
  ;; contiguous block tagging (eg. `invisible' / `agent-shell-ui-section')
  ;; that brackets the body, breaking toggle/replace operations on the
  ;; surrounding fragment.  See PR #597.
  (with-temp-buffer
    (insert (propertize "# Title\nbody line\n"
                        'agent-shell-ui-section 'body
                        'invisible t))
    (agent-shell-markdown-replace-markup)
    (dotimes (i (1- (point-max)))
      (let ((pos (1+ i)))
        (should (eq 'body
                    (get-text-property pos 'agent-shell-ui-section)))
        (should (eq t (get-text-property pos 'invisible)))))))

(ert-deftest agent-shell-markdown-watermark-keeps-pending-table-in-scope ()
  ;; When table rows stream in one at a time, the table needs at least
  ;; two consecutive pipe-rows in scope before `--find-tables' will
  ;; render anything.  If the watermark advances past each row as it
  ;; arrives, the renderer never sees enough rows at once and the
  ;; whole table stays raw forever.  `--extending-table-start' has to
  ;; back off through a streak of raw pipe-rows just like it does
  ;; through a rendered table, so the next chunk's narrow includes the
  ;; whole accumulating table.
  (with-temp-buffer
    (insert "intro paragraph\n\n")
    (agent-shell-markdown-replace-markup)
    (dolist (row '("| A | B |\n"
                   "|---|---|\n"
                   "| 1 | 2 |\n"
                   "| 3 | 4 |\n"))
      (goto-char (point-max))
      (insert row)
      (agent-shell-markdown-replace-markup))
    (should (string-match-p "│"
                            (substring-no-properties (buffer-string))))
    (should-not (string-match-p "^| A | B |"
                                (substring-no-properties (buffer-string))))))

(ert-deftest agent-shell-markdown-watermark-keeps-pending-table-with-partial-separator ()
  ;; Real-world regression: an LLM streams a 5-column table cell-by-
  ;; cell and the separator row arrives as a sequence of `|-------'
  ;; chunks that aren't a complete pipe-row until the trailing `|'
  ;; lands.  While the separator is mid-stream, the strict pipe-row
  ;; regex doesn't match (it needs the closing `|'); the lenient
  ;; pending-line regex must still recognise it so the watermark
  ;; stays at the header line.  Otherwise the watermark slips past
  ;; the header and `--find-tables' eventually renders only
  ;; separator + data rows, leaving the header raw outside the table.
  (with-temp-buffer
    (dolist (chunk '("| Col 1 | Col 2 |\n"
                     "|-------"
                     "|-------"
                     "|"
                     "\n"
                     "| Row 1 | A |\n"
                     "| Row 2 | B |\n"))
      (goto-char (point-max))
      (insert chunk)
      (agent-shell-markdown-replace-markup))
    (let ((rendered (substring-no-properties (buffer-string))))
      ;; Header is part of the rendered Unicode table — no raw `|' on
      ;; its line.
      (should (string-match-p "│ Col 1 *│ Col 2 *│" rendered))
      (should-not (string-match-p "^| Col 1" rendered)))))

(ert-deftest agent-shell-markdown-inline-code-completes-across-chunk-boundary ()
  ;; LLM streams may split an inline-code span across chunks (e.g.
  ;; `\\`co' lands first, then `de\\`').  The first render sees an
  ;; unclosed backtick on the last line — `--inline-code-ranges' marks
  ;; the rest of the line as a still-streaming range so `--style-
  ;; inline-code's two-backtick regex doesn't match yet, and the
  ;; watermark stays at the start of that line.  When the closing
  ;; backtick arrives on the same line in the next chunk, the second
  ;; render matches the full span and strips both backticks.
  ;;
  ;; This regression-guards the watermark too: if a future change
  ;; advanced the watermark past the open backtick, the second render
  ;; would narrow past the opener and leave it raw.
  (with-temp-buffer
    (insert "text `co")
    (agent-shell-markdown-replace-markup)
    (should (string-match-p "`co"
                            (substring-no-properties (buffer-string))))
    (goto-char (point-max))
    (insert "de`")
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string))
                   "text code"))
    (should (eq (get-text-property (- (point-max) 1) 'face)
                'agent-shell-markdown-inline-code))))

(ert-deftest agent-shell-markdown-replace-markup-force-clears-watermark ()
  ;; The `:force' key drops the stored watermark before the call, so
  ;; the whole buffer is re-scanned.  We simulate a maximally
  ;; advanced watermark by stamping one at `point-max' — a non-force
  ;; call narrows to (point-max, point-max) and is a no-op; a `:force
  ;; t' call clears the watermark first and renders normally.
  (with-temp-buffer
    (insert "**bold**\n")
    (with-silent-modifications
      (put-text-property (point-min) (1+ (point-min))
                         'agent-shell-markdown-watermark (point-max)))
    (agent-shell-markdown-replace-markup)
    (should (string-match-p "\\*\\*bold\\*\\*"
                            (substring-no-properties (buffer-string))))
    (agent-shell-markdown-replace-markup :force t)
    (should-not (string-match-p "\\*\\*"
                                (substring-no-properties (buffer-string))))))

(provide 'agent-shell-markdown-tests)

;;; agent-shell-markdown-tests.el ends here
