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
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "# Title"))
                 '(("Title" (agent-shell-markdown-header-1))))))

(ert-deftest agent-shell-markdown-convert-header-level-3 ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "### Title"))
                 '(("Title" (agent-shell-markdown-header-3))))))

(ert-deftest agent-shell-markdown-convert-header-with-bold ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "## **Big** title"))
                 '(("Big" (agent-shell-markdown-header-2 agent-shell-markdown-bold))
                   (" title" (agent-shell-markdown-header-2))))))

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
                   ("**not bold**
_not italic_
" (agent-shell-markdown-source-block))
                   ("after " nil)
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
  ;; The `[b](v)' inside fences stays literal (it isn't re-processed
  ;; as a link), but rendered source-block bodies now carry the
  ;; `agent-shell-markdown-source-block' background face.
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
                   ("[b](v)
" (agent-shell-markdown-source-block))
                   ("after " nil)
                   ("c" (agent-shell-markdown-link))))))

(ert-deftest agent-shell-markdown-convert-source-block-no-language ()
  ;; Plain fenced block (no language): fences deleted, body remains.
  ;; Body chars carry the `agent-shell-markdown-source-block' bg face
  ;; (and the `agent-shell-markdown-frozen' tag, which `--deconstruct'
  ;; doesn't surface).  The body region includes the trailing `\\n'
  ;; so `:extend t' on the bg face reaches the right edge of the
  ;; window on the last line too.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "```
body
```"))
                 '(("body
" (agent-shell-markdown-source-block))))))

(ert-deftest agent-shell-markdown-convert-source-block-with-language ()
  ;; `emacs-lisp' source block: fences deleted, body chars get
  ;; `emacs-lisp-mode' font-lock faces *plus* the
  ;; `agent-shell-markdown-source-block' background face (layered
  ;; with `add-face-text-property' APPEND so it ends up at the tail
  ;; of the cascade, behind the language's font-lock).  In batch the
  ;; keyword `if' is faced.  The trailing `\\n' isn't part of the
  ;; body region and stays unfaced.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "```emacs-lisp
(if t nil)
```"))
                 '(("(" (agent-shell-markdown-source-block))
                   ("if" (font-lock-keyword-face
                          agent-shell-markdown-source-block))
                   (" t nil)
" (agent-shell-markdown-source-block))))))

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
                   '(("**not bold**
" (agent-shell-markdown-source-block))
                     ("
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
             ("**not bold**
" (agent-shell-markdown-source-block))
             ("
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

(provide 'agent-shell-markdown-tests)

;;; agent-shell-markdown-tests.el ends here
