;;; agent-shell-markdown-tests.el --- Tests for agent-shell-markdown -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run via:
;;
;;   `emacs -batch -l ert -l tests/agent-shell-markdown-tests.el \
;;          -f ert-run-tests-batch-and-exit'

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

(ert-deftest agent-shell-markdown-escaped-asterisk-in-bold ()
  ;; Regression: `**let vs let\\***' is bold text ending in an escaped
  ;; literal `*' (CommonMark).  The escape lets the bold span match and
  ;; the `*' render literally rather than as a stray closing delimiter.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "**let vs let\\***"))
                 '(("let vs let*" (agent-shell-markdown-bold))))))

(ert-deftest agent-shell-markdown-escaped-punctuation-not-markup ()
  ;; A backslash-escaped delimiter renders as the bare char with no
  ;; emphasis: `\\_' stays `_', `\\*' stays `*'.
  (should (equal (substring-no-properties
                  (agent-shell-markdown-convert "a \\_b\\_ c and \\* star"))
                 "a _b_ c and * star")))

(ert-deftest agent-shell-markdown-escaped-punctuation-preserves-properties ()
  "Escaped punctuation retains caller-owned text properties."
  (with-temp-buffer
    (insert "den\\.")
    (add-text-properties (1- (point-max)) (point-max)
                         '(agent-shell-ui-state sentinel
                           agent-shell-ui-section label-right
                           read-only t))
    (let ((inhibit-read-only t))
      (agent-shell-markdown-replace-markup :render-images nil))
    (let ((period (1- (point-max))))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "den."))
      (should (eq (get-text-property period 'agent-shell-ui-state) 'sentinel))
      (should (eq (get-text-property period 'agent-shell-ui-section) 'label-right))
      (should (eq (get-text-property period 'read-only) t)))))

(ert-deftest agent-shell-markdown-escaped-punctuation-round-trips ()
  ;; The escaped markdown is stashed, so copy-as-markdown restores the
  ;; backslashes verbatim.
  (should (equal (agent-shell-markdown-tests--roundtrip "**let vs let\\***\n")
                 "**let vs let\\***\n"))
  (should (equal (agent-shell-markdown-tests--roundtrip "a \\* b\n")
                 "a \\* b\n")))

(ert-deftest agent-shell-markdown-escape-left-literal-inside-code ()
  ;; Inside a code span a backslash is literal, so `\\*' must NOT be
  ;; unescaped (and the `*' stays plain, not a delimiter).
  (should (equal (substring-no-properties
                  (agent-shell-markdown-convert "`a \\* b`"))
                 "a \\* b")))

(ert-deftest agent-shell-markdown-emphasis-preserves-frozen-region ()
  ;; An emphasis pass that WRAPS an already-frozen region (e.g. one an
  ;; external `agent-shell-markdown-render-functions' renderer claimed and
  ;; anchored an async result inside) must strip only the delimiters and
  ;; leave the inner text in place: deleting and re-inserting the whole
  ;; span used to collapse markers pointing into it and drop overlays.
  ;; Assert markers straddling the frozen text stay valid (still spanning
  ;; it), an overlay on it survives, and the frozen property is intact.
  (with-temp-buffer
    (insert "a **b FROZEN c**\n")
    (goto-char (point-min))
    (search-forward "FROZEN")
    (let* ((inner-start (match-beginning 0))
           (inner-end (match-end 0))
           (m-start (copy-marker inner-start))
           (m-end (copy-marker inner-end))
           (ov (make-overlay inner-start inner-end)))
      (overlay-put ov 'display "IMG")
      (put-text-property inner-start inner-end 'agent-shell-markdown-frozen t)
      (agent-shell-markdown--replace-bolds :avoid-ranges nil)
      ;; Delimiters gone, inner text kept.
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "a b FROZEN c\n"))
      ;; Markers still bracket the same text (not collapsed).
      (should (< (marker-position m-start) (marker-position m-end)))
      (should (equal (buffer-substring-no-properties
                      (marker-position m-start) (marker-position m-end))
                     "FROZEN"))
      ;; Overlay survived over the same text.
      (should (overlay-buffer ov))
      (should (equal (buffer-substring-no-properties
                      (overlay-start ov) (overlay-end ov))
                     "FROZEN"))
      ;; Frozen property intact, bold applied to the surrounding text.
      (should (get-text-property (marker-position m-start)
                                 'agent-shell-markdown-frozen))
      (should (memq 'agent-shell-markdown-bold
                    (let ((f (get-text-property (marker-position m-start)
                                                'face)))
                      (if (listp f) f (list f))))))))

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

(ert-deftest agent-shell-markdown-convert-italic-underscore-intraword ()
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "Echo _hello_world"))
                 '(("Echo _hello_world" nil)))))

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
                 ;; The block is framed by a blank line on each side (it
                 ;; butts against "before **b**" above and "after **b2**"
                 ;; below); those untinted `\\n's merge into the plain
                 ;; runs bracketing the block.
                 '(("before " nil)
                   ("b" (agent-shell-markdown-bold))
                   ("

" nil)
                   ("
" (agent-shell-markdown-source-block))
                   ("snippet ⧉" (agent-shell-markdown-source-block-language))
                   ("

**not bold**
_not italic_

" (agent-shell-markdown-source-block))
                   ("
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

(ert-deftest agent-shell-markdown-hint-stops-at-end-of-what-it-describes ()
  "A hint answers on its own text and not on the character after it.

`cursor-sensor' reads `cursor-sensor-functions' with
`get-pos-property' before falling back to `get-char-property', and
`get-pos-property' reports what text inserted at a position would
inherit rather than what sits there.  Unless the span is
rear-nonsticky the hint still answers one position past its end, so a
link's \"Press RET to open in browser\" shows while point sits on the
space after the link."
  (with-temp-buffer
    (insert "see [wool.com](https://wool.com/x) ok\n\n"
            "| A | L |\n|---|---|\n| x | [j.com](https://j.com/y) |\n\n"
            "```elisp\n(hi)\n```\n")
    (agent-shell-markdown-replace-markup :force t)
    (let ((pos (point-min))
          (spans 0))
      (while (setq pos (text-property-not-all
                        pos (point-max) 'cursor-sensor-functions nil))
        (let ((end (or (next-single-property-change pos 'cursor-sensor-functions)
                       (point-max))))
          (setq spans (1+ spans))
          ;; Reaches its own first character.
          (should (or (get-pos-property pos 'cursor-sensor-functions)
                      (get-char-property pos 'cursor-sensor-functions)))
          ;; ...and doesn't leak past its last.  Checked with
          ;; `get-pos-property' alone: `get-char-property' there would
          ;; report whatever span comes next, which is legitimate.
          (should-not (get-pos-property end 'cursor-sensor-functions))
          (setq pos end)))
      (should (> spans 0)))))

(ert-deftest agent-shell-markdown-convert-link-with-balanced-parens ()
  "A bare destination may carry balanced parens, as Wikipedia URLs do.

Without this the destination ended at the inner `)', truncating the URL
and leaving the outer `)' visible as prose."
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "see [Bender](https://en.wikipedia.org/wiki/Bender_(Futurama)) please"))
                 '(("see " nil)
                   ("Bender" (agent-shell-markdown-link))
                   (" please" nil))))
  (should (equal "https://en.wikipedia.org/wiki/Bender_(Futurama)"
                 (get-text-property
                  4 'agent-shell-markdown-url
                  (agent-shell-markdown-convert
                   "see [Bender](https://en.wikipedia.org/wiki/Bender_(Futurama)) please")))))

(ert-deftest agent-shell-markdown-convert-link-with-unbalanced-paren-left-literal ()
  "An unbalanced paren still ends the destination, so the markup stays put.

CommonMark asks for it to be escaped or angle-bracketed.  Nothing is
swallowed either way: the text reads verbatim, and the bare URL within
it is still picked up by the bare-URL linkifier."
  (let ((rendered (agent-shell-markdown-convert "see [x](http://a(b) please")))
    (should (equal "see [x](http://a(b) please" (substring-no-properties rendered)))
    ;; The label is left as prose rather than becoming a link title.
    (should-not (get-text-property 5 'agent-shell-markdown-url rendered))))

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

(ert-deftest agent-shell-markdown-convert-remote-image-falls-back-to-link ()
  ;; A remote image that can't be shown inline (no cache configured, and a
  ;; non-graphical display in batch) becomes a clickable link, not raw markup.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "see ![docs](https://example.com/a.png) end"))
                 '(("see " nil)
                   ("docs" (agent-shell-markdown-link))
                   (" end" nil)))))

(ert-deftest agent-shell-markdown-convert-remote-image-empty-alt-uses-url ()
  ;; With no alt text, the link label is the URL itself.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "![](https://example.com/a.png)"))
                 '(("https://example.com/a.png" (agent-shell-markdown-link))))))

(ert-deftest agent-shell-markdown-image-render-preserves-surrounding-properties ()
  ;; Regression: an inline `![alt](file)' image renders by replacing the
  ;; markup with a placeholder carrying the `display' image.  That placeholder
  ;; must keep the properties of the surrounding text, otherwise it punches a
  ;; hole in an otherwise-contiguous property run.  The shell tags a whole
  ;; streamed message body with `agent-shell-ui-section' body; a hole there
  ;; makes the fragment layer mis-locate the body on the next chunk and hide
  ;; every line after the image.  (`[title](url)' links already do this by
  ;; capturing the title with its properties; images used to drop them.)
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image)
                   (lambda (&rest _) '(image :type svg :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil)))
    (with-temp-buffer
      (insert (format "before\n\n![svg graphics](%s)\n\nafter\n" image-file))
      (put-text-property (point-min) (point-max) 'agent-shell-ui-section 'body)
      (agent-shell-markdown-replace-markup :render-images t)
      ;; The markup is replaced by the alt-text placeholder shown as an image.
      (should (equal (substring-no-properties (buffer-string))
                     "before\n\nsvg graphics\n\nafter\n"))
      (goto-char (point-min))
      (search-forward "svg graphics")
      (let ((placeholder-start (match-beginning 0)))
        ;; The placeholder carries the `display' image...
        (should (get-text-property placeholder-start 'display))
        ;; ...and still carries the surrounding body-section tag.
        (should (eq (get-text-property placeholder-start 'agent-shell-ui-section)
                    'body)))
      ;; The whole body stays one contiguous `agent-shell-ui-section' run --
      ;; the image placeholder leaves no gap for the fragment layer to trip on.
      (should-not (text-property-any (point-min) (point-max)
                                     'agent-shell-ui-section nil))))
      (delete-file image-file))))

(ert-deftest agent-shell-markdown-image-hints-open-key ()
  ;; A rendered image gives no hint that it's actionable beyond the hand
  ;; pointer, which only the mouse sees, so entering it echoes whichever
  ;; key its keymap binds to open the file.
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg"))
        (echoed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image)
                   (lambda (&rest _) '(image :type svg :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq echoed (apply #'format format-string args)))))
          (with-temp-buffer
            (insert (format "![svg graphics](%s)\n" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (goto-char (point-min))
            (funcall (seq-first (get-text-property (point) 'cursor-sensor-functions))
                     nil nil 'entered)
            (should (equal "Press RET to open image" echoed))))
      (delete-file image-file))))

(ert-deftest agent-shell-markdown-image-hints-resize-keys ()
  ;; Resizing is bound at mode level rather than on the image itself, so
  ;; the hint is the only thing making it discoverable.  It names the keys
  ;; `agent-shell-markdown-image-scale-commands' are bound to here, and
  ;; leaves resizing out entirely where they're bound to nothing (the
  ;; `agent-shell-markdown-image-hints-open-key' case above).
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg"))
        (echoed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image)
                   (lambda (&rest _) '(image :type svg :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq echoed (apply #'format format-string args)))))
          (with-temp-buffer
            (use-local-map
             (let ((map (make-sparse-keymap)))
               (define-key map (kbd "+") #'agent-shell-markdown-image-scale-increase)
               (define-key map (kbd "-") #'agent-shell-markdown-image-scale-decrease)
               (define-key map (kbd "0") #'agent-shell-markdown-image-scale-reset)
               map))
            (insert (format "![svg graphics](%s)\n" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (goto-char (point-min))
            (funcall (seq-first (get-text-property (point) 'cursor-sensor-functions))
                     nil nil 'entered)
            (should (equal "Press RET to open image, +/-/0 to resize" echoed))))
      (delete-file image-file))))

(ert-deftest agent-shell-markdown-image-hints-shell-resize-keys ()
  ;; `agent-shell-mode' binds resizing to its own wrappers (so `+' still
  ;; self-inserts while typing a prompt) rather than to the markdown
  ;; commands the viewport binds.  The hint looks up both names, so it
  ;; names keys that work there instead of going silent about resizing.
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg"))
        (echoed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image)
                   (lambda (&rest _) '(image :type svg :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq echoed (apply #'format format-string args)))))
          (with-temp-buffer
            ;; The wrappers live in agent-shell.el, which these tests don't
            ;; load -- binding the symbols is all the hint looks at.
            (use-local-map
             (let ((map (make-sparse-keymap)))
               (define-key map (kbd "+") 'agent-shell-image-scale-increase)
               (define-key map (kbd "-") 'agent-shell-image-scale-decrease)
               (define-key map (kbd "0") 'agent-shell-image-scale-reset)
               map))
            (insert (format "![svg graphics](%s)\n" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (goto-char (point-min))
            (funcall (seq-first (get-text-property (point) 'cursor-sensor-functions))
                     nil nil 'entered)
            (should (equal "Press RET to open image, +/-/0 to resize" echoed))))
      (delete-file image-file))))

(ert-deftest agent-shell-markdown-image-file-path-hints-open-key ()
  ;; A bare image-path line renders as an image too, and hints the same way.
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg"))
        (echoed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image)
                   (lambda (&rest _) '(image :type svg :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq echoed (apply #'format format-string args)))))
          (with-temp-buffer
            (insert image-file "\n")
            (agent-shell-markdown-replace-markup :render-images t)
            (goto-char (point-min))
            (funcall (seq-first (get-text-property (point) 'cursor-sensor-functions))
                     nil nil 'entered)
            (should (equal "Press RET to open image" echoed))))
      (delete-file image-file))))

(ert-deftest agent-shell-markdown-remote-image-link-hints-open-key ()
  ;; A remote image that falls back to a link hints at the browser rather
  ;; than at opening a file, since that's what invoking it does.
  (let ((rendered (agent-shell-markdown-convert
                   "![docs](https://example.com/a.png)"))
        (echoed nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq echoed (apply #'format format-string args)))))
      (funcall (seq-first (get-text-property 0 'cursor-sensor-functions rendered))
               nil nil 'entered)
      (should (equal "Press RET to open image in browser" echoed)))))

(ert-deftest agent-shell-markdown-link-hints-where-it-lands ()
  ;; A link looks actionable (link face), but not where invoking it goes: `agent-shell-markdown--open-link' visits local
  ;; files in Emacs and sends everything else to the browser.  The hint
  ;; names that, and is computed on entry so a file appearing or going
  ;; away later doesn't leave it lying.
  (let ((linked-file (make-temp-file "agent-shell-test" nil ".el"))
        (echoed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq echoed (apply #'format format-string args)))))
          (let ((rendered (agent-shell-markdown-convert
                           (format "[code](%s)" linked-file))))
            (funcall (seq-first (get-text-property
                                 0 'cursor-sensor-functions rendered))
                     nil nil 'entered)
            (should (equal "Press RET to open file" echoed)))
          (let ((rendered (agent-shell-markdown-convert
                           "[docs](https://example.com)")))
            (funcall (seq-first (get-text-property
                                 0 'cursor-sensor-functions rendered))
                     nil nil 'entered)
            (should (equal "Press RET to open in browser" echoed))))
      (delete-file linked-file))))

(ert-deftest agent-shell-markdown-link-help-echo-says-where-it-lands ()
  ;; The mouse gets the same wording the echo area does, minus the key
  ;; (a tooltip naming one is no use to whoever is holding a mouse).
  ;; `help-echo' is a function, so it resolves on hover like the hint.
  (let ((linked-file (make-temp-file "agent-shell-test" nil ".el")))
    (unwind-protect
        (progn
          (should (equal "Open file"
                         (funcall (get-text-property
                                   0 'help-echo
                                   (agent-shell-markdown-convert
                                    (format "[code](%s)" linked-file)))
                                  nil nil 0)))
          (should (equal "Open in browser"
                         (funcall (get-text-property
                                   0 'help-echo
                                   (agent-shell-markdown-convert
                                    "[docs](https://example.com)"))
                                  nil nil 0)))
          ;; A remote image falling back to a link keeps its own wording.
          (should (equal "Open image in browser"
                         (funcall (get-text-property
                                   0 'help-echo
                                   (agent-shell-markdown-convert
                                    "![docs](https://example.com/a.png)"))
                                  nil nil 0))))
      (delete-file linked-file))))

(ert-deftest agent-shell-markdown-image-help-echo-names-the-action ()
  ;; Images say the same to the mouse, leaving resizing out since it has
  ;; no mouse equivalent.
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image)
                   (lambda (&rest _) '(image :type svg :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil)))
          (with-temp-buffer
            (insert (format "![svg graphics](%s)\n" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (should (equal "Open image"
                           (get-text-property (point-min) 'help-echo))))
          ;; Same for a bare image path on its own line.
          (with-temp-buffer
            (insert (format "%s\n" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (should (equal "Open image"
                           (get-text-property (point-min) 'help-echo)))))
      (delete-file image-file))))

(ert-deftest agent-shell-markdown-image-reconstructs-to-source ()
  ;; A rendered `![alt](url)' image shows only the alt placeholder, but
  ;; `agent-shell-copy-as-markdown' must round-trip it back to the original
  ;; markdown.  The renderer stashes the source on `agent-shell-markdown-source'
  ;; (like links do) so `agent-shell-markdown-reconstruct' recovers it.
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image)
                   (lambda (&rest _) '(image :type svg :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil)))
          (with-temp-buffer
            (insert (format "see ![svg graphics](%s) end" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            ;; Visible text is the bare placeholder...
            (should (equal (substring-no-properties (buffer-string))
                           "see svg graphics end"))
            ;; ...but reconstruction restores the full markup.
            (should (equal (agent-shell-markdown-reconstruct (point-min) (point-max))
                           (format "see ![svg graphics](%s) end" image-file)))))
      (delete-file image-file))))

(ert-deftest agent-shell-markdown-remote-image-fallback-reconstructs-to-source ()
  ;; A remote image that falls back to a link (non-graphical display in batch)
  ;; also round-trips to its original `![alt](url)' markup, not the link label.
  (with-temp-buffer
    (insert "x ![](https://example.com/a.png) y")
    (agent-shell-markdown-replace-markup :render-images t)
    (should (equal (substring-no-properties (buffer-string))
                   "x https://example.com/a.png y"))
    (should (equal (agent-shell-markdown-reconstruct (point-min) (point-max))
                   "x ![](https://example.com/a.png) y"))))

(ert-deftest agent-shell-markdown-image-attributes-parse ()
  (should (equal (agent-shell-markdown--parse-image-attributes "width=300")
                 '((:max-width . 300))))
  (should (equal (agent-shell-markdown--parse-image-attributes "width=300px height=200")
                 '((:max-width . 300) (:max-height . 200))))
  ;; Classes / ids are ignored; only width / height are picked up.
  (should (equal (agent-shell-markdown--parse-image-attributes ".hero width=42")
                 '((:max-width . 42))))
  (should (null (agent-shell-markdown--parse-image-attributes "caption"))))

(ert-deftest agent-shell-markdown-image-attribute-ratio ()
  ;; A percentage yields its window fraction; a fixed pixel or bare
  ;; number yields nil (not window-relative).
  (should (= 0.5 (agent-shell-markdown--image-attribute-ratio "50%")))
  (should (= 0.2 (agent-shell-markdown--image-attribute-ratio "20%")))
  (should (null (agent-shell-markdown--image-attribute-ratio "300")))
  (should (null (agent-shell-markdown--image-attribute-ratio "300px"))))

(ert-deftest agent-shell-markdown-image-attributes-parse-percentage-ratio ()
  ;; A percentage width surfaces its window fraction on `:max-width-ratio'
  ;; (for `agent-shell-markdown-rerender-images') alongside the resolved
  ;; `:max-width' pixels; a fixed pixel width carries no ratio.
  (should (= 0.5 (map-elt (agent-shell-markdown--parse-image-attributes "width=50%")
                          :max-width-ratio)))
  (should (null (map-elt (agent-shell-markdown--parse-image-attributes "width=300")
                         :max-width-ratio))))

(ert-deftest agent-shell-markdown-image-render-stores-width-ratio ()
  ;; A percentage-width image records its window fraction and the width it
  ;; was sized against, so `agent-shell-markdown-rerender-images' can
  ;; re-size it; a fixed pixel width records no ratio.
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image) (lambda (&rest _) '(image :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil))
                  ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
                  ((symbol-function 'window-body-width) (lambda (&rest _) 1000)))
          (with-temp-buffer
            (insert (format "![a](%s){width=50%%}" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (should (= 0.5 (get-text-property
                            (point-min) 'agent-shell-markdown-image-width-ratio)))
            (should (= 1000 (get-text-property
                             (point-min) 'agent-shell-markdown-image-window-width))))
          ;; No attribute: sized by the default `image-max-width', so
          ;; tracked with the `default' sentinel (not a baked-in value),
          ;; whether that default is a ratio or a pixel width.
          ;; A trailing newline settles the streaming attribute guard.
          (with-temp-buffer
            (insert (format "![a](%s)\n" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (should (eq 'default (get-text-property
                                  (point-min) 'agent-shell-markdown-image-width-ratio))))
          (with-temp-buffer
            (insert (format "![a](%s)\n" image-file))
            (let ((agent-shell-markdown-image-max-width 300))
              (agent-shell-markdown-replace-markup :render-images t))
            (should (eq 'default (get-text-property
                                  (point-min) 'agent-shell-markdown-image-width-ratio))))
          (with-temp-buffer
            (insert (format "![a](%s){width=300px}" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (should (null (get-text-property
                           (point-min) 'agent-shell-markdown-image-width-ratio)))))
      (delete-file image-file))))

(ert-deftest agent-shell-markdown-rerender-images-resizes-stale-only ()
  ;; Re-size only images whose stored window width differs from the
  ;; current one, passing the max-width recomputed from the image's own
  ;; ratio; images already at the current width are left alone.
  (with-temp-buffer
    (insert "AB")
    (put-text-property 1 2 'agent-shell-markdown-image-width-ratio 0.5)
    (put-text-property 1 2 'agent-shell-markdown-image-window-width 1000)
    (put-text-property 2 3 'agent-shell-markdown-image-width-ratio 0.4)
    (put-text-property 2 3 'agent-shell-markdown-image-window-width 2000)
    (let ((calls nil))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
                ((symbol-function 'window-body-width) (lambda (&rest _) 2000))
                ((symbol-function 'agent-shell-markdown--resize-image)
                 (lambda (start _end max-width width)
                   (push (list start max-width width) calls))))
        (agent-shell-markdown-rerender-images)
        ;; Only the stale image (stored 1000), at 0.5 * 2000 = 1000 px.
        (should (equal calls '((1 1000 2000))))))))

(ert-deftest agent-shell-markdown-rerender-images-default-reads-live-max-width ()
  ;; A `default'-sized image (from `agent-shell-markdown-image-max-width')
  ;; re-reads that variable live on resize, so a changed setting applies
  ;; rather than a value baked in at render time.
  (with-temp-buffer
    (insert "X")
    (put-text-property (point-min) (point-max)
                       'agent-shell-markdown-image-width-ratio 'default)
    (put-text-property (point-min) (point-max)
                       'agent-shell-markdown-image-window-width 500)
    (let ((max-widths nil)
          (agent-shell-markdown-image-max-width 0.6))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
                ((symbol-function 'window-body-width) (lambda (&rest _) 1000))
                ((symbol-function 'agent-shell-markdown--resize-image)
                 (lambda (_s _e max-width _w) (push max-width max-widths))))
        (agent-shell-markdown-rerender-images)
        ;; 0.6 (the live value) * 1000 = 600.
        (should (equal max-widths '(600)))))))

(ert-deftest agent-shell-markdown-resize-image-applies-max-width ()
  ;; Applies the given `:max-width', keeps `:max-height', and records the
  ;; new window width.
  (with-temp-buffer
    (insert "X")
    (put-text-property (point-min) (point-max) 'display
                       '(image :file "x.png" :max-width 500 :max-height 40))
    (cl-letf (((symbol-function 'create-image)
               (lambda (_file &optional _type _data-p &rest props) (cons 'image props)))
              ((symbol-function 'image-flush) (lambda (&rest _) nil)))
      (agent-shell-markdown--resize-image (point-min) (point-max) 1000 2000))
    (let ((image (get-text-property (point-min) 'display)))
      (should (= 1000 (image-property image :max-width)))
      (should (= 40 (image-property image :max-height))))
    (should (= 2000 (get-text-property
                     (point-min) 'agent-shell-markdown-image-window-width)))))

(ert-deftest agent-shell-markdown-resize-image-skips-matching-max-width ()
  ;; An image already at the requested width isn't re-created (a pixel-sized
  ;; default sees the same width at every window width); only the window
  ;; width it's now measured against is recorded.
  (with-temp-buffer
    (insert "X")
    (put-text-property (point-min) (point-max) 'display
                       '(image :file "x.png" :max-width 500))
    (cl-letf (((symbol-function 'create-image)
               (lambda (&rest _) (error "Should not re-create the image"))))
      (agent-shell-markdown--resize-image (point-min) (point-max) 500 2000))
    (should (equal '(image :file "x.png" :max-width 500)
                   (get-text-property (point-min) 'display)))
    (should (= 2000 (get-text-property
                     (point-min) 'agent-shell-markdown-image-window-width)))))

(ert-deftest agent-shell-markdown-rerender-images-force-ignores-stored-width ()
  ;; `agent-shell-markdown-image-max-width' changing (rather than the
  ;; window) leaves the stored window width matching, so forcing is what
  ;; re-sizes the image.
  (with-temp-buffer
    (insert "X")
    (put-text-property (point-min) (point-max)
                       'agent-shell-markdown-image-width-ratio 'default)
    (put-text-property (point-min) (point-max)
                       'agent-shell-markdown-image-window-width 1000)
    (let ((max-widths nil)
          (agent-shell-markdown-image-max-width 300))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
                ((symbol-function 'window-body-width) (lambda (&rest _) 1000))
                ((symbol-function 'agent-shell-markdown--resize-image)
                 (lambda (_s _e max-width _w) (push max-width max-widths))))
        (agent-shell-markdown-rerender-images)
        (should (null max-widths))
        (agent-shell-markdown-rerender-images :force t)
        (should (equal max-widths '(300)))))))

(ert-deftest agent-shell-markdown-next-visible-image ()
  ;; Walks to the next image, skipping the one point is on, so repeated
  ;; calls advance rather than sticking.
  (with-temp-buffer
    (insert "a b c")
    (put-text-property 1 2 'display '(image :file "a.png"))
    (put-text-property 3 4 'display '(image :file "b.png"))
    (put-text-property 5 6 'display '(image :file "c.png"))
    (goto-char 1)
    (should (= 3 (agent-shell-markdown--next-visible-image)))
    (goto-char 3)
    (should (= 5 (agent-shell-markdown--next-visible-image)))
    (goto-char 5)
    (should (null (agent-shell-markdown--next-visible-image)))))

(ert-deftest agent-shell-markdown-next-visible-image-skips-hidden ()
  ;; An image hidden inside collapsed text isn't a navigation stop:
  ;; landing there would put point in text nothing shows.
  (with-temp-buffer
    (insert "a b c")
    (put-text-property 1 2 'display '(image :file "a.png"))
    (put-text-property 3 4 'display '(image :file "b.png"))
    (put-text-property 3 4 'invisible t)
    (put-text-property 5 6 'display '(image :file "c.png"))
    (goto-char 1)
    (should (= 5 (agent-shell-markdown--next-visible-image)))
    ;; Expanded, it's a stop again.
    (remove-list-of-text-properties 3 4 '(invisible))
    (goto-char 1)
    (should (= 3 (agent-shell-markdown--next-visible-image)))))

(ert-deftest agent-shell-markdown-previous-visible-image ()
  ;; Mirrors the forward search, hidden images included.
  (with-temp-buffer
    (insert "a b c")
    (put-text-property 1 2 'display '(image :file "a.png"))
    (put-text-property 3 4 'display '(image :file "b.png"))
    (put-text-property 5 6 'display '(image :file "c.png"))
    (goto-char 5)
    (should (= 3 (agent-shell-markdown--previous-visible-image)))
    (goto-char 3)
    (should (= 1 (agent-shell-markdown--previous-visible-image)))
    (goto-char 1)
    (should (null (agent-shell-markdown--previous-visible-image)))
    (put-text-property 3 4 'invisible t)
    (goto-char 5)
    (should (= 1 (agent-shell-markdown--previous-visible-image)))))

(ert-deftest agent-shell-markdown-linkify-bare-url ()
  ;; A bare URL in prose keeps its text and becomes a link: faced,
  ;; carrying its target for copy/export, and a stop for item navigation.
  (with-temp-buffer
    (insert "see https://example.com now.")
    (agent-shell-markdown-replace-markup)
    (should (equal "see https://example.com now."
                   (buffer-substring-no-properties (point-min) (point-max))))
    ;; Trailing punctuation stays out of the target.
    (should (equal "https://example.com"
                   (agent-shell-markdown-link-url-at-point 5)))
    (should (equal 'agent-shell-markdown-link (get-text-property 5 'face)))
    (should (get-text-property 5 'keymap))
    (should (get-text-property 5 'cursor-sensor-functions))
    (goto-char (point-min))
    (should (= 5 (agent-shell-markdown--next-visible-link)))))

(ert-deftest agent-shell-markdown-linkify-bare-url-skips-code ()
  ;; Code is verbatim: a URL inside inline code or a fenced block is text,
  ;; not a link.
  (with-temp-buffer
    (insert "code `https://example.com` here")
    (agent-shell-markdown-replace-markup)
    (should (equal "code https://example.com here"
                   (buffer-substring-no-properties (point-min) (point-max))))
    (should (null (agent-shell-markdown-link-url-at-point 6))))
  (with-temp-buffer
    (insert "```\nhttps://example.com\n```\n")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-min))
    (should (null (text-property-search-forward 'agent-shell-markdown-url)))))

(ert-deftest agent-shell-markdown-linkify-bare-url-leaves-rendered-images ()
  ;; A `file://' image path renders as the image, and stays that: linking
  ;; it as a URL too would swap the keymap opening the file for one
  ;; opening the URL, and make one image two navigation stops.
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image) (lambda (&rest _) '(image :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil)))
          (with-temp-buffer
            (insert (format "file://%s\n" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (should (eq 'image (car-safe (get-text-property (point-min) 'display))))
            (should (null (agent-shell-markdown-link-url-at-point (point-min))))))
      (delete-file image-file)))
  ;; A `file://' target that isn't an image is a link, as any URL is.
  (with-temp-buffer
    (insert "see file:///tmp/notes.txt here")
    (agent-shell-markdown-replace-markup :render-images t)
    (should (equal "file:///tmp/notes.txt"
                   (agent-shell-markdown-link-url-at-point 5)))))

(ert-deftest agent-shell-markdown-linkify-bare-url-leaves-rendered-links ()
  ;; A link whose title is itself a URL keeps pointing at its own target,
  ;; rather than being re-linkified to the text it displays.
  (with-temp-buffer
    (insert "[https://label.com](https://target.com)")
    (agent-shell-markdown-replace-markup)
    (should (equal "https://label.com"
                   (buffer-substring-no-properties (point-min) (point-max))))
    (should (equal "https://target.com"
                   (agent-shell-markdown-link-url-at-point (point-min))))))

(ert-deftest agent-shell-markdown-remote-image-fallback-is-a-link ()
  ;; A remote image that can't be shown inline renders as a link opening
  ;; it, so it carries its target like any rendered link: recoverable for
  ;; copy/export, and a stop for item navigation rather than a spot only
  ;; RET knows about.
  (with-temp-buffer
    (insert "see ![remote](https://example.com/x.png) here")
    (agent-shell-markdown-replace-markup :render-images t)
    (should (equal "see remote here"
                   (buffer-substring-no-properties (point-min) (point-max))))
    (should (equal "https://example.com/x.png"
                   (agent-shell-markdown-link-url-at-point 5)))
    (goto-char (point-min))
    (should (= 5 (agent-shell-markdown--next-visible-link)))))

(ert-deftest agent-shell-markdown-next-visible-link ()
  ;; Walks rendered links (those stamped with their URL), skipping the one
  ;; point is on, in both directions.
  (with-temp-buffer
    (insert "see docs and more here now")
    (put-text-property 5 9 'agent-shell-markdown-url "https://a.example")
    (put-text-property 18 22 'agent-shell-markdown-url "https://b.example")
    (goto-char (point-min))
    (should (= 5 (agent-shell-markdown--next-visible-link)))
    (goto-char 5)
    (should (= 18 (agent-shell-markdown--next-visible-link)))
    (goto-char 18)
    (should (null (agent-shell-markdown--next-visible-link)))
    (goto-char (point-max))
    (should (= 18 (agent-shell-markdown--previous-visible-link)))
    (goto-char 18)
    (should (= 5 (agent-shell-markdown--previous-visible-link)))))

(ert-deftest agent-shell-markdown-next-visible-link-skips-hidden ()
  ;; A link hidden inside collapsed text isn't a navigation stop either.
  (with-temp-buffer
    (insert "see docs and more here now")
    (put-text-property 5 9 'agent-shell-markdown-url "https://a.example")
    (put-text-property 5 9 'invisible t)
    (put-text-property 18 22 'agent-shell-markdown-url "https://b.example")
    (goto-char (point-min))
    (should (= 18 (agent-shell-markdown--next-visible-link)))
    (remove-list-of-text-properties 5 9 '(invisible))
    (goto-char (point-min))
    (should (= 5 (agent-shell-markdown--next-visible-link)))))

(ert-deftest agent-shell-markdown-visible-link-and-image-are-separate-stops ()
  ;; Rendered text carrying both (an image whose alt text is a link) is one
  ;; stop for each search, and plain rendered links aren't image stops.
  (with-temp-buffer
    (insert "ab")
    (put-text-property 1 2 'display '(image :file "a.png"))
    (put-text-property 2 3 'agent-shell-markdown-url "https://a.example")
    (goto-char (point-min))
    (should (null (agent-shell-markdown--next-visible-image)))
    (should (= 2 (agent-shell-markdown--next-visible-link)))))

(ert-deftest agent-shell-markdown-next-visible-source-block ()
  ;; Walks rendered source blocks, landing on the `⧉' their label ends
  ;; with (where RET copies the body) rather than on the block's text.
  (with-temp-buffer
    (insert "Intro\n\n```python\nprint(1)\n```\n\nMiddle\n\n```\nplain\n```\n\nEnd\n")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-min))
    (let ((first (agent-shell-markdown--next-visible-source-block)))
      (should first)
      (should (equal "⧉" (buffer-substring-no-properties first (1+ first))))
      (goto-char first)
      (let ((second (agent-shell-markdown--next-visible-source-block)))
        (should second)
        (should (equal "⧉" (buffer-substring-no-properties second (1+ second))))
        (goto-char second)
        (should (null (agent-shell-markdown--next-visible-source-block)))
        ;; And back again.
        (should (= first (agent-shell-markdown--previous-visible-source-block)))))))

(ert-deftest agent-shell-markdown-next-visible-source-block-skips-hidden ()
  ;; A block hidden inside collapsed text isn't a navigation stop.
  (with-temp-buffer
    (insert "Intro\n\n```python\nprint(1)\n```\n\nEnd\n")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-min))
    (let ((glyph (agent-shell-markdown--next-visible-source-block)))
      (put-text-property glyph (1+ glyph) 'invisible t)
      (goto-char (point-min))
      (should (null (agent-shell-markdown--next-visible-source-block)))
      (remove-list-of-text-properties glyph (1+ glyph) '(invisible))
      (goto-char (point-min))
      (should (= glyph (agent-shell-markdown--next-visible-source-block))))))

(ert-deftest agent-shell-markdown-visible-image-search-without-images ()
  ;; Plain text is no stop in either direction.
  (with-temp-buffer
    (insert "no images here")
    (goto-char (point-min))
    (should (null (agent-shell-markdown--next-visible-image)))
    (goto-char (point-max))
    (should (null (agent-shell-markdown--previous-visible-image)))))

(ert-deftest agent-shell-markdown-image-scale-steps ()
  ;; A ratio steps by 0.1 and a pixel width by 50, each clamped at its
  ;; bounds.  Re-sizing is a no-op here: the buffer is in no window.
  (with-temp-buffer
    (setq-local agent-shell-markdown-image-max-width 0.4)
    (agent-shell-markdown-image-scale-increase)
    (should (= 0.5 agent-shell-markdown-image-max-width))
    (agent-shell-markdown-image-scale-decrease)
    (agent-shell-markdown-image-scale-decrease)
    (should (= 0.3 agent-shell-markdown-image-max-width))
    ;; Repeated steps stay on 0.1 boundaries (no float drift) and stop at 1.0.
    (dotimes (_ 10)
      (agent-shell-markdown-image-scale-increase))
    (should (= 1.0 agent-shell-markdown-image-max-width))
    ;; ... and at 0.1 going down.
    (dotimes (_ 20)
      (agent-shell-markdown-image-scale-decrease))
    (should (= 0.1 agent-shell-markdown-image-max-width)))
  (with-temp-buffer
    (setq-local agent-shell-markdown-image-max-width 300)
    (agent-shell-markdown-image-scale-increase)
    (should (= 350 agent-shell-markdown-image-max-width))
    (agent-shell-markdown-image-scale-decrease)
    (should (= 300 agent-shell-markdown-image-max-width))
    (dotimes (_ 10)
      (agent-shell-markdown-image-scale-decrease))
    (should (= 50 agent-shell-markdown-image-max-width))))

(ert-deftest agent-shell-markdown-image-scale-steps-buffer-locally ()
  ;; Scaling one buffer leaves the setting (and every other buffer's
  ;; images) as configured, rather than moving a global width.
  (let ((configured (default-value 'agent-shell-markdown-image-max-width)))
    (with-temp-buffer
      (agent-shell-markdown-image-scale-increase)
      (should (local-variable-p 'agent-shell-markdown-image-max-width))
      (should (equal configured
                     (default-value 'agent-shell-markdown-image-max-width))))
    ;; A second buffer starts from the configured width, not the first
    ;; buffer's step.
    (with-temp-buffer
      (should-not (local-variable-p 'agent-shell-markdown-image-max-width))
      (should (equal configured agent-shell-markdown-image-max-width)))))

(ert-deftest agent-shell-markdown-image-scale-at-point-steps-ratio ()
  ;; With point on a window-tracking image, only that image steps: its
  ;; fraction grows by 0.1 and stays tracked (so it keeps following the
  ;; window), while the setting other images follow is left alone.
  (with-temp-buffer
    (insert "AB")
    (put-text-property 1 2 'display '(image :file "a.png" :max-width 500))
    (put-text-property 1 2 'agent-shell-markdown-image-width-ratio 0.5)
    (put-text-property 2 3 'display '(image :file "b.png" :max-width 400))
    (put-text-property 2 3 'agent-shell-markdown-image-width-ratio 'default)
    (setq-local agent-shell-markdown-image-max-width 0.4)
    (cl-letf (((symbol-function 'create-image)
               (lambda (file &optional _type _data-p &rest props)
                 (append (list 'image :file file) props)))
              ((symbol-function 'image-flush) (lambda (&rest _) nil))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&rest _) 1000)))
      (goto-char 1)
      (agent-shell-markdown-image-scale-increase))
    (should (= 0.6 (get-text-property 1 'agent-shell-markdown-image-width-ratio)))
    (should (= 600 (image-property (get-text-property 1 'display) :max-width)))
    ;; The image at point aside, nothing else moved.
    (should (= 0.4 agent-shell-markdown-image-max-width))
    (should (= 400 (image-property (get-text-property 2 'display) :max-width)))))

(ert-deftest agent-shell-markdown-image-scale-at-point-steps-pixels ()
  ;; An image the markdown sizes in pixels (no tracked ratio) steps by 50
  ;; when point is on it, as it does under a whole-buffer step.
  (with-temp-buffer
    (insert "A")
    (put-text-property 1 2 'display '(image :file "a.png" :max-width 300))
    (cl-letf (((symbol-function 'create-image)
               (lambda (file &optional _type _data-p &rest props)
                 (append (list 'image :file file) props)))
              ((symbol-function 'image-flush) (lambda (&rest _) nil))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&rest _) 1000)))
      (goto-char 1)
      (agent-shell-markdown-image-scale-increase)
      (should (= 350 (image-property (get-text-property 1 'display) :max-width)))
      (agent-shell-markdown-image-scale-decrease)
      (agent-shell-markdown-image-scale-decrease)
      (should (= 250 (image-property (get-text-property 1 'display) :max-width)))
      ;; Still untracked, so a window resize won't pull it back to the
      ;; default width.
      (should (null (get-text-property
                     1 'agent-shell-markdown-image-width-ratio))))))

(ert-deftest agent-shell-markdown-image-scale-at-point-untracks-default ()
  ;; Stepping a `default'-sized image detaches it from
  ;; `agent-shell-markdown-image-max-width'.  With a pixel default there's
  ;; no fraction to track, so it's dropped from tracking altogether and
  ;; keeps its stepped width across a resize.
  (with-temp-buffer
    (insert "A")
    (put-text-property 1 2 'display '(image :file "a.png" :max-width 300))
    (put-text-property 1 2 'agent-shell-markdown-image-width-ratio 'default)
    (put-text-property 1 2 'agent-shell-markdown-image-window-width 1000)
    (setq-local agent-shell-markdown-image-max-width 300)
    (cl-letf (((symbol-function 'create-image)
               (lambda (file &optional _type _data-p &rest props)
                 (append (list 'image :file file) props)))
              ((symbol-function 'image-flush) (lambda (&rest _) nil))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&rest _) 2000)))
      (goto-char 1)
      (agent-shell-markdown-image-scale-increase)
      (should (= 350 (image-property (get-text-property 1 'display) :max-width)))
      (should (null (get-text-property
                     1 'agent-shell-markdown-image-width-ratio)))
      (agent-shell-markdown-rerender-images :force t)
      (should (= 350 (image-property (get-text-property 1 'display) :max-width))))))

(ert-deftest agent-shell-markdown-image-scale-at-point-follows-preceding-image ()
  ;; A cursor resting just past an image still counts as being on it,
  ;; where a cursor on plain text falls back to whole-buffer scaling.
  (with-temp-buffer
    (insert "Ax")
    (put-text-property 1 2 'display '(image :file "a.png" :max-width 300))
    (setq-local agent-shell-markdown-image-max-width 0.4)
    (cl-letf (((symbol-function 'create-image)
               (lambda (file &optional _type _data-p &rest props)
                 (append (list 'image :file file) props)))
              ((symbol-function 'image-flush) (lambda (&rest _) nil))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&rest _) 1000)))
      (goto-char 2)
      (agent-shell-markdown-image-scale-increase)
      (should (= 350 (image-property (get-text-property 1 'display) :max-width)))
      (should (= 0.4 agent-shell-markdown-image-max-width))
      ;; On plain text: the buffer's width steps, and the image steps with
      ;; it on its own pixel size rather than being skipped.
      (goto-char 3)
      (agent-shell-markdown-image-scale-increase)
      (should (= 0.5 agent-shell-markdown-image-max-width))
      (should (= 400 (image-property (get-text-property 1 'display) :max-width))))))

(ert-deftest agent-shell-markdown-image-scale-off-image-steps-every-image ()
  ;; A buffer-wide step moves all three kinds: one following the setting,
  ;; one the markdown sized as a percentage, one it sized in pixels.
  (with-temp-buffer
    (insert "ABC.")
    (put-text-property 1 2 'display '(image :file "a.png" :max-width 400))
    (put-text-property 1 2 'agent-shell-markdown-image-width-ratio 'default)
    (put-text-property 2 3 'display '(image :file "b.png" :max-width 500))
    (put-text-property 2 3 'agent-shell-markdown-image-width-ratio 0.5)
    (put-text-property 3 4 'display '(image :file "c.png" :max-width 300))
    (setq-local agent-shell-markdown-image-max-width 0.4)
    (cl-letf (((symbol-function 'create-image)
               (lambda (file &optional _type _data-p &rest props)
                 (append (list 'image :file file) props)))
              ((symbol-function 'image-flush) (lambda (&rest _) nil))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&rest _) 1000)))
      (goto-char (point-max))
      (should-not (agent-shell-markdown--image-position-at-point))
      (agent-shell-markdown-image-scale-increase)
      ;; Following the buffer's width, now 0.5.
      (should (= 0.5 agent-shell-markdown-image-max-width))
      (should (= 500 (image-property (get-text-property 1 'display) :max-width)))
      ;; Its own fraction stepped to 0.6, still tracking the window.
      (should (= 0.6 (get-text-property 2 'agent-shell-markdown-image-width-ratio)))
      (should (= 600 (image-property (get-text-property 2 'display) :max-width)))
      ;; Its own pixel width stepped by 50.
      (should (= 350 (image-property (get-text-property 3 'display) :max-width)))
      (should (null (get-text-property
                     3 'agent-shell-markdown-image-width-ratio))))))

(ert-deftest agent-shell-markdown-image-scale-reset-restores-markup-width ()
  ;; Reset re-sizes the image at point to what its markdown asks for: a
  ;; pixel width stays untracked at that width, a percentage goes back to
  ;; tracking its own fraction.
  (with-temp-buffer
    (insert "AB")
    (put-text-property 1 2 'display '(image :file "a.png" :max-width 500))
    (put-text-property 1 2 'agent-shell-markdown-source "![a](a.png){width=300}")
    (put-text-property 2 3 'display '(image :file "b.png" :max-width 500))
    (put-text-property 2 3 'agent-shell-markdown-image-width-ratio 0.9)
    (put-text-property 2 3 'agent-shell-markdown-source "![b](b.png){width=50%}")
    (cl-letf (((symbol-function 'create-image)
               (lambda (file &optional _type _data-p &rest props)
                 (append (list 'image :file file) props)))
              ((symbol-function 'image-flush) (lambda (&rest _) nil))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&rest _) 1000)))
      (goto-char 1)
      (agent-shell-markdown-image-scale-reset)
      (should (= 300 (image-property (get-text-property 1 'display) :max-width)))
      (should (null (get-text-property 1 'agent-shell-markdown-image-width-ratio)))
      (goto-char 2)
      (agent-shell-markdown-image-scale-reset)
      (should (= 500 (image-property (get-text-property 2 'display) :max-width)))
      (should (= 0.5 (get-text-property
                      2 'agent-shell-markdown-image-width-ratio))))))

(ert-deftest agent-shell-markdown-image-scale-reset-restores-default-tracking ()
  ;; An image whose markdown asks for no width goes back to following
  ;; `agent-shell-markdown-image-max-width', so later whole-buffer
  ;; scaling takes it along again.
  (with-temp-buffer
    (insert "A")
    (put-text-property 1 2 'display '(image :file "a.png" :max-width 250))
    (put-text-property 1 2 'agent-shell-markdown-source "![a](a.png)")
    (setq-local agent-shell-markdown-image-max-width 0.4)
    (cl-letf (((symbol-function 'create-image)
               (lambda (file &optional _type _data-p &rest props)
                 (append (list 'image :file file) props)))
              ((symbol-function 'image-flush) (lambda (&rest _) nil))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&rest _) 1000)))
      (goto-char 1)
      (agent-shell-markdown-image-scale-reset)
      (should (eq 'default (get-text-property
                            1 'agent-shell-markdown-image-width-ratio)))
      (should (= 400 (image-property (get-text-property 1 'display) :max-width)))
      ;; Following the setting again: scaling from off the image moves it.
      (goto-char (point-max))
      (agent-shell-markdown-image-scale-increase)
      (should (= 500 (image-property
                      (get-text-property 1 'display) :max-width))))))

(ert-deftest agent-shell-markdown-image-scale-reset-without-image-errors ()
  ;; A buffer with no image and no scaling of its own has nothing to
  ;; reset, and says so rather than silently doing nothing.
  (with-temp-buffer
    (insert "no image here")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&rest _) 1000)))
      (should-error (agent-shell-markdown-image-scale-reset) :type 'user-error))))

(ert-deftest agent-shell-markdown-image-scale-reset-off-image-resets-buffer ()
  ;; Off an image, reset restores every image in the buffer and drops the
  ;; buffer's own width, so images following it return to the configured
  ;; one.
  (with-temp-buffer
    (insert "AB.")
    ;; Stepped at point earlier: untracked at 500, markdown asking for 300.
    (put-text-property 1 2 'display '(image :file "a.png" :max-width 500))
    (put-text-property 1 2 'agent-shell-markdown-source "![a](a.png){width=300}")
    ;; Following the buffer's stepped width (0.8 of 1000).
    (put-text-property 2 3 'display '(image :file "b.png" :max-width 800))
    (put-text-property 2 3 'agent-shell-markdown-image-width-ratio 'default)
    (put-text-property 2 3 'agent-shell-markdown-source "![b](b.png)")
    (setq-local agent-shell-markdown-image-max-width 0.8)
    (cl-letf (((symbol-function 'create-image)
               (lambda (file &optional _type _data-p &rest props)
                 (append (list 'image :file file) props)))
              ((symbol-function 'image-flush) (lambda (&rest _) nil))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&rest _) 1000)))
      ;; Clear of both images: the char before point isn't one either.
      (goto-char (point-max))
      (should-not (agent-shell-markdown--image-position-at-point))
      (agent-shell-markdown-image-scale-reset)
      ;; Back to the width its markdown asks for, untracked.
      (should (= 300 (image-property (get-text-property 1 'display) :max-width)))
      (should (null (get-text-property 1 'agent-shell-markdown-image-width-ratio)))
      ;; Back to the configured width, still tracking it.
      (should-not (local-variable-p 'agent-shell-markdown-image-max-width))
      (should (eq 'default (get-text-property
                            2 'agent-shell-markdown-image-width-ratio)))
      (should (= (agent-shell-markdown--image-max-width)
                 (image-property (get-text-property 2 'display) :max-width))))))

(ert-deftest agent-shell-markdown-image-scale-reset-drops-scaling-without-images ()
  ;; A buffer scaled before its images arrived (or after they were
  ;; cleared) still has its own width dropped.
  (with-temp-buffer
    (insert "no images yet")
    (setq-local agent-shell-markdown-image-max-width 0.8)
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&rest _) 1000)))
      (agent-shell-markdown-image-scale-reset)
      (should-not (local-variable-p 'agent-shell-markdown-image-max-width)))))

(ert-deftest agent-shell-markdown-image-source-attributes ()
  ;; Reads the size block off the stashed markup; markup without one (or
  ;; no markup at all) constrains nothing.
  (with-temp-buffer
    (insert "ABC")
    (put-text-property 1 2 'agent-shell-markdown-source "![a](a.png){width=300}")
    (put-text-property 2 3 'agent-shell-markdown-source "![a](a.png)")
    (should (equal '((:max-width . 300))
                   (agent-shell-markdown--image-source-attributes 1)))
    (should (null (agent-shell-markdown--image-source-attributes 2)))
    (should (null (agent-shell-markdown--image-source-attributes 3)))))

(ert-deftest agent-shell-markdown-complete-renders-trailing-image ()
  ;; An image ending the text is held back while streaming, in case a
  ;; `{width=...}' block is still coming, and renders once the render is
  ;; marked complete.
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image) (lambda (&rest _) '(image :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil)))
          (with-temp-buffer
            (insert (format "plot\n\n![alt](%s)" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (should (string-suffix-p (format "![alt](%s)" image-file)
                                     (buffer-substring-no-properties
                                      (point-min) (point-max))))
            (agent-shell-markdown-replace-markup :render-images t :complete t)
            (should (equal "plot\n\nalt" (buffer-substring-no-properties
                                         (point-min) (point-max))))
            (should (eq 'image (car-safe (get-text-property (- (point-max) 1)
                                                            'display))))))
      (delete-file image-file))))

(ert-deftest agent-shell-markdown-watermark-survives-shift ()
  ;; The frontier is stored relative to the text carrying it, so it still
  ;; means the same place after that text moves.  Stored as a position it
  ;; would drift by whatever was added or removed above, and a later scan
  ;; could start mid-markup, skipping it.
  (with-temp-buffer
    (insert "ABOVE\n")
    (let ((body-start (point)))
      (insert "hello **world**\nlast line here")
      (save-restriction
        (narrow-to-region body-start (point-max))
        (agent-shell-markdown-replace-markup))
      ;; Something above shrinks, moving the body up.
      (goto-char (point-min))
      (delete-char 3)
      (setq body-start (- body-start 3))
      (save-restriction
        (narrow-to-region body-start (point-max))
        (should (= (agent-shell-markdown--watermark-start)
                   (save-excursion
                     (goto-char (point-max))
                     (line-beginning-position))))))))

(ert-deftest agent-shell-markdown-complete-keeps-attributes-together ()
  ;; Completing doesn't break the streaming guard's purpose: an image
  ;; whose `{width=...}' block did arrive still renders with it applied,
  ;; rather than stranding the attributes as literal text.
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image)
                   (lambda (_file &optional _type _data-p &rest props)
                     (cons 'image props)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil)))
          (with-temp-buffer
            (insert (format "![alt](%s){width=300}" image-file))
            (agent-shell-markdown-replace-markup :render-images t :complete t)
            (should (equal "alt" (buffer-substring-no-properties
                                  (point-min) (point-max))))
            (should (= 300 (image-property (get-text-property (point-min) 'display)
                                           :max-width)))))
      (delete-file image-file))))

(ert-deftest agent-shell-markdown-image-attributes-round-trip ()
  ;; A trailing `{width=...}' block is consumed with the image (no leaked
  ;; braces) and folded into the stashed source, so copy-as-markdown
  ;; restores the full markup.
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image)
                   (lambda (&rest _) '(image :type svg :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil)))
          (with-temp-buffer
            (insert (format "see ![logo](%s){width=300} end" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (should (equal (substring-no-properties (buffer-string))
                           "see logo end"))
            (should (equal (agent-shell-markdown-reconstruct (point-min) (point-max))
                           (format "see ![logo](%s){width=300} end" image-file)))))
      (delete-file image-file))))

(ert-deftest agent-shell-markdown-image-attributes-not-orphaned-when-streamed ()
  ;; Regression: when the `{width=...}' block streams in AFTER the
  ;; `![alt](url)', the image must not render before the attributes
  ;; arrive and leave them as literal text.  The image is deferred while
  ;; it could still gain a trailing block, then renders and consumes it.
  (with-temp-buffer
    (insert "x ![a](https://x.com/i.png)")
    (agent-shell-markdown-replace-markup :render-images t)
    ;; At end of buffer a `{...}' may still arrive, so nothing rendered.
    (should (equal (substring-no-properties (buffer-string))
                   "x ![a](https://x.com/i.png)"))
    (goto-char (point-max))
    (insert "{width=100%} y")
    (agent-shell-markdown-replace-markup :render-images t)
    ;; Now the block is complete: render and consume it (no leaked braces).
    (should (equal (substring-no-properties (buffer-string)) "x a y"))))

(ert-deftest agent-shell-markdown-image-attributes-pending-p ()
  ;; End of buffer, or an unclosed `{', means a trailing attribute block
  ;; may still stream in; a complete `{...}' or a non-brace char does not.
  (with-temp-buffer
    (insert "![a](u)")
    (should (agent-shell-markdown--image-attributes-pending-p (point-max))))
  (with-temp-buffer
    (insert "![a](u){wid")
    (should (agent-shell-markdown--image-attributes-pending-p 8)))
  (with-temp-buffer
    (insert "![a](u){w=1%} x")
    (should-not (agent-shell-markdown--image-attributes-pending-p 8)))
  (with-temp-buffer
    (insert "![a](u) x")
    (should-not (agent-shell-markdown--image-attributes-pending-p 8))))

(ert-deftest agent-shell-markdown-image-attributes-sized-create-image ()
  ;; `width='/`height=' become per-image `:max-width' / `:max-height' px
  ;; passed to `create-image'.
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg"))
        (props nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image)
                   (lambda (&rest args) (setq props (nthcdr 3 args)) '(image :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil)))
          (with-temp-buffer
            (insert (format "![logo](%s){width=300 height=200}" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (should (equal (plist-get props :max-width) 300))
            (should (equal (plist-get props :max-height) 200))))
      (delete-file image-file))))

(ert-deftest agent-shell-markdown-convert-link-angle-brackets ()
  ;; CommonMark angle-bracketed destination `[t](<url>)' renders like the
  ;; bare form, with both the brackets and the angle brackets stripped.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "see [docs](<https://example.com>) please"))
                 '(("see " nil)
                   ("docs" (agent-shell-markdown-link))
                   (" please" nil)))))

(ert-deftest agent-shell-markdown-convert-image-angle-brackets-remote-falls-back ()
  ;; A remote image whose destination is angle-bracketed still resolves to
  ;; the http url (brackets stripped), so the non-inline fallback is a link.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert "![docs](<https://example.com/a.png>)"))
                 '(("docs" (agent-shell-markdown-link))))))

(ert-deftest agent-shell-markdown--link-markup-url-angle-allows-spaces ()
  ;; The whole point of the angle-bracket form: the url may contain spaces
  ;; (and parentheses), which the bare `(...)' form cannot represent.
  (with-temp-buffer
    (insert "[x](</path/with spaces (1).png>)")
    (goto-char (point-min))
    (should (re-search-forward (agent-shell-markdown--link-markup-regexp) nil t))
    (should (equal (agent-shell-markdown--link-markup-url)
                   "/path/with spaces (1).png"))))

(ert-deftest agent-shell-markdown--link-markup-url-bare-form ()
  ;; The bare destination is still captured (from group 3) unchanged.
  (with-temp-buffer
    (insert "[x](https://example.com)")
    (goto-char (point-min))
    (should (re-search-forward (agent-shell-markdown--link-markup-regexp) nil t))
    (should (equal (agent-shell-markdown--link-markup-url) "https://example.com"))))

(ert-deftest agent-shell-markdown--link-markup-regexp-image-empty-alt ()
  ;; Image alt may be empty; the angle-bracketed url is still captured.
  (with-temp-buffer
    (insert "![](<a b.png>)")
    (goto-char (point-min))
    (should (re-search-forward (agent-shell-markdown--link-markup-regexp :as-image? t) nil t))
    (should (equal (match-string 1) ""))
    (should (equal (agent-shell-markdown--link-markup-url) "a b.png"))))

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
                 ;; Panel framed by a blank line on each side (see
                 ;; `agent-shell-markdown--pad-rendered-blocks').
                 '(("before " nil)
                   ("a" (agent-shell-markdown-link))
                   ("

" nil)
                   ("
" (agent-shell-markdown-source-block))
                   ("snippet ⧉" (agent-shell-markdown-source-block-language))
                   ("

[b](v)

" (agent-shell-markdown-source-block))
                   ("
after " nil)
                   ("c" (agent-shell-markdown-link))))))

(ert-deftest agent-shell-markdown-convert-source-block-no-language ()
  ;; Plain fenced block (no language): fences deleted, a "snippet ⧉"
  ;; header is inserted directly above the body as real buffer text
  ;; (no display property), bracketed by tinted vpad newlines so the
  ;; panel reads as a contiguous block.  Body chars carry the
  ;; `agent-shell-markdown-frozen' tag (not surfaced by
  ;; `--deconstruct').
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "```
body
```"))
                 '(("
" (agent-shell-markdown-source-block))
                   ("snippet ⧉" (agent-shell-markdown-source-block-language))
                   ("

body

" (agent-shell-markdown-source-block))))))

(ert-deftest agent-shell-markdown-convert-source-block-language-label ()
  ;; Every fence renders with an actionable label inserted as real
  ;; buffer text directly above the body — "LANG ⧉" when a language
  ;; is declared, "snippet ⧉" otherwise.  No display property, no
  ;; overlays.  The label sits between tinted vpad newlines that
  ;; make the surrounding panel read as a contiguous block.  RET or
  ;; mouse-1 anywhere on the label kills the body to the kill ring.
  (let* ((with-lang (agent-shell-markdown-convert "```python
print(\"hi\")
```
"))
         (no-lang (agent-shell-markdown-convert "```
body
```
")))
    (should (string-prefix-p "\npython ⧉\n\nprint("
                             (substring-no-properties with-lang)))
    (should (string-prefix-p "\nsnippet ⧉\n\nbody"
                             (substring-no-properties no-lang)))
    ;; Label face + actionable props on both the first name char and
    ;; the ⧉ glyph.  The leading char is the tinted vpad `\\n', so the
    ;; label starts at index 1; "python " is 7 chars, so the ⧉ glyph
    ;; sits at index 8.
    (dolist (i '(1 8))
      (should (eq (get-text-property i 'face with-lang)
                  'agent-shell-markdown-source-block-language))
      (should (eq (get-text-property i 'pointer with-lang) 'hand))
      (should (keymapp (get-text-property i 'keymap with-lang))))))

(ert-deftest agent-shell-markdown-source-block-copy-hint-follows-binding ()
  ;; The hint the label echoes on cursor entry names whichever key its
  ;; keymap binds, rather than hardcoding RET (which would start lying
  ;; the moment the map binds something else).  Mouse bindings are
  ;; skipped, so the hint always names something pressable.
  (let* ((rendered (agent-shell-markdown-convert "```
body
```
"))
         ;; Index 1 is the label's first char (index 0 is the tinted vpad).
         (map (get-text-property 1 'keymap rendered))
         (sensor (seq-first (get-text-property 1 'cursor-sensor-functions rendered)))
         (echoed nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq echoed (apply #'format format-string args)))))
      (funcall sensor nil nil 'entered)
      (should (equal "Press RET to copy" echoed))
      (define-key map (kbd "TAB") (lookup-key map (kbd "RET")))
      (define-key map (kbd "RET") nil t)
      (funcall sensor nil nil 'entered)
      (should (equal "Press TAB to copy" echoed)))))

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
    (should (equal rendered "
markdown ⧉

```python
print(\"hi\")
```

"))))

(ert-deftest agent-shell-markdown-convert-source-block-with-language ()
  ;; `emacs-lisp' source block: fences deleted, an "emacs-lisp ⧉"
  ;; header is inserted as buffer text bracketed by tinted vpad
  ;; newlines, then the body chars get the language's `font-lock'
  ;; faces layered over the `agent-shell-markdown-source-block' bg.
  ;; In batch the keyword `if' is faced; the rest of the body stays
  ;; with just the panel bg.
  (should (equal (agent-shell-markdown--deconstruct
                  (agent-shell-markdown-convert
                   "```emacs-lisp
(if t nil)
```"))
                 '(("
" (agent-shell-markdown-source-block))
                   ("emacs-lisp ⧉" (agent-shell-markdown-source-block-language))
                   ("

(" (agent-shell-markdown-source-block))
                   ("if" (font-lock-keyword-face agent-shell-markdown-source-block))
                   (" t nil)

" (agent-shell-markdown-source-block))))))

(ert-deftest agent-shell-markdown-highlight-code-delays-mode-hooks-locally ()
  "Highlighting code should not delay mode hooks in another buffer."
  (with-temp-buffer
    (let ((delay-mode-hooks nil)
          (observer (current-buffer))
          highlighted-mode-hook-ran
          observer-hook-ran)
      (add-hook 'agent-shell-markdown-tests-observer-hook
                (lambda () (setq observer-hook-ran t)) nil t)
      (cl-letf (((symbol-function 'agent-shell-markdown-tests-mode)
                 (lambda ()
                   (kill-all-local-variables)
                   (add-hook 'agent-shell-markdown-tests-mode-hook
                             (lambda ()
                               (setq highlighted-mode-hook-ran t))
                             nil t)
                   (with-current-buffer observer
                     (run-mode-hooks
                      'agent-shell-markdown-tests-observer-hook))
                   (run-mode-hooks
                    'agent-shell-markdown-tests-mode-hook))))
        (agent-shell-markdown--highlight-code
         "" "agent-shell-markdown-tests"))
      (should-not highlighted-mode-hook-ran)
      (should observer-hook-ran))))

(ert-deftest agent-shell-markdown-convert-source-block-body-tagged ()
  ;; Body chars carry `agent-shell-markdown-frozen t' so subsequent calls
  ;; treat them as an avoid-range (streaming-safe).  Rendered output
  ;; is `\\n<label>\\n\\n<body>\\n\\n' — the label and body chars are
  ;; tagged; the bracketing vpad `\\n's are not (they're styling, not
  ;; protected content).
  (let ((s (agent-shell-markdown-convert "```
**not bold**
```")))
    ;; Leading vpad `\\n' is not frozen.
    (should (null (get-text-property 0 'agent-shell-markdown-frozen s)))
    ;; Label start char "s" of "snippet" is frozen.
    (should (eq t (get-text-property 1 'agent-shell-markdown-frozen s)))
    ;; A body char ("n" in "**not bold**") is frozen.
    (should (eq t (get-text-property 14 'agent-shell-markdown-frozen s)))
    ;; Trailing vpad `\\n' is not frozen.
    (should (null (get-text-property (1- (length s)) 'agent-shell-markdown-frozen s)))))

(ert-deftest agent-shell-markdown-convert-inline-code-body-tagged ()
  ;; Inline code body chars are also `agent-shell-markdown-frozen t'-tagged
  ;; so a stray "**X**" inside backticks stays literal on re-runs.
  (let ((s (agent-shell-markdown-convert "a `**not bold**` b")))
    (should (eq t (get-text-property 2 'agent-shell-markdown-frozen s)))
    (should (eq t (get-text-property 13 'agent-shell-markdown-frozen s)))
    (should (null (get-text-property 0 'agent-shell-markdown-frozen s)))))

(ert-deftest agent-shell-markdown-convert-rendered-text-marked-fontified ()
  ;; Rendered chars carry `fontified t' so jit-lock never re-runs over
  ;; them.  We style via `face'/`font-lock-face' text properties, not
  ;; font-lock keywords, so a jit-lock pass applies nothing — but its
  ;; firing mid-drag disturbs mouse drag-tracking and collapses the
  ;; selection to empty, silently breaking mouse copy of rendered text.
  ;; Marking `fontified t' up front prevents that pass entirely.
  (let ((s (agent-shell-markdown-convert "hello **world**")))
    (should (eq t (get-text-property 0 'fontified s)))
    (should (eq t (get-text-property (1- (length s)) 'fontified s))))
  ;; Also holds for fenced source blocks (the reported failure case).
  (let ((s (agent-shell-markdown-convert "```\ncode\n```")))
    (should (eq t (get-text-property 1 'fontified s)))))

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
                   "
python ⧉

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
                   '(("
" (agent-shell-markdown-source-block))
                     ("snippet ⧉" (agent-shell-markdown-source-block-language))
                     ("

**not bold**

" (agent-shell-markdown-source-block))
                     ("
" nil)
                     ("real bold" (agent-shell-markdown-bold)))))))

(ert-deftest agent-shell-markdown-source-block-framed-when-flush-against-prose ()
  ;; A source block that butts directly against surrounding prose (the
  ;; agent emitted no blank line) gains one blank line on each side so it
  ;; doesn't read as cramped.  A well-formed input that already had the
  ;; blank lines renders identically (the gap is normalised, not
  ;; doubled).  See `agent-shell-markdown--pad-rendered-blocks'.
  (let ((framed "intro


snippet ⧉

x


outro"))
    (should (equal (substring-no-properties
                    (agent-shell-markdown-convert "intro\n```\nx\n```\noutro"))
                   framed))
    (should (equal (substring-no-properties
                    (agent-shell-markdown-convert "intro\n\n```\nx\n```\n\noutro"))
                   framed))))

(ert-deftest agent-shell-markdown-source-block-top-framing-gap-is-untinted ()
  ;; Regression: the top framing blank line is inserted on the block's
  ;; tinted first line, so it must not inherit that line's box chrome
  ;; (`face' / `font-lock-face', the `line-prefix' / `wrap-prefix' gutter
  ;; and the rendered tag), else it renders tinted and visually extends
  ;; the box upward instead of separating the block from the text above.
  (with-temp-buffer
    (insert "intro\n```\nx\n```\noutro")
    (agent-shell-markdown-replace-markup)
    ;; The gap is the blank line right after "intro\n", before the
    ;; block's tinted top line.
    (goto-char (point-min))
    (forward-line 1)
    (should (eq (char-after) ?\n))
    (should (null (get-text-property (point) 'face)))
    (should (null (get-text-property (point) 'font-lock-face)))
    (should (null (get-text-property (point) 'line-prefix)))
    (should (null (get-text-property (point) 'wrap-prefix)))
    (should (null (get-text-property
                   (point) 'agent-shell-markdown-source-block-rendered)))
    ;; The block's own tinted top line follows and does carry the face.
    (should (eq (get-text-property (1+ (point)) 'face)
                'agent-shell-markdown-source-block))))

(ert-deftest agent-shell-markdown-source-block-not-framed-when-alone ()
  ;; A source block that owns the whole buffer has no neighbouring prose,
  ;; so no framing blank line is added at the buffer edges.
  (should (equal (substring-no-properties
                  (agent-shell-markdown-convert "```\nx\n```"))
                 "
snippet ⧉

x

")))

(ert-deftest agent-shell-markdown-source-block-below-gap-deferred-until-successor ()
  ;; While a source block is the last content there is nothing below to
  ;; separate from, so no framing blank line is appended.  Once
  ;; non-blank text streams in after it, the gap appears; and
  ;; re-rendering does not stack a second one.
  (with-temp-buffer
    (insert "intro\n```\nx\n```\n")
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string))
                   "intro


snippet ⧉

x

"))
    (goto-char (point-max))
    (insert "outro")
    (agent-shell-markdown-replace-markup)
    (let ((framed "intro


snippet ⧉

x


outro"))
      (should (equal (substring-no-properties (buffer-string)) framed))
      ;; Idempotent: a redundant re-render leaves the framing intact.
      (agent-shell-markdown-replace-markup)
      (should (equal (substring-no-properties (buffer-string)) framed)))))

(ert-deftest agent-shell-markdown-source-block-framing-gap-reconstructs-as-blank-line ()
  ;; The inserted framing `\\n' carries `agent-shell-markdown-source'
  ;; `\"\\n\"', so `agent-shell-copy-as-markdown' recovers it as the
  ;; standard blank line separating block-level elements.  A cramped
  ;; input therefore round-trips to well-formed markdown (the blank
  ;; lines materialise), and a well-formed input round-trips unchanged.
  (let ((wellformed "a\n\n```\nx\n```\n\nb"))
    (should (equal (agent-shell-markdown-tests--roundtrip "a\n```\nx\n```\nb")
                   wellformed))
    (should (equal (agent-shell-markdown-tests--roundtrip wellformed)
                   wellformed))))

(ert-deftest agent-shell-markdown-table-framed-when-flush-against-prose ()
  ;; A rendered table butting against prose gains a blank line on each
  ;; side, the same guarantee as source blocks.
  (with-temp-buffer
    (insert "Intro\n| a | b |\n|---|---|\n| 1 | 2 |\nOutro")
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string))
                   "Intro

│ a │ b │
├───┼───┤
│ 1 │ 2 │

Outro"))))

(ert-deftest agent-shell-markdown-table-not-framed-when-alone ()
  ;; A table that owns the whole buffer has no neighbouring prose, so
  ;; no framing blank line is added at the buffer edges.
  (with-temp-buffer
    (insert "| a | b |\n|---|---|\n| 1 | 2 |")
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string))
                   "│ a │ b │
├───┼───┤
│ 1 │ 2 │"))))

(ert-deftest agent-shell-markdown-table-framing-preserves-row-folding ()
  ;; Regression: a row that streams in right after a rendered table
  ;; must still fold into it.  Framing must not drop a blank line
  ;; between the table and the new row (that would split the table).
  (with-temp-buffer
    (insert "Intro\n| a | b |\n|---|---|\n| 1 | 2 |\n")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-max))
    (insert "| 3 | 4 |\n")
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string))
                   "Intro

│ a │ b │
├───┼───┤
│ 1 │ 2 │
│ 3 │ 4 │
"))))

(ert-deftest agent-shell-markdown-table-below-gap-held-back-for-pending-row ()
  ;; While a partial row (no closing `|' yet) sits after a rendered
  ;; table, no bottom gap is inserted (it would split the table once
  ;; the row completes and folds in).  The row is left raw until it
  ;; completes, then folds into the same table.
  (with-temp-buffer
    (insert "Intro\n| a | b |\n|---|---|\n| 1 | 2 |\n")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-max))
    (insert "| 3 | 4")
    (agent-shell-markdown-replace-markup)
    ;; No blank line between the table and the pending raw row.
    (should (equal (substring-no-properties (buffer-string))
                   "Intro

│ a │ b │
├───┼───┤
│ 1 │ 2 │
| 3 | 4"))
    ;; Completing the row folds it into the one table.
    (goto-char (point-max))
    (insert " |\n")
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string))
                   "Intro

│ a │ b │
├───┼───┤
│ 1 │ 2 │
│ 3 │ 4 │
"))))

(ert-deftest agent-shell-markdown-list-unordered-renders-bullet-as-text ()
  ;; The marker is replaced with the bullet as real buffer text (so a
  ;; plain copy yields the glyph); the markdown is stashed on
  ;; `agent-shell-markdown-source' for copy-as-markdown.  The line gets a
  ;; display-only base indent and is tagged so `--pad-rendered-blocks'
  ;; frames it.
  (let ((agent-shell-markdown-list-bullets '("•")))
    (with-temp-buffer
      (insert "- Item\n")
      (agent-shell-markdown-replace-markup)
      (should (equal (buffer-substring-no-properties (point-min) (1+ (point-min)))
                     "•"))
      (should (null (get-text-property (point-min) 'display)))
      (should (equal (get-text-property (point-min) 'agent-shell-markdown-source)
                     "- Item"))
      (should (equal (get-text-property (point-min) 'line-prefix)
                     agent-shell-markdown-list-line-prefix))
      (should (get-text-property (point-min)
                                 'agent-shell-markdown-list-rendered))
      (should (get-text-property (point-min) 'agent-shell-markdown-frozen)))))

(ert-deftest agent-shell-markdown-list-bullets-cycle-by-depth ()
  ;; Two-space nesting steps one glyph per level.  Bind a multi-glyph
  ;; set so the test doesn't depend on the configured default.
  (let ((agent-shell-markdown-list-bullets '("A" "B")))
    (with-temp-buffer
      (insert "- Top\n  - Nested\n")
      (agent-shell-markdown-replace-markup)
      (goto-char (point-min))
      (should (equal (char-to-string (char-after)) "A"))
      (forward-line 1)
      (skip-chars-forward " ")
      (should (equal (char-to-string (char-after)) "B")))))

(ert-deftest agent-shell-markdown-list-task-checkboxes ()
  ;; `- [ ]' / `- [x]' swap to matching box glyphs; a checked item's
  ;; text carries `agent-shell-markdown-list-done', an unchecked one
  ;; does not.
  (with-temp-buffer
    (insert "- [ ] Todo\n- [x] Done\n")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-min))
    (should (equal (buffer-substring-no-properties
                    (point)
                    (+ (point) (length agent-shell-markdown-list-checkbox-unchecked)))
                   agent-shell-markdown-list-checkbox-unchecked))
    (save-excursion
      (search-forward "Todo")
      (should (null (get-text-property (1- (point)) 'face))))
    (forward-line 1)
    (should (equal (buffer-substring-no-properties
                    (point)
                    (+ (point) (length agent-shell-markdown-list-checkbox-checked)))
                   agent-shell-markdown-list-checkbox-checked))
    (search-forward "Done")
    (should (eq (get-text-property (1- (point)) 'face)
                'agent-shell-markdown-list-done))))

(ert-deftest agent-shell-markdown-list-ordered-keeps-number ()
  ;; Ordered items keep their number (no `display' swap) and face the
  ;; marker; no auto-renumbering, so the source round-trips.
  (with-temp-buffer
    (insert "1. First\n")
    (agent-shell-markdown-replace-markup)
    (should (null (get-text-property (point-min) 'display)))
    (should (eq (char-after (point-min)) ?1))
    (should (eq (get-text-property (point-min) 'face)
                'agent-shell-markdown-list-marker))
    (should (equal (get-text-property (point-min) 'line-prefix)
                   agent-shell-markdown-list-line-prefix))))

(ert-deftest agent-shell-markdown-list-framed-when-flush-against-prose ()
  ;; A list flush against prose gains a blank line on each side.  The
  ;; buffer text shows the rendered glyphs (base indent is display-only).
  (let ((agent-shell-markdown-list-bullets '("•")))
    (with-temp-buffer
      (insert "Intro\n- One\n- Two\nOutro")
      (agent-shell-markdown-replace-markup)
      (should (equal (substring-no-properties (buffer-string))
                     "Intro\n\n• One\n• Two\n\nOutro")))))

(ert-deftest agent-shell-markdown-list-below-gap-held-back-while-streaming ()
  ;; Regression: a new item streaming in after a rendered list must fold
  ;; into it, not be split off by a framing blank line.
  (let ((agent-shell-markdown-list-bullets '("•")))
    (with-temp-buffer
      (insert "Intro\n- One\n- Two\n")
      (agent-shell-markdown-replace-markup)
      (goto-char (point-max))
      (insert "- Three\n")
      (agent-shell-markdown-replace-markup)
      (should (equal (substring-no-properties (buffer-string))
                     "Intro\n\n• One\n• Two\n• Three\n")))))

(ert-deftest agent-shell-markdown-list-frontier-marker-not-framed ()
  ;; Regression: a bare list marker at the streaming frontier (its
  ;; trailing space not yet arrived) must not fool the framing pass into
  ;; stranding a blank line above the item it becomes, which the collapse
  ;; pass would then have to remove a render later.
  (let ((agent-shell-markdown-list-bullets '("•")))
    (with-temp-buffer
      (insert "- one\n")
      (agent-shell-markdown-replace-markup)
      (goto-char (point-max))
      (insert "-")
      (agent-shell-markdown-replace-markup)
      ;; No framing blank stranded between the item and the bare marker.
      (should (equal (substring-no-properties (buffer-string))
                     "• one\n-"))
      (goto-char (point-max))
      (insert " two\n")
      (agent-shell-markdown-replace-markup)
      (should (equal (substring-no-properties (buffer-string))
                     "• one\n• two\n")))))

(ert-deftest agent-shell-markdown-list-items-not-split-when-rendered-separately ()
  ;; Regression: streaming under a body narrow renders a last item early
  ;; (before its trailing newline arrives), so that newline lands
  ;; untagged between two rendered list lines, splitting the list into
  ;; two runs.  Framing must still treat them as one block and not
  ;; strand a blank between the items.  The body narrow here excludes a
  ;; trailing newline (as a fragment body does), which is what triggers
  ;; the early render.
  (let ((agent-shell-markdown-list-bullets '("•")))
    (with-temp-buffer
      (insert "- one\n")
      (save-restriction (narrow-to-region (point-min) (1- (point-max)))
                        (agent-shell-markdown-replace-markup))
      (goto-char (1- (point-max)))
      (insert "\n- two")
      (save-restriction (narrow-to-region (point-min) (1- (point-max)))
                        (agent-shell-markdown-replace-markup))
      (should (equal (substring-no-properties (buffer-string))
                     "• one\n• two\n")))))

(ert-deftest agent-shell-markdown-list-item-not-split-when-line-streams-in-parts ()
  ;; Regression: a chunk can end mid-item, so the last-line handler
  ;; renders that item before its whole line has streamed in; its
  ;; rendered span ends partway through the line and the rest arrives
  ;; untagged.  Framing must still fold the item's whole line into the
  ;; block and not strand a blank between it and the next item.  Uses a
  ;; body narrow (excluding a trailing newline) to trigger the early
  ;; render, and splits item one mid-line like the real stream did.
  (let ((agent-shell-markdown-list-bullets '("•")))
    (with-temp-buffer
      (insert "\n")
      (goto-char (1- (point-max)))
      (insert "- one (")
      (save-restriction (narrow-to-region (point-min) (1- (point-max)))
                        (agent-shell-markdown-replace-markup))
      (goto-char (1- (point-max)))
      (insert "rest)\n- two")
      (save-restriction (narrow-to-region (point-min) (1- (point-max)))
                        (agent-shell-markdown-replace-markup))
      (should (equal (substring-no-properties
                      (buffer-substring (point-min) (1- (point-max))))
                     "• one (rest)\n• two")))))

(ert-deftest agent-shell-markdown-list-joins-blank-separated-items ()
  ;; A list renders tight no matter how the source spaced its items:
  ;; blank lines sitting between two list items are removed.
  (let ((agent-shell-markdown-list-bullets '("•")))
    (with-temp-buffer
      (insert "- A\n\n- B\n\n- C\n")
      (agent-shell-markdown-replace-markup)
      (should (equal (substring-no-properties (buffer-string))
                     "• A\n• B\n• C\n")))))

(ert-deftest agent-shell-markdown-list-joins-blank-separated-items-when-streamed ()
  ;; Same as above, but arriving a few chars at a time, which is what
  ;; `agent-shell-markdown--whitespace-normalization-start' has to keep
  ;; working: a gap not yet collapsed looks just like a block boundary,
  ;; so a look-back stopping at the first one starts inside the list and
  ;; frames the newest item as its own block, splitting the list.  The
  ;; two-blank-line source checks that a gap of several lines still
  ;; counts as one boundary.
  (let ((agent-shell-markdown-list-bullets '("•")))
    (dolist (source '("- A\n\n- B\n\n- C\n"
                      "- A\n\n\n- B\n\n\n- C\n"
                      "intro\n\n- A\n\n- B\n\n- C\ntail\n"))
      (dolist (chunk-size '(1 3 7))
        (with-temp-buffer
          (let ((i 0))
            (while (< i (length source))
              (goto-char (point-max))
              (insert (substring source i
                                 (min (length source) (+ i chunk-size))))
              (setq i (+ i chunk-size))
              (agent-shell-markdown-replace-markup)))
          (should (equal (substring-no-properties (buffer-string))
                         (with-temp-buffer
                           (insert source)
                           (agent-shell-markdown-replace-markup)
                           (substring-no-properties (buffer-string))))))))))

(ert-deftest agent-shell-markdown-list-joins-bold-numbered-items ()
  ;; Bold-prefixed points like `**1. X**' become ordered list items once
  ;; the bold markup is stripped, so a blank line the author left between
  ;; them is joined too, and sub-bullets written tight stay tight.
  (let ((agent-shell-markdown-list-bullets '("•")))
    (with-temp-buffer
      (insert "**1. First** point.\n\n**2. Second** point:\n- a\n- b\n")
      (agent-shell-markdown-replace-markup)
      (should (equal (substring-no-properties (buffer-string))
                     "1. First point.\n2. Second point:\n• a\n• b\n")))))

(ert-deftest agent-shell-markdown-list-keeps-blank-bordering-prose ()
  ;; A blank with a list item on only one side frames the list off from
  ;; prose and stays; only inter-item gaps are joined.
  (let ((agent-shell-markdown-list-bullets '("•")))
    (with-temp-buffer
      (insert "intro\n\n- A\n\n- B\n\nafter\n")
      (agent-shell-markdown-replace-markup)
      (should (equal (substring-no-properties (buffer-string))
                     "intro\n\n• A\n• B\n\nafter\n")))))

(ert-deftest agent-shell-markdown-list-last-line-renders-under-narrow ()
  ;; Regression: a fragment body is rendered narrowed to its content, so
  ;; a list item on the last line has its terminating newline just
  ;; outside the narrow.  The newline-anchored pass would leave that last
  ;; item raw; it must still render, since a newline exists right past
  ;; the narrow (the line is complete).
  (let ((agent-shell-markdown-list-bullets '("•")))
    (with-temp-buffer
      (insert "Sources:\n\n- First item\n- Last item\n")
      (save-restriction
        ;; Narrow to the body, excluding the last item's trailing newline.
        (narrow-to-region (point-min) (1- (point-max)))
        (agent-shell-markdown-replace-markup))
      (goto-char (point-max))
      (search-backward "Last item")
      (goto-char (line-beginning-position))
      (should (eq (char-after) ?•))
      (should (get-text-property (point) 'agent-shell-markdown-list-rendered))
      ;; Source is stashed whole, so copy-as-markdown round-trips.
      (should (equal (get-text-property (point) 'agent-shell-markdown-source)
                     "- Last item")))))

(ert-deftest agent-shell-markdown-list-last-line-raw-while-streaming ()
  ;; The last line with no newline anywhere after it is a still-streaming
  ;; frontier (the marker may not be a list item yet), so it stays raw
  ;; until its line completes.
  (let ((agent-shell-markdown-list-bullets '("•")))
    (with-temp-buffer
      (insert "- First item\n- Last item")
      (agent-shell-markdown-replace-markup)
      (goto-char (point-max))
      (goto-char (line-beginning-position))
      (should (eq (char-after) ?-))
      (should (null (get-text-property
                     (point) 'agent-shell-markdown-list-rendered))))))

(ert-deftest agent-shell-markdown-list-reconstructs-to-source ()
  ;; The rendered glyphs are real buffer text, but each line stashes its
  ;; markdown on `agent-shell-markdown-source', so copy-as-markdown
  ;; recovers the original verbatim.
  (should (equal (agent-shell-markdown-tests--roundtrip
                  "- First\n  - Nested\n- [x] Done\n1. One\n")
                 "- First\n  - Nested\n- [x] Done\n1. One\n")))

(ert-deftest agent-shell-markdown-list-not-styled-in-code-block ()
  ;; A `- x' line inside a fenced block is code, not a list: its marker
  ;; is left untouched and it isn't tagged as a rendered list.
  (with-temp-buffer
    (insert "```\n- not a list\n```\n")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-min))
    (search-forward "- not a list")
    (goto-char (match-beginning 0))
    (should (eq (char-after) ?-))
    (should (null (get-text-property
                   (point) 'agent-shell-markdown-list-rendered)))))

(ert-deftest agent-shell-markdown-list-does-not-match-divider-or-emphasis ()
  ;; `---' is a divider and `*word*' is emphasis; neither is a list (the
  ;; marker requires a following space).
  (with-temp-buffer
    (insert "---\n*word*\n")
    (agent-shell-markdown-replace-markup)
    (should (null (text-property-not-all
                   (point-min) (point-max)
                   'agent-shell-markdown-list-rendered nil)))))

(ert-deftest agent-shell-markdown-list-glyph-keeps-surrounding-block ()
  ;; Regression: replacing a marker with a glyph must carry the
  ;; surrounding output's properties (e.g. `agent-shell-ui-state'), or
  ;; the glyph splits the navigatable block and item navigation stops on
  ;; every bullet.
  (let ((agent-shell-markdown-list-bullets '("•"))
        (state (list (cons :navigatable t))))
    (with-temp-buffer
      (insert (propertize "- One\n- [x] Two\n" 'agent-shell-ui-state state))
      (agent-shell-markdown-replace-markup)
      ;; No rendered char (glyphs included) drops the block state.
      (should-not (text-property-not-all (point-min) (point-max)
                                         'agent-shell-ui-state state)))))

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

(ert-deftest agent-shell-markdown-inline-code-body-not-linked ()
  ;; Regression: link markup inside inline code is literal text, not a
  ;; link.  The passes after inline-code styling avoid its body through
  ;; marker ranges into the span, which a delete-and-reinsert of the span
  ;; would collapse, exposing the body as prose.
  (with-temp-buffer
    (insert "see `[title](url)` here")
    (agent-shell-markdown-replace-markup)
    (should (equal "see [title](url) here" (buffer-substring-no-properties
                                            (point-min) (point-max))))
    (should (null (agent-shell-markdown-link-url-at-point 5)))
    (should (equal (agent-shell-markdown--deconstruct (buffer-string))
                   '(("see " nil)
                     ("[title](url)" (agent-shell-markdown-inline-code))
                     (" here" nil))))))

(ert-deftest agent-shell-markdown-inline-code-body-not-imaged ()
  ;; Same protection for image markup: inside inline code it stays
  ;; literal text rather than rendering an image.
  (let ((image-file (make-temp-file "agent-shell-test" nil ".svg")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_f) t))
                  ((symbol-function 'create-image) (lambda (&rest _) '(image :fake t)))
                  ((symbol-function 'image-flush) (lambda (&rest _) nil)))
          (with-temp-buffer
            (insert (format "see `![a](%s)` here\n" image-file))
            (agent-shell-markdown-replace-markup :render-images t)
            (should (string-match-p (regexp-quote (format "![a](%s)" image-file))
                                    (buffer-substring-no-properties
                                     (point-min) (point-max))))
            (should (null (get-text-property 5 'display)))))
      (delete-file image-file))))

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

(ert-deftest agent-shell-markdown-table-leaves-cell-link-keys-alone ()
  ;; Cells hold whatever the inline passes rendered, links included.  A
  ;; table claims no keys and echoes no hint of its own (item navigation
  ;; walks its cells like any other item), so a link sitting in a cell
  ;; keeps both: RET opens it, and entering it says so.
  (let ((rendered (agent-shell-markdown-convert "| A | [docs](https://example.com) |
|---|---|
| 1 | 2 |"))
        (echoed nil))
    (let ((link-start (string-match "docs" (substring-no-properties rendered))))
      (should link-start)
      ;; The border char before the link carries neither...
      (should-not (get-text-property 0 'keymap rendered))
      (should-not (get-text-property 0 'cursor-sensor-functions rendered))
      ;; ...while the link keeps its own, opening the URL.
      (should (get-text-property link-start 'keymap rendered))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq echoed (apply #'format format-string args)))))
        (funcall (seq-first (get-text-property
                             link-start 'cursor-sensor-functions rendered))
                 nil nil 'entered)
        (should (equal "Press RET to open in browser" echoed))))))

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
               (lambda (&optional _window) 30)))
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
               (lambda (&optional _window) 30)))
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

(ert-deftest agent-shell-markdown-convert-table-output-wraps-cjk-cell-without-spaces ()
  ;; A whitespace-free CJK cell must still wrap: CJK characters are
  ;; individually breakable, while ASCII words (here \"Cage\", \"4\",
  ;; \"33\", \"20\") stay intact.
  (let ((agent-shell-markdown-table-max-width-fraction 1.0))
    (cl-letf (((symbol-function 'agent-shell-markdown--display-width)
               (lambda (&optional _window) 36)))
      (should (equal (substring-no-properties
                      (agent-shell-markdown-convert
                       "| 作曲家 | 主な特徴 |
|---|---|
| Cage | 「4分33秒」に代表される偶然性の音楽の導入で20世紀音楽の概念を根本から問い直した |"))
                     "│ 作曲 │ 主な特徴                 │
│ 家   │                          │
├──────┼──────────────────────────┤
│ Cage │ 「4分33秒」に代表される  │
│      │ 偶然性の音楽の導入で20世 │
│      │ 紀音楽の概念を根本から問 │
│      │ い直した                 │")))))

(ert-deftest agent-shell-markdown-table-hard-breaks-long-words-to-fit ()
  ;; When the window is narrower than a column's longest unbreakable
  ;; word, the table must shrink that column below the word and
  ;; hard-break it across lines, so the whole table still fits the
  ;; window instead of overflowing and line-wrapping as a jumbled
  ;; block.  Mocks the display width to 24, narrower than the 20-column
  ;; \"agent-shell-markdown\" word plus borders and the other column.
  (let ((agent-shell-markdown-table-max-width-fraction 1.0))
    (cl-letf (((symbol-function 'agent-shell-markdown--display-width)
               (lambda (&optional _window) 24)))
      (let ((rendered (substring-no-properties
                       (agent-shell-markdown-convert
                        "| Name | Detail |
|---|---|
| x | agent-shell-markdown |"))))
        ;; Every rendered line fits within the window.
        (should (<= (apply #'max (mapcar #'string-width
                                         (split-string rendered "\n")))
                    24))
        ;; The long word was broken: no line holds it whole.
        (should-not (string-match-p "agent-shell-markdown" rendered))))))

(ert-deftest agent-shell-markdown-table-longest-word-breaks-at-cjk-chars ()
  ;; Words are unbreakable; line-breakable (category `|') characters
  ;; contribute their own char-width.  Emoji sequences are not
  ;; breakable, so a modifier stays attached to its base.
  (should (= 0 (agent-shell-markdown--table-longest-word :str nil)))
  (should (= 0 (agent-shell-markdown--table-longest-word :str "")))
  (should (= 3 (agent-shell-markdown--table-longest-word :str "foo ba")))
  (should (= 2 (agent-shell-markdown--table-longest-word :str "現代音楽")))
  (should (= 3 (agent-shell-markdown--table-longest-word :str "日本のfoo語")))
  (should (= 4 (agent-shell-markdown--table-longest-word :str "👍🏽"))))

(ert-deftest agent-shell-markdown-table-wrap-text-breaks-after-cjk-not-inside-ascii ()
  ;; Wrapping breaks after CJK characters rather than inside an
  ;; embedded ASCII word.
  (should (equal (agent-shell-markdown--table-wrap-text "日本のfoo語" 3)
                 '("日" "本" "の" "foo" "語"))))

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

(ert-deftest agent-shell-markdown-table-cell-backtick-does-not-swallow-later-cell ()
  ;; Regression: a stray unclosed backtick in one table cell must not
  ;; protect across the `|' boundary and swallow markup (e.g. a link) in
  ;; a later cell -- a code span cannot cross a cell boundary.
  (with-temp-buffer
    (insert "| A | Notes | Ref |\n|---|---|---|\n"
            "| x | Fenced ` inside | [t](https://example.com/1) |\n")
    (agent-shell-markdown-replace-markup)
    ;; The link in the Ref cell renders (its `](url)' markup is gone) and
    ;; carries the recoverable URL, rather than being left raw.
    (should-not (save-excursion (goto-char (point-min))
                                (search-forward "](https" nil t)))
    (should (save-excursion
              (goto-char (point-min))
              (text-property-search-forward 'agent-shell-markdown-url)))))

(ert-deftest agent-shell-markdown-inline-code-unclosed-backtick-protects-rest-of-line ()
  ;; Outside a table an unmatched backtick still protects the rest of the
  ;; line (a still-streaming code span), so markup after it is left raw.
  (let ((s (agent-shell-markdown-convert "a `x **bold** y")))
    (should (null (get-text-property (string-search "bold" s) 'face s)))))

(ert-deftest agent-shell-markdown-table-offscreen-measures-with-no-window ()
  ;; Regression: when the shell buffer is rendered while not displayed in
  ;; any window (common mid-stream), the table renderer must not fall
  ;; back to `selected-window' and measure cell widths in that window's
  ;; (foreign) font, which bakes wrong, per-row column widths.  With no
  ;; window showing the buffer, width measurement must receive a nil
  ;; window so it uses the deterministic `string-width' path.
  (let ((windows nil))
    (cl-letf (((symbol-function 'agent-shell-markdown--table-display-width)
               (cl-function
                (lambda (&key str window)
                  (push window windows)
                  (string-width str)))))
      (with-temp-buffer
        (insert "| A | B |\n|---|---|\n| x | y |\n")
        (agent-shell-markdown-replace-markup)
        (should windows)
        (should (seq-every-p #'null windows))))))

(ert-deftest agent-shell-markdown-rerender-tables-noop-when-undisplayed ()
  ;; With no window showing the buffer there is nothing to measure
  ;; against, so re-rendering leaves the buffer untouched (never
  ;; downgrades a table to string-width).
  (with-temp-buffer
    (insert "before\n\n| A | B |\n|---|---|\n| 1 | 2 |\n\nafter\n")
    (agent-shell-markdown-replace-markup)
    (let ((rendered (buffer-string)))
      (agent-shell-markdown-rerender-tables)
      (should (equal (buffer-string) rendered)))))

(ert-deftest agent-shell-markdown-rerender-tables-skips-matching-width ()
  ;; Re-render touches only tables whose stored
  ;; `agent-shell-markdown-table-width' differs from the window's
  ;; current width; a table already laid out for that width is skipped.
  (with-temp-buffer
    (insert "| A | B |\n|---|---|\n| 1 | 2 |\n")
    (agent-shell-markdown-replace-markup)
    (let ((calls 0))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'w))
                ((symbol-function 'window-body-width) (lambda (&rest _) 500))
                ((symbol-function 'agent-shell-markdown--render-table)
                 (lambda (_table) (setq calls (1+ calls)))))
        ;; Stored width (nil, rendered off-screen) differs from 500.
        (agent-shell-markdown-rerender-tables)
        (should (= calls 1))
        ;; Mark the table as already laid out for 500 -> skipped.
        (put-text-property (point-min) (point-max)
                           'agent-shell-markdown-table-width 500)
        (setq calls 0)
        (agent-shell-markdown-rerender-tables)
        (should (= calls 0))))))

(ert-deftest agent-shell-markdown-carry-properties-drops-font-lock-face ()
  ;; `font-lock-face' is our own styling (mirrored from `face'), not a
  ;; caller property, so it must not ride along on the delete+insert.
  ;; Carrying the table's first char (a border) would otherwise spread
  ;; the border face uniformly and grey out the re-rendered table.
  ;; Caller properties (here a block tag) still ride along.
  (with-temp-buffer
    (insert "x")
    (put-text-property (point-min) (point-max)
                       'font-lock-face 'agent-shell-markdown-table-border)
    (put-text-property (point-min) (point-max) 'agent-shell-ui-state 'blk)
    (let ((carried (agent-shell-markdown--carry-properties (point-min))))
      (should-not (plist-member carried 'font-lock-face))
      (should (eq (plist-get carried 'agent-shell-ui-state) 'blk)))))

(ert-deftest agent-shell-markdown-rerendered-table-keeps-per-cell-faces ()
  ;; Regression: re-rendering a table (e.g. on window resize) must not
  ;; smear the first char's `font-lock-face' (a border) across every
  ;; cell.  A fresh render mirrors each cell's `face' to `font-lock-face'
  ;; on its own, so header cells stay `table-header' and plain data
  ;; cells stay unstyled rather than all going border-grey.
  (with-temp-buffer
    (insert "| A | B |\n|---|---|\n| 1 | 2 |\n")
    (agent-shell-markdown-replace-markup)
    ;; Simulate the corrupted post-carry state: a uniform border
    ;; `font-lock-face' over the whole rendered table, then re-render
    ;; from the stashed source as a resize would.
    (when-let* ((region (save-excursion
                          (goto-char (point-min))
                          (text-property-search-forward
                           'agent-shell-markdown-table-source))))
      (put-text-property (prop-match-beginning region)
                         (prop-match-end region)
                         'font-lock-face 'agent-shell-markdown-table-border)
      (goto-char (prop-match-beginning region))
      (agent-shell-markdown--render-table
       (list (cons :source (prop-match-value region))
             (cons :start (prop-match-beginning region))
             (cons :end (prop-match-end region)))))
    ;; Header cell recovers its header face; a plain data cell has no
    ;; border font-lock-face smeared onto it.
    (should (eq (get-text-property
                 (save-excursion (goto-char (point-min))
                                 (1- (search-forward "A")))
                 'font-lock-face)
                'agent-shell-markdown-table-header))
    (should (null (get-text-property
                   (save-excursion (goto-char (point-min))
                                   (1- (search-forward "1")))
                   'font-lock-face)))))

(ert-deftest agent-shell-markdown-table-sizes-against-destination-window ()
  ;; Regression: column allocation must size against the table's
  ;; destination window, not whichever window happens to be selected
  ;; when the render runs (e.g. a re-layout fired from an idle timer
  ;; after a resize, with an unrelated wider window selected).  Sizing
  ;; to the selected window lays the table out too wide and it overflows
  ;; the window actually showing it.
  (let ((measured '()))
    (cl-letf (((symbol-function 'agent-shell-markdown--display-width)
               (lambda (&optional window)
                 (push window measured)
                 ;; Narrow destination window, wide selected window.
                 (if window 30 200))))
      (with-temp-buffer
        (insert "| Feature | Notes |\n|---|---|\n"
                "| Streaming parser "
                "| Handles token-by-token input with backpressure |\n")
        (let ((rendered (agent-shell-markdown--render-table-source
                         :source (buffer-string) :window 'destination-window)))
          ;; The passed window was measured; the selected window (nil) was not.
          (should (memq 'destination-window measured))
          (should-not (memq nil measured))
          ;; So the wide Notes column wrapped to fit the narrow destination
          ;; window rather than laying out at its ~69-column natural width.
          (should (< (apply #'max (mapcar #'string-width
                                          (split-string rendered "\n")))
                     45)))))))

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

(ert-deftest agent-shell-markdown-pad-table-string-floors-on-degenerate-measurement ()
  ;; The pixel path pads by comparing the cell's measured pixel width
  ;; against WIDTH (a character count) times the per-char pixel width.
  ;; When `window-text-pixel-size' returns a bogus (too-large) value --
  ;; which can happen transiently mid-stream, before redisplay settles --
  ;; `pad-px' goes <= 0.  It used to return the cell *unpadded*, so the
  ;; right border drifted and the stale layout stuck (its width stamp
  ;; matched the display, so the re-layout pass skipped it).  The floor
  ;; guarantees a cell is never emitted narrower than its column's
  ;; character width: a degenerate measurement degrades to ASCII-accurate
  ;; alignment instead of none.
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _d) t))
            ((symbol-function 'window-live-p) (lambda (_w) t))
            ((symbol-function 'agent-shell-markdown--table-char-pixel-width)
             (lambda (_window) 10))
            ;; Bogus: report the content far wider than any column budget,
            ;; forcing pad-px <= 0.
            ((symbol-function 'agent-shell-markdown--table-measure-string)
             (lambda (_str _window) 100000)))
    ;; Short cell: floored to WIDTH columns rather than returned raw.
    (let ((padded (agent-shell-markdown--pad-table-string
                   :str "hi" :width 10 :window 'fake :force-pixel t)))
      (should (equal padded "hi        "))
      (should (= (string-width padded) 10)))
    ;; A cell already at/over WIDTH is left untouched -- the floor pads,
    ;; it never truncates.
    (should (equal "abcdefghij"
                   (agent-shell-markdown--pad-table-string
                    :str "abcdefghij" :width 5 :window 'fake :force-pixel t)))))

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

(ert-deftest agent-shell-markdown-table-measure-string-leaves-destination-alone ()
  ;; Measurement used to insert the string into the destination buffer,
  ;; measure, then delete.  Anything that exited non-locally in between
  ;; stranded the probe, and since every caller catches the error and
  ;; falls back to `string-width' the failure was silent: runs of
  ;; `MMMMMMMMMM' (the face-ratio sample) and spaces (the per-char
  ;; sample) piled up at the end of the rendered text.  It now measures
  ;; in a work buffer, so the destination is untouched either way.
  (with-temp-buffer
    (insert "content")
    (set-buffer-modified-p nil)
    (cl-letf (((symbol-function 'window-buffer) (lambda (_window) (current-buffer))))
      ;; A measurement that succeeds.
      (cl-letf (((symbol-function 'buffer-text-pixel-size)
                 (lambda (&rest _) (cons 100 10))))
        (should (= 100 (agent-shell-markdown--table-measure-string
                        "MMMMMMMMMM" 'fake-window))))
      ;; And one that signals partway through.
      (cl-letf (((symbol-function 'buffer-text-pixel-size)
                 (lambda (&rest _) (error "Measurement failed"))))
        (should-error (agent-shell-markdown--table-measure-string
                       "MMMMMMMMMM" 'fake-window))))
    (should (equal "content" (buffer-string)))
    (should-not (buffer-modified-p))))

(ert-deftest agent-shell-markdown-table-measure-string-carries-face-remapping ()
  ;; `text-scale-mode' and `buffer-face-mode' work through
  ;; `face-remapping-alist', so measuring without carrying it over
  ;; reports a text-scaled buffer at its unscaled width and misaligns
  ;; every table in it.  Verified against a real window, this returned
  ;; 100 unscaled and 170 at `text-scale-mode' +3; here we just pin that
  ;; the destination's remapping reaches the measurement buffer.
  (with-temp-buffer
    (setq-local face-remapping-alist '((default . (:height 1.7))))
    (cl-letf* ((seen nil)
               ((symbol-function 'window-buffer) (lambda (_window) (current-buffer)))
               ((symbol-function 'buffer-text-pixel-size)
                (lambda (&rest _)
                  (setq seen face-remapping-alist)
                  (cons 170 10))))
      (agent-shell-markdown--table-measure-string "MMMMMMMMMM" 'fake-window)
      (should (equal '((default . (:height 1.7))) seen)))))

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
               (lambda (&optional _window) 30)))
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

(ert-deftest agent-shell-markdown-table-entry-position-resolves-from-outside-only ()
  ;; A table is entered at its first cell, so a position inside one
  ;; resolves there.  Coming from inside that same table it doesn't:
  ;; navigation walks its cells and their links one by one.
  (with-temp-buffer
    (insert "before

| A | B |
|---|---|
| 1 | see [docs](https://example.com) |
")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-min))
    (should (search-forward "A"))
    (let ((first-cell (match-beginning 0))
          (outside (point-min)))
      (should (search-forward "docs"))
      (let ((link (match-beginning 0)))
        (should (eq first-cell
                    (agent-shell-markdown-table--entry-position
                     :position link :from outside)))
        (should (eq link
                    (agent-shell-markdown-table--entry-position
                     :position link :from first-cell)))
        ;; A position outside any table is nobody's entry point.
        (should (eq outside
                    (agent-shell-markdown-table--entry-position
                     :position outside :from link)))))))

(ert-deftest agent-shell-markdown-table-first-cell-returns-to-entry-point ()
  (with-temp-buffer
    (insert "| A | B |
|---|---|
| 1 | 2 |
")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-min))
    (should (search-forward "A"))
    ;; Any position in the table resolves to its first cell.
    (let ((first-cell (match-beginning 0)))
      (should (search-forward "2"))
      (goto-char (match-beginning 0))
      (should (eq first-cell (agent-shell-markdown-table--first-cell (point)))))
    ;; Outside a table there's no cell to resolve to.
    (goto-char (point-max))
    (should-not (agent-shell-markdown-table--first-cell (point)))))

(ert-deftest agent-shell-markdown-visible-search-finds-nearest-table-cell ()
  ;; Cell starts are what item navigation searches for to find a table.
  (with-temp-buffer
    (insert "before

| A | B |
|---|---|
| 1 | 2 |

after
")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-min))
    ;; Forward from outside, the next cell is the table's first one.
    (should (eq (save-excursion
                  (should (search-forward "A"))
                  (match-beginning 0))
                (agent-shell-markdown--search-visible
                 :property 'agent-shell-markdown-table-cell-start)))
    (goto-char (point-max))
    ;; Backward from outside, it's the last one (callers resolve it to
    ;; the first with `agent-shell-markdown-table--first-cell').
    (should (eq (save-excursion
                  (goto-char (point-min))
                  (should (search-forward "2"))
                  (match-beginning 0))
                (agent-shell-markdown--search-visible
                 :property 'agent-shell-markdown-table-cell-start
                 :backwards t)))))

(ert-deftest agent-shell-markdown-visible-search-skips-hidden-table ()
  ;; A table inside collapsed text is text nothing shows, so navigation
  ;; doesn't enter it.
  (with-temp-buffer
    (insert "| A | B |
|---|---|
| 1 | 2 |
")
    (agent-shell-markdown-replace-markup)
    (put-text-property (point-min) (point-max) 'invisible t)
    (goto-char (point-min))
    (should-not (agent-shell-markdown--search-visible
                 :property 'agent-shell-markdown-table-cell-start))
    (goto-char (point-max))
    (should-not (agent-shell-markdown--search-visible
                 :property 'agent-shell-markdown-table-cell-start
                 :backwards t))))

(ert-deftest agent-shell-markdown-table-next-cell-errors-outside-table ()
  (with-temp-buffer
    (insert "not a table at all")
    (goto-char (point-min))
    (should-error (agent-shell-markdown-table-next-cell) :type 'user-error)
    (should-error (agent-shell-markdown-table-previous-cell) :type 'user-error)))

(ert-deftest agent-shell-markdown-table-link-keeps-ret-across-rerenders ()
  "A link in a cell keeps its keymap when later rows stream in.

The table re-renders from its stashed source on each new row, carrying
the properties at its start position across the delete+insert.  A
link's `keymap' can sit there, so carrying it would spread that one
link's map over every cell (see
`agent-shell-markdown--carry-properties').

Plain cell text keeps no keymap of its own, leaving the buffer's own
keys to navigate it."
  (with-temp-buffer
    (insert "| Bag | Link |\n|---|---|\n| Wool | [wool.com](https://wool.com/x) |\n")
    (agent-shell-markdown-replace-markup)
    (goto-char (point-max))
    (insert "| July | [july.com](https://july.com/y) |\n")
    (agent-shell-markdown-replace-markup)
    (dolist (needle '("wool.com" "july.com"))
      (goto-char (point-min))
      (should (search-forward needle nil t))
      (should (lookup-key (get-text-property (match-beginning 0) 'keymap)
                          (kbd "RET"))))
    (goto-char (point-min))
    (should (search-forward "Wool" nil t))
    (should-not (get-text-property (match-beginning 0) 'keymap))))

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
             ("
" (agent-shell-markdown-source-block))
             ("snippet ⧉" (agent-shell-markdown-source-block-language))
             ("

**not bold**

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

(ert-deftest agent-shell-markdown-header-keeps-properties-scoped ()
  (with-temp-buffer
    (insert (propertize "# "
                        'agent-shell-ui-section 'body
                        'invisible 'markdown-markup))
    (insert (propertize "Title"
                        'agent-shell-ui-section 'body))
    (insert (propertize "\n"
                        'agent-shell-ui-section 'body
                        'invisible t))
    (agent-shell-markdown-replace-markup)
    (should (equal (substring-no-properties (buffer-string))
                   "Title\n"))
    (dotimes (i (length "Title"))
      (let ((pos (+ (point-min) i)))
        (should (eq 'body
                    (get-text-property pos 'agent-shell-ui-section)))
        (should-not (get-text-property pos 'invisible))))
    (let ((newline-pos (+ (point-min) (length "Title"))))
      (should (eq 'body
                  (get-text-property newline-pos 'agent-shell-ui-section)))
      (should (eq t (get-text-property newline-pos 'invisible))))))

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
                         'agent-shell-markdown-watermark
                         (- (point-max) (point-min))))
    (agent-shell-markdown-replace-markup)
    (should (string-match-p "\\*\\*bold\\*\\*"
                            (substring-no-properties (buffer-string))))
    (agent-shell-markdown-replace-markup :force t)
    (should-not (string-match-p "\\*\\*"
                                (substring-no-properties (buffer-string))))))

(ert-deftest agent-shell-markdown--url-copy-file-test ()
  "Test `agent-shell-markdown--url-copy-file'.

Synchronously downloads a URL to a file, validating HTTP 200 and an optional
Content-Type prefix before writing.  `url-retrieve-synchronously' is stubbed
so the test never touches the network."
  (let* ((make-response
          (lambda (status content-type body)
            (lambda (&rest _)
              (let ((buffer (generate-new-buffer " *fake-http*")))
                (with-current-buffer buffer
                  (set-buffer-multibyte nil)
                  (insert (format "HTTP/1.1 %s\r\nContent-Type: %s\r\n\r\n"
                                  status content-type))
                  (insert body))
                buffer))))
         (dest (make-temp-file "agent-shell-url-copy")))
    (unwind-protect
        (progn
          ;; 200 + matching Content-Type prefix -> writes body, returns dest.
          (delete-file dest)
          (cl-letf (((symbol-function 'url-retrieve-synchronously)
                     (funcall make-response "200 OK" "image/png" "PNGBYTES")))
            (should (equal (agent-shell-markdown--url-copy-file
                            :url "https://example.com/a.png" :file dest
                            :content-type-prefix "image/")
                           dest))
            (should (file-exists-p dest)))

          ;; No Content-Type prefix -> any 200 response is written.
          (delete-file dest)
          (cl-letf (((symbol-function 'url-retrieve-synchronously)
                     (funcall make-response "200 OK" "text/plain" "hello")))
            (should (equal (agent-shell-markdown--url-copy-file
                            :url "https://example.com/a" :file dest)
                           dest))
            (should (file-exists-p dest)))

          ;; 200 but Content-Type prefix mismatch -> nil, nothing written.
          (delete-file dest)
          (cl-letf (((symbol-function 'url-retrieve-synchronously)
                     (funcall make-response "200 OK" "text/html" "<html>nope</html>")))
            (should-not (agent-shell-markdown--url-copy-file
                         :url "https://example.com/a" :file dest
                         :content-type-prefix "image/"))
            (should-not (file-exists-p dest)))

          ;; Non-200 -> nil, nothing written.
          (cl-letf (((symbol-function 'url-retrieve-synchronously)
                     (funcall make-response "404 Not Found" "image/png" "x")))
            (should-not (agent-shell-markdown--url-copy-file
                         :url "https://example.com/a.png" :file dest
                         :content-type-prefix "image/"))
            (should-not (file-exists-p dest)))

          ;; Connection failure (nil buffer) -> nil, nothing written.
          (cl-letf (((symbol-function 'url-retrieve-synchronously)
                     (lambda (&rest _) nil)))
            (should-not (agent-shell-markdown--url-copy-file
                         :url "https://example.com/a.png" :file dest))
            (should-not (file-exists-p dest))))
      (when (file-exists-p dest) (delete-file dest)))))

(ert-deftest agent-shell-markdown--fetch-remote-image-test ()
  "Test `agent-shell-markdown--fetch-remote-image'.

Owns the image policy (http-only, known image extension, md5-named cache);
the download itself is delegated to `agent-shell-markdown--url-copy-file',
which is stubbed here so the test exercises only the policy."
  ;; With a CACHE-DIRECTORY: cached path requested from the downloader and
  ;; returned, under that directory.
  (cl-letf (((symbol-function 'agent-shell-markdown--url-copy-file)
             (lambda (&rest args) (plist-get args :file))))
    (let ((file (agent-shell-markdown--fetch-remote-image
                 "https://example.com/a.png" "/tmp/img-cache")))
      (should (string-prefix-p "/tmp/img-cache/" file))
      (should (string-suffix-p ".png" file))
      (should (string-match-p "/[0-9a-f]+\\.png\\'" file))))

  ;; Without a CACHE-DIRECTORY: remote images are not fetched.
  (cl-letf (((symbol-function 'agent-shell-markdown--url-copy-file)
             (lambda (&rest _) (error "should not download"))))
    (should-not (agent-shell-markdown--fetch-remote-image "https://example.com/a.png" nil)))

  ;; A failed download -> nil (no silent path returned).
  (cl-letf (((symbol-function 'agent-shell-markdown--url-copy-file)
             (lambda (&rest _) nil)))
    (should-not (agent-shell-markdown--fetch-remote-image "https://example.com/b.png" "/tmp/img-cache")))

  ;; Non-http uris and extensionless urls are never downloaded.
  (cl-letf (((symbol-function 'agent-shell-markdown--url-copy-file)
             (lambda (&rest _) (error "should not download"))))
    (should-not (agent-shell-markdown--fetch-remote-image "file:///tmp/x.png" "/tmp/img-cache"))
    (should-not (agent-shell-markdown--fetch-remote-image "https://example.com/img?id=1" "/tmp/img-cache"))))

(ert-deftest agent-shell-markdown--resolve-image-url-bare-relative-test ()
  "Test `agent-shell-markdown--resolve-image-url' gates bare relative paths.

`![alt](logo.png)' resolves, since the markup says an image is meant.
The same path on its own does not, prose naming a file being far more
common than an image meant to render."
  (let* ((directory (make-temp-file "agent-shell-bare" t))
         (image (expand-file-name "logo.png" directory)))
    (unwind-protect
        (progn
          (with-temp-file image (insert "not really a png"))
          (let ((default-directory directory))
            (should (equal (file-truename image)
                           (file-truename
                            (agent-shell-markdown--resolve-image-url
                             "logo.png" :allow-bare-relative t))))
            (should-not (agent-shell-markdown--resolve-image-url "logo.png"))
            ;; Absent files stay nil either way.
            (should-not (agent-shell-markdown--resolve-image-url
                         "missing.png" :allow-bare-relative t))
            ;; A scheme is never mistaken for a path.
            (should-not (agent-shell-markdown--resolve-image-url
                         "data:image/png;base64,AAAA" :allow-bare-relative t))))
      (delete-directory directory t))))

(ert-deftest agent-shell-markdown--resolve-image-url-remote-test ()
  "Test that `agent-shell-markdown--resolve-image-url' fetches http(s) urls.

A remote url is resolved through `agent-shell-markdown--fetch-remote-image'
\(stubbed), forwarding the injected cache directory; local-path resolution is
unaffected."
  (cl-letf (((symbol-function 'agent-shell-markdown--fetch-remote-image)
             (lambda (url image-cache-directory)
               (and (string-match-p "\\`https?://" url) image-cache-directory
                    (format "%s/x.png" image-cache-directory)))))
    ;; Remote url -> fetched; the image-cache-directory argument is forwarded.
    (should (equal (agent-shell-markdown--resolve-image-url
                    "https://example.com/x.png" :image-cache-directory "/injected")
                   "/injected/x.png"))
    ;; No image-cache-directory -> remote image is not fetched (nil).
    (should-not (agent-shell-markdown--resolve-image-url "https://example.com/x.png"))
    ;; A non-existent local path still resolves to nil (no fetch attempted).
    (should-not (agent-shell-markdown--resolve-image-url "/no/such/file.png"))))

(ert-deftest agent-shell-markdown--open-externally-test ()
  "Test `agent-shell-markdown--open-externally' gates on confirmation."
  (let ((opened nil))
    (cl-letf (((symbol-function 'shell-command-do-open)
               (lambda (files) (setq opened files)))
              ((symbol-function 'browse-url-of-file)
               (lambda (file) (setq opened (list file)))))
      ;; Confirmed -> opens.
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (setq opened nil)
        (agent-shell-markdown--open-externally "/tmp/x.bin")
        (should (equal opened '("/tmp/x.bin"))))
      ;; Declined -> does nothing.
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (setq opened nil)
        (agent-shell-markdown--open-externally "/tmp/x.bin")
        (should-not opened)))))

(ert-deftest agent-shell-markdown--binary-file-p-test ()
  "Test `agent-shell-markdown--binary-file-p' NUL-byte heuristic."
  (let ((text (make-temp-file "agent-shell-text"))
        (binary (make-temp-file "agent-shell-binary")))
    (unwind-protect
        (progn
          (with-temp-file text (insert "plain text\nmore"))
          (let ((coding-system-for-write 'binary))
            (with-temp-file binary (insert "abc\0def")))
          (should-not (agent-shell-markdown--binary-file-p text))
          (should (agent-shell-markdown--binary-file-p binary)))
      (delete-file text)
      (delete-file binary))))

(ert-deftest agent-shell-markdown-visit-file-without-line-range-test ()
  "Visiting without a line range just opens the file."
  (let ((file (make-temp-file "agent-shell-tests")))
    (unwind-protect
        (save-window-excursion
          (agent-shell-markdown-visit-file :file file)
          ;; Compare truenames: `find-file' may resolve symlinks
          ;; (/var vs /private/var on macOS) depending on config.
          (should (equal (file-truename file)
                         (file-truename (buffer-file-name)))))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest agent-shell-markdown-visit-file-with-line-range-selects-region-test ()
  "Visiting with a line range selects those lines."
  (let ((file (make-temp-file "agent-shell-tests" nil nil "one\ntwo\nthree\nfour\n")))
    (unwind-protect
        (save-window-excursion
          (agent-shell-markdown-visit-file :file file :line-start 2 :line-end 3)
          (should (equal (file-truename file)
                         (file-truename (buffer-file-name))))
          (should (equal "two\nthree"
                         (buffer-substring-no-properties (point) (mark)))))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest agent-shell-markdown-visit-file-pushes-xref-marker-test ()
  "Following a link leaves a way back, as any other jump does.

The spot the link was followed from goes on xref\'s marker stack, so
`xref-go-back\' returns to it rather than leaving the reader to hunt
for where they were in the conversation."
  (let ((file (make-temp-file "agent-shell-tests" nil nil "one\ntwo\nthree\n"))
        (origin (generate-new-buffer "*agent-shell-origin*")))
    (unwind-protect
        (with-selected-window (frame-first-window)
          (save-window-excursion
            (switch-to-buffer origin)
            (insert "see the link here")
            (goto-char 5)
            (agent-shell-markdown-visit-file :file file :line-start 2)
            (should (equal (file-truename file)
                           (file-truename (or (buffer-file-name) ""))))
            (xref-go-back)
            (should (eq origin (current-buffer)))
            (should (= 5 (point)))))
      (kill-buffer origin)
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest agent-shell-markdown-visit-file-no-window-pushes-nothing-test ()
  "An opener showing nothing leaves the marker stack alone.

Pushing regardless would offer a way back to the spot the reader never
left."
  (let ((file (make-temp-file "agent-shell-tests" nil nil "one\ntwo\n"))
        (pushed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'xref-push-marker-stack)
                   (lambda (&optional _m) (setq pushed t))))
          (let ((agent-shell-markdown-open-file-function (lambda (_path) nil)))
            (agent-shell-markdown-visit-file :file file :line-start 2))
          (should-not pushed))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest agent-shell-markdown-open-file-returns-window-test ()
  "The default opener returns the window showing the file."
  (let ((file (make-temp-file "agent-shell-tests")))
    (unwind-protect
        (save-window-excursion
          (should (windowp (agent-shell-markdown-open-file file))))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest agent-shell-markdown-visit-file-rejects-non-window-test ()
  "An opener returning something other than a window is an error.

Returning the buffer is the tempting mistake, and it would otherwise
leave point somewhere the user can't see rather than fail."
  (let ((file (make-temp-file "agent-shell-tests" nil nil "one\ntwo\n")))
    (unwind-protect
        (let ((agent-shell-markdown-open-file-function
               (lambda (path) (find-file-noselect path))))
          (should-error (agent-shell-markdown-visit-file :file file :line-start 2)
                        :type 'user-error))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest agent-shell-markdown-visit-file-tolerates-no-window-test ()
  "An opener showing nothing is legal: there is simply no point to move."
  (let ((file (make-temp-file "agent-shell-tests" nil nil "one\ntwo\n")))
    (unwind-protect
        (let ((agent-shell-markdown-open-file-function (lambda (_path) nil)))
          (should-not (agent-shell-markdown-visit-file :file file :line-start 2)))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest agent-shell-markdown--open-local-link-binary-vs-text-test ()
  "Test `agent-shell-markdown--open-local-link' routes by file type.
Text/navigable files open in Emacs; binary files open externally."
  (let ((text (make-temp-file "agent-shell-ol-text" nil ".txt"))
        (binary (make-temp-file "agent-shell-ol-bin" nil ".bin"))
        (action nil))
    (unwind-protect
        (progn
          (with-temp-file text (insert "hello"))
          (let ((coding-system-for-write 'binary))
            (with-temp-file binary (insert "x\0y")))
          (cl-letf (((symbol-function 'agent-shell-markdown--open-externally)
                     (lambda (f) (setq action (cons 'external f)))))
            ;; Text file -> opened in Emacs, however the opener displays it.
            (setq action nil)
            (let ((agent-shell-markdown-open-file-function
                   (lambda (path) (setq action (cons 'in-emacs path)) nil)))
              (should (agent-shell-markdown--open-local-link (concat "file://" text))))
            (should (equal action (cons 'in-emacs text)))
            ;; Binary file -> open externally.
            (setq action nil)
            (should (agent-shell-markdown--open-local-link (concat "file://" binary)))
            (should (equal action (cons 'external binary)))
            ;; Binary file with a line number -> still external (line ignored).
            (setq action nil)
            (should (agent-shell-markdown--open-local-link (concat "file://" binary "#L10")))
            (should (equal action (cons 'external binary)))))
      (delete-file text)
      (delete-file binary))))

(ert-deftest agent-shell-markdown--parse-local-link-lines-test ()
  "Test `agent-shell-markdown--parse-local-link' reads lines and ranges.

Agents routinely emit GitHub-style ranges, so `#L10-L24' and the
plainer `#L10-24' both have to land."
  (let ((file (make-temp-file "agent-shell-lines" nil ".el" "one\ntwo\nthree\nfour\nfive\n")))
    (unwind-protect
        (progn
          (should (equal (list (cons :line-start nil) (cons :line-end nil))
                         (seq-drop (agent-shell-markdown--parse-local-link file) 1)))
          (should (equal (list (cons :line-start 2) (cons :line-end nil))
                         (seq-drop (agent-shell-markdown--parse-local-link
                                    (concat file "#L2")) 1)))
          (should (equal (list (cons :line-start 2) (cons :line-end 4))
                         (seq-drop (agent-shell-markdown--parse-local-link
                                    (concat file "#L2-L4")) 1)))
          (should (equal (list (cons :line-start 2) (cons :line-end 4))
                         (seq-drop (agent-shell-markdown--parse-local-link
                                    (concat file "#L2-4")) 1)))
          (should (equal (list (cons :line-start 3) (cons :line-end 5))
                         (seq-drop (agent-shell-markdown--parse-local-link
                                    (concat file ":3-5")) 1)))
          ;; Garbage after the digits is still rejected outright.
          (should-not (agent-shell-markdown--parse-local-link (concat file "#L2xy"))))
      (delete-file file))))

(ert-deftest agent-shell-markdown--open-local-link-selects-range-test ()
  "Test `agent-shell-markdown--open-local-link' selects a linked range.

Without a range it only moves point, leaving the mark alone."
  (let ((file (make-temp-file "agent-shell-range" nil ".el" "one\ntwo\nthree\nfour\nfive\n")))
    (unwind-protect
        (save-window-excursion
          (should (agent-shell-markdown--open-local-link (concat file "#L2-L4")))
          (should (equal 2 (line-number-at-pos (point))))
          (should (equal "two\nthree\nfour"
                         (buffer-substring-no-properties (point) (mark))))
          (deactivate-mark)
          (should (agent-shell-markdown--open-local-link (concat file "#L3")))
          (should (equal 3 (line-number-at-pos (point))))
          (should-not (region-active-p)))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(defun agent-shell-markdown-tests--source-blocks (markdown)
  "Return the `:source-blocks' descriptors a renderer sees for MARKDOWN.
Each :block marker range is replaced by the text it spans, so the
result is stable to compare against.  (The marker behaviour itself is
exercised by the editing in the -all-math-cases test.)"
  (let (blocks)
    (with-temp-buffer
      (let ((agent-shell-markdown-render-functions
             (list (lambda (context)
                     (setq blocks
                           (mapcar (lambda (b)
                                     (list (cons :language (map-elt b :language))
                                           (cons :block (buffer-substring-no-properties
                                                         (map-nested-elt b '(:block :start))
                                                         (map-nested-elt b '(:block :end))))
                                           (cons :body (map-elt b :body))
                                           (cons :complete (map-elt b :complete))))
                                   (map-elt context :source-blocks)))
                     nil))))
        (insert markdown)
        (agent-shell-markdown-replace-markup)))
    blocks))

(ert-deftest agent-shell-markdown-render-functions-receives-source-blocks ()
  ;; A render function is handed `:source-blocks' descriptors: the language,
  ;; the block's marker range (shown here as the text it spans), the body,
  ;; and completeness.
  (should (equal (agent-shell-markdown-tests--source-blocks
                  "text
```math
\\frac{a}{b}
```
")
                 '(((:language . "math")
                    (:block . "```math\n\\frac{a}{b}\n```\n")
                    (:body . "\\frac{a}{b}")
                    (:complete . t))))))

(ert-deftest agent-shell-markdown-render-functions-source-blocks-incomplete ()
  ;; A still-streaming fence is reported with `:complete' nil and no
  ;; `:body', so a renderer knows the language but not to claim it yet.
  (should (equal (agent-shell-markdown-tests--source-blocks
                  "```math
\\frac{a}{b")
                 '(((:language . "math")
                    (:block . "```math\n\\frac{a}{b")
                    (:body . nil)
                    (:complete . nil))))))

(ert-deftest agent-shell-markdown-render-functions-frozen-region-protected ()
  ;; A render function that tags its region `agent-shell-markdown-frozen'
  ;; has it treated as an avoid-range: the emphasis passes leave `_'/`*'
  ;; inside literal, while markup outside the region still renders.
  (with-temp-buffer
    (let ((agent-shell-markdown-render-functions
           (list (lambda (_context)
                   (goto-char (point-min))
                   (when (re-search-forward "\\$\\$.*?\\$\\$" nil t)
                     (put-text-property (match-beginning 0) (match-end 0)
                                        'agent-shell-markdown-frozen t))
                   nil))))
      (insert "see **bold** and $$a_b*c*$$ end")
      (agent-shell-markdown-replace-markup)
      (should (equal (agent-shell-markdown--deconstruct (buffer-string))
                     '(("see " nil)
                       ("bold" (agent-shell-markdown-bold))
                       (" and $$a_b*c*$$ end" nil)))))))

(ert-deftest agent-shell-markdown-render-functions-frozen-fenced-block-left-intact ()
  ;; A render function can claim a ```math / ```latex fence in place
  ;; (freezing the whole block without deleting its fences), and
  ;; `--style-source-blocks' honors `agent-shell-markdown-frozen' and
  ;; leaves it untouched.  Without this the source-block pass would strip
  ;; the fences and re-fontify the body as a code panel, clobbering the
  ;; renderer's overlay and forcing it to mutate the agent's original text.
  (with-temp-buffer
    (let ((agent-shell-markdown-render-functions
           (list (lambda (context)
                   (dolist (block (map-elt context :source-blocks))
                     (when (and (equal (map-elt block :language) "math")
                                (map-elt block :complete))
                       (put-text-property (map-nested-elt block '(:block :start))
                                          (map-nested-elt block '(:block :end))
                                          'agent-shell-markdown-frozen t)))
                   nil))))
      (insert "```math\n\\frac{a}{b}\n```\n")
      (agent-shell-markdown-replace-markup)
      ;; Fences and body survive verbatim: no code-panel "⧉" label was
      ;; inserted and the frozen claim still stands for later passes.
      (should (equal (substring-no-properties (buffer-string))
                     "```math\n\\frac{a}{b}\n```\n"))
      (should-not (string-match-p "⧉" (buffer-string)))
      (should (eq t (get-text-property (point-min)
                                       'agent-shell-markdown-frozen))))))

(ert-deftest agent-shell-markdown-render-functions-watermark-held-back ()
  ;; A render function returning `:watermark' holds the streaming frontier
  ;; behind its own open delimiter, even when it spans lines above the last
  ;; one (which the built-in start-of-last-line back-off wouldn't cover).
  (with-temp-buffer
    (let ((agent-shell-markdown-render-functions
           (list (lambda (_context)
                   (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward "\\$\\$" nil t)
                       (list (cons :watermark (match-beginning 0)))))))))
      (insert "intro\n$$\nx_y\nz_w")
      (let ((open-dollar (save-excursion
                           (goto-char (point-min))
                           (re-search-forward "\\$\\$")
                           (match-beginning 0))))
        (agent-shell-markdown-replace-markup)
        (should (= (agent-shell-markdown--watermark-start) open-dollar))))))

(ert-deftest agent-shell-markdown-render-functions-all-math-cases ()
  ;; A renderer that claims every math form the PR supports and wraps its
  ;; LaTeX in brackets: inline \(..\) as [..], and block \[..\], $$..$$ and
  ;; fenced ```math / ```latex as the multi-line [\n..\n].  It routes the
  ;; fenced blocks by `:language', keeps $$ inside a fenced code block
  ;; literal, and uses `:inline-code-ranges' to keep a \(..\) inside an
  ;; inline `code` span literal too.
  (with-temp-buffer
    (let ((agent-shell-markdown-render-functions
           (list
            (lambda (context)
              ;; Fenced ```math / ```latex blocks, claimed by language.
              ;; Back-to-front so replacing one block does not disturb the
              ;; markers of an adjacent earlier one.
              (dolist (block (reverse (map-elt context :source-blocks)))
                (when (and (member (map-elt block :language) '("math" "latex"))
                           (map-elt block :complete))
                  (let ((start (map-nested-elt block '(:block :start)))
                        (end (map-nested-elt block '(:block :end)))
                        (body (map-elt block :body)))
                    (delete-region start end)
                    (goto-char start)
                    (insert (format "[\n%s\n]\n\n" body))
                    (put-text-property start (point)
                                       'agent-shell-markdown-frozen t))))
              ;; Inline / block delimiters, skipping matches that fall
              ;; inside code: a non-math fenced block (so $$ in code stays
              ;; literal) or an inline `code` span from
              ;; `:inline-code-ranges' (so a literal \(..\) the agent meant
              ;; as code stays literal).
              (let ((code-ranges
                     (append
                      (map-elt context :inline-code-ranges)
                      (seq-keep
                       (lambda (b)
                         (unless (member (map-elt b :language) '("math" "latex"))
                           (cons (map-nested-elt b '(:block :start))
                                 (map-nested-elt b '(:block :end)))))
                       (map-elt context :source-blocks)))))
                (dolist (spec (list (list (rx "\\(" (group (*? anychar)) "\\)") "[%s]")
                                    (list (rx "\\[" (group (*? anychar)) "\\]") "[\n%s\n]\n")
                                    (list (rx "$$" (group (*? anychar)) "$$") "[\n%s\n]\n")))
                  (save-excursion
                    (goto-char (point-min))
                    (while (re-search-forward (car spec) nil t)
                      (let ((start (match-beginning 0))
                            (content (match-string 1)))
                        (unless (or (get-text-property start 'agent-shell-markdown-frozen)
                                    (seq-some (lambda (r) (and (>= start (car r))
                                                              (< start (cdr r))))
                                              code-ranges))
                          (replace-match (format (cadr spec) content) nil t)
                          (put-text-property start (point)
                                             'agent-shell-markdown-frozen t)))))))
              nil))))
      (insert "```python
q = \"$$not math$$\"
```
inline \\(a+b\\) here
verbatim `\\(z\\)` code
\\[x = y\\]
$$E = mc^2$$
```math
\\frac{a}{b}
```
```latex
\\alpha
```
")
      (agent-shell-markdown-replace-markup)
      ;; Every math form rendered with its LaTeX in brackets; the $$ inside
      ;; the python block stayed literal (its language kept it out of
      ;; reach), and the \(z\) inside the inline `code` span stayed literal
      ;; too (`:inline-code-ranges' kept it out of reach).  The blank line
      ;; between the python block and "inline [a+b] here" is the framing
      ;; gap `--pad-rendered-blocks' adds below a block that butts
      ;; against following prose.
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "
python ⧉

q = \"$$not math$$\"


inline [a+b] here
verbatim \\(z\\) code
[
x = y
]

[
E = mc^2
]

[
\\frac{a}{b}
]

[
\\alpha
]

")))))

;;; Reconstructing markdown from rendered text (copy-as-markdown).

(defun agent-shell-markdown-tests--roundtrip (markdown)
  "Render MARKDOWN, then reconstruct it from the whole buffer.
Returns the reconstructed markdown, which should equal MARKDOWN
for a fully-selected buffer."
  (with-temp-buffer
    (insert markdown)
    (agent-shell-markdown-replace-markup :force t :render-images nil)
    (agent-shell-markdown-reconstruct (point-min) (point-max))))

(ert-deftest agent-shell-markdown-reconstruct-inline ()
  (should (equal (agent-shell-markdown-tests--roundtrip
                  "Some **bold**, *italic*, `code` and a [link](https://x.com).\n")
                 "Some **bold**, *italic*, `code` and a [link](https://x.com).\n")))

(ert-deftest agent-shell-markdown-reconstruct-header ()
  (should (equal (agent-shell-markdown-tests--roundtrip "## My title\n")
                 "## My title\n")))

(ert-deftest agent-shell-markdown-reconstruct-fenced-block ()
  (should (equal (agent-shell-markdown-tests--roundtrip
                  "```python\ndef foo():\n    return 1\n```\n")
                 "```python\ndef foo():\n    return 1\n```\n")))

(ert-deftest agent-shell-markdown-reconstruct-table ()
  (should (equal (agent-shell-markdown-tests--roundtrip
                  "| A | B |\n|---|---|\n| 1 | 2 |\n")
                 "| A | B |\n|---|---|\n| 1 | 2 |\n")))

(ert-deftest agent-shell-markdown-reconstruct-mixed ()
  (let ((markdown (concat "# Title\n\n"
                          "A **bold** paragraph.\n\n"
                          "```js\nx = 1\n```\n\n"
                          "> a quote\n\n"
                          "- item *one*\n- item two\n")))
    (should (equal (agent-shell-markdown-tests--roundtrip markdown) markdown))))

(ert-deftest agent-shell-markdown-reconstruct-nested ()
  ;; Nested and overlapping markup reconstructs faithfully, including
  ;; the mirror cases (`**_x_**' vs `[**b**](u)') where two constructs
  ;; land on the same characters.
  (dolist (markdown '("This is **bold _and italic_ inside**."
                      "**_x_**"
                      "a link with [**bold** text](https://x.com) inside"
                      "**bold `code` and _italic_ end**"
                      "## A **big** title\n"))
    (should (equal (agent-shell-markdown-tests--roundtrip markdown) markdown))))

(ert-deftest agent-shell-markdown-reconstruct-partial-selection-is-verbatim ()
  ;; A construct only partially covered by the region is copied as shown
  ;; (visible text), not reconstructed to its source.
  (with-temp-buffer
    (insert "Some **bold text** here.\n")
    (agent-shell-markdown-replace-markup :force t :render-images nil)
    ;; Rendered buffer is "Some bold text here.\n"; selecting from the
    ;; middle of the span through the end must not restore any `**'.
    (should (equal (agent-shell-markdown-reconstruct
                    (+ (point-min) 7) (point-max))
                   "ld text here.\n"))))

(ert-deftest agent-shell-markdown-reconstruct-across-streaming ()
  ;; Markup split unclosed across two render passes still reconstructs.
  (with-temp-buffer
    (insert "A **para")
    (agent-shell-markdown-replace-markup :render-images nil)
    (goto-char (point-max))
    (insert "graph** here.\n\nsecond line.\n")
    (agent-shell-markdown-replace-markup :render-images nil)
    (should (equal (agent-shell-markdown-reconstruct
                    (point-min) (point-max))
                   "A **paragraph** here.\n\nsecond line.\n"))))

;;; Exposing rendered link URLs (issue #669).

(ert-deftest agent-shell-markdown-link-exposes-url ()
  (with-temp-buffer
    (insert "see [docs](https://example.com) ok")
    (agent-shell-markdown-replace-markup :force t :render-images nil)
    ;; Markup is replaced with the visible title.
    (should (equal (buffer-string) "see docs ok"))
    (goto-char (point-min))
    (search-forward "docs")
    (let ((on-link (1- (point))))
      ;; The URL is recoverable from a text property on the title.
      (should (equal (agent-shell-markdown-link-url-at-point on-link)
                     "https://example.com"))
      ;; Click behaviour is preserved (keymap still on the title).
      (should (get-text-property on-link 'keymap)))
    ;; Off the link there is no URL.
    (should-not (agent-shell-markdown-link-url-at-point (point-min)))))

(ert-deftest agent-shell-markdown-link-url-at-point-defaults-to-point ()
  (with-temp-buffer
    (insert "[docs](https://example.com)")
    (agent-shell-markdown-replace-markup :force t :render-images nil)
    (goto-char (1+ (point-min)))
    (should (equal (agent-shell-markdown-link-url-at-point)
                   "https://example.com"))))

;;; Exposing rendered code block bodies at point.

(ert-deftest agent-shell-markdown-source-block-at-point ()
  (with-temp-buffer
    (insert "```python\ndef foo():\n    return 1\n```\n")
    (agent-shell-markdown-replace-markup :force t :render-images nil)
    (goto-char (point-min))
    (search-forward "def foo")
    ;; Point on the body returns the code without fences or label.
    (should (equal (agent-shell-markdown-source-block-at-point (1- (point)))
                   "def foo():\n    return 1"))
    ;; The language label above the body is not the body.
    (goto-char (point-min))
    (search-forward "⧉")
    (should-not (agent-shell-markdown-source-block-at-point (1- (point))))))

(provide 'agent-shell-markdown-tests)

;;; agent-shell-markdown-tests.el ends here

(ert-deftest agent-shell-markdown-linkify-file-reference-in-inline-code-test ()
  "Test a cited `FILE:LINE' is linked, inside backticks or in prose.

Agents nearly always wrap a citation in backticks, and an inline `code'
body is tagged `agent-shell-markdown-frozen', so a pass honouring that
tag would leave almost every reference plain."
  (let* ((directory (make-temp-file "agent-shell-refs" t))
         (default-directory directory)
         (file (expand-file-name "foo.el" directory)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "one\ntwo\nthree\n"))
          (with-temp-buffer
            (insert "see `foo.el:2` and foo.el:3 here\n")
            (agent-shell-markdown-replace-markup :force t :complete t)
            (goto-char (point-min))
            (search-forward "foo.el:2")
            (should (equal "foo.el:2"
                           (get-text-property (match-beginning 0)
                                              'agent-shell-markdown-url)))
            (search-forward "foo.el:3")
            (should (equal "foo.el:3"
                           (get-text-property (match-beginning 0)
                                              'agent-shell-markdown-url)))))
      (delete-directory directory t))))

(ert-deftest agent-shell-markdown-linkify-file-reference-needs-a-file-test ()
  "Test prose that merely reads like a citation stays prose.

The path is what decides: matching the shape is not enough, since
ordinary text and version numbers land on it too."
  (let* ((directory (make-temp-file "agent-shell-refs" t))
         (default-directory directory))
    (unwind-protect
        (with-temp-buffer
          (insert "absent.el:2 and `nope/missing.txt:10-20` stay text\n")
          (agent-shell-markdown-replace-markup :force t :complete t)
          (should-not (text-property-not-all (point-min) (point-max)
                                             'agent-shell-markdown-url nil)))
      (delete-directory directory t))))

(ert-deftest agent-shell-markdown-linkify-file-reference-skips-fenced-blocks-test ()
  "Test a path inside a fenced block is left alone.

There it is sample content or command output rather than a citation,
while the same path in prose alongside it is linked."
  (let* ((directory (make-temp-file "agent-shell-refs" t))
         (default-directory directory)
         (file (expand-file-name "foo.el" directory)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "one\ntwo\nthree\n"))
          (with-temp-buffer
            (insert "cited foo.el:2 here\n\n```console\nfoo.el:3: matched\n```\n")
            (agent-shell-markdown-replace-markup :force t :complete t)
            (goto-char (point-min))
            (search-forward "foo.el:2")
            (should (get-text-property (match-beginning 0)
                                       'agent-shell-markdown-url))
            (search-forward "foo.el:3")
            (should-not (get-text-property (match-beginning 0)
                                           'agent-shell-markdown-url))))
      (delete-directory directory t))))
