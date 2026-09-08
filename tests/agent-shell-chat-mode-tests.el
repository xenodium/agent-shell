;;; agent-shell-chat-mode-tests.el --- Tests for agent-shell-chat-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-chat-mode)

;;; Code:

;; Declared special so tests can dynamically bind it (the prompt bar need
;; not be loaded); chat-mode reads it with `bound-and-true-p'.
(defvar agent-shell-prompt-bar-mode)

;; A self-contained `:extend' background face, standing in for a code
;; block panel (whose real face lives in `agent-shell-markdown', not
;; loaded here).  `:extend' is what chat-mode keys off, not the color.
(defface agent-shell-chat-mode-tests--panel
  '((t :extend t :background "gray20"))
  "An `:extend' background face for tests."
  :group 'agent-shell)

(defface agent-shell-chat-mode-tests--plain
  '((t :extend nil))
  "A face that does not extend, for tests."
  :group 'agent-shell)

(defun agent-shell-chat-mode-tests--label-string (me)
  "Return the label rendered for ME, a `Me' overlay.
The label rides the newline above the prompt, on its own overlay, so that
nothing is shown at the prompt itself.  Falls back to ME when the run has
no newline above to carry it."
  (or (seq-some (lambda (overlay)
                  (and (eq (overlay-get overlay 'agent-shell-chat--tag) 'me-label)
                       (= (overlay-end overlay) (overlay-start me))
                       (overlay-get overlay 'before-string)))
                (overlays-in (max (point-min) (1- (overlay-start me)))
                             (1+ (overlay-start me))))
      (overlay-get me 'before-string)))

(defun agent-shell-chat-mode-tests--marker-string (me)
  "Return the string carrying ME\\='s prompt marker.
It travels as the covered prompt's `display' (standing on its buffer
positions, adding none) where the label rides the newline above; with no
newline to ride, the label renders on ME itself and the marker rejoins it
there."
  (or (seq-find (lambda (string) (string-match-p "❯" string))
                (list (or (overlay-get me 'display) "")
                      (or (overlay-get me 'before-string) "")))
      ""))

(defun agent-shell-chat-mode-tests--me-overlays ()
  "Return the `Me' label overlays in the current buffer, ordered by position."
  (sort (seq-filter (lambda (overlay)
                      (eq (overlay-get overlay 'agent-shell-chat--tag) 'me))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun agent-shell-chat-mode-tests--draft-overlay ()
  "Return the overlay indenting the live prompt's draft, or nil."
  (seq-find (lambda (overlay)
              (eq (overlay-get overlay 'agent-shell-chat--tag) 'me-draft))
            (overlays-in (point-min) (point-max))))

(defun agent-shell-chat-mode-tests--agent-overlays ()
  "Return the agent label overlays in the current buffer."
  (seq-filter (lambda (overlay)
                (eq (overlay-get overlay 'agent-shell-chat--tag) 'agent))
              (overlays-in (point-min) (point-max))))

(defun agent-shell-chat-mode-tests--prompt (text)
  "Insert a shell prompt run displaying TEXT, as shell-maker fontifies it."
  (insert (propertize text 'font-lock-face
                      '(comint-highlight-prompt comint-highlight-prompt))))

(defun agent-shell-chat-mode-tests--marker ()
  "Insert the invisible `<shell-maker-end-of-prompt>' marker.
Carries `shell-maker--marker', as shell-maker's real marker does."
  (insert (propertize "<shell-maker-end-of-prompt>"
                      'invisible t 'shell-maker--marker t)))

(defmacro agent-shell-chat-mode-tests--with-shell (&rest body)
  "Run BODY in a labeled shell buffer named \"Claude\"."
  (declare (indent 0))
  `(with-temp-buffer
     (setq-local agent-shell--state '((:agent-config . ((:mode-line-name . "Claude"))))
                 agent-shell-chat--labeled t)
     ,@body))

(ert-deftest agent-shell-chat--turn-label-at-ignores-empty-label ()
  "Only the overlay actually drawing a label reports one.
Chat mode gives the other overlay of the pair an empty `before-string',
which is still non-nil, so a turn would otherwise be labelled twice."
  (with-temp-buffer
    (setq-local agent-shell-chat-mode t)
    (insert "Claude> question")
    (let ((label (make-overlay 1 3))
          (body (make-overlay 3 9)))
      (overlay-put label 'agent-shell-chat--tag 'me-label)
      (overlay-put label 'before-string "Me\n\n")
      (overlay-put body 'agent-shell-chat--tag 'me)
      (overlay-put body 'before-string ""))
    (should (equal (agent-shell-chat--turn-label-at 1) "Me"))
    (should-not (agent-shell-chat--turn-label-at 3))))

(ert-deftest agent-shell-chat--hidden-range-at-reports-range ()
  "A covering overlay reports where the text it stands in for ends."
  (with-temp-buffer
    (setq-local agent-shell-chat-mode t)
    (insert "Claude> question")
    (let ((overlay (make-overlay 1 9)))
      (overlay-put overlay 'agent-shell-chat--tag 'me-label)
      (overlay-put overlay 'display "")
      (overlay-put overlay 'before-string "Me\n\n"))
    (should (equal (map-elt (agent-shell-chat--hidden-range-at 1) :start) 1))
    (should (equal (map-elt (agent-shell-chat--hidden-range-at 1) :end) 9))
    (should (equal (map-elt (agent-shell-chat--hidden-range-at 1) :start) 1))
    (should (equal (map-elt (agent-shell-chat--hidden-range-at 1) :end) 9))
    (should-not (agent-shell-chat--hidden-range-at 5))))

(ert-deftest agent-shell-chat--displayed-substring-applies-overlays ()
  "Reading a region follows the screen: labels in, covered chrome out.
Chat mode draws labels with `before-string' and hides the prompt behind
`display', neither of which `buffer-substring' can see."
  (with-temp-buffer
    (setq-local agent-shell-chat-mode t)
    (insert "Claude> hi")
    (let ((overlay (make-overlay 1 9)))
      (overlay-put overlay 'agent-shell-chat--tag 'me)
      (overlay-put overlay 'display "")
      (overlay-put overlay 'before-string "Me\n\n"))
    (should (equal (substring-no-properties
                    (agent-shell-chat--displayed-substring (point-min) (point-max)))
                   "Me\n\nhi"))))

(ert-deftest agent-shell-chat--displayed-substring-ignores-foreign-overlays ()
  "Only chat mode's own overlays are substituted.
An image `display' from elsewhere must not be stringified, so overlays
without an `agent-shell-chat--tag' are left alone."
  (with-temp-buffer
    (setq-local agent-shell-chat-mode t)
    (insert "hello")
    (let ((overlay (make-overlay 1 6)))
      (overlay-put overlay 'display '(image :type png :file "x.png"))
      (overlay-put overlay 'before-string "IGNORED"))
    (should (equal (substring-no-properties
                    (agent-shell-chat--displayed-substring (point-min) (point-max)))
                   "hello"))))

(ert-deftest agent-shell-chat--displayed-substring-passes-through-when-off ()
  "With chat mode off the region reads as plain `buffer-substring'."
  (with-temp-buffer
    (setq-local agent-shell-chat-mode nil)
    (insert "Claude> hi")
    (let ((overlay (make-overlay 1 9)))
      (overlay-put overlay 'agent-shell-chat--tag 'me)
      (overlay-put overlay 'display "")
      (overlay-put overlay 'before-string "Me\n\n"))
    (should (equal (substring-no-properties
                    (agent-shell-chat--displayed-substring (point-min) (point-max)))
                   "Claude> hi"))))

(ert-deftest agent-shell-chat-prompt-face-p-test ()
  "Prompt runs are recognized whether the face is a symbol or a list."
  (should (agent-shell-chat--prompt-face-p 'comint-highlight-prompt))
  (should (agent-shell-chat--prompt-face-p '(comint-highlight-prompt comint-highlight-prompt)))
  (should-not (agent-shell-chat--prompt-face-p 'default))
  (should-not (agent-shell-chat--prompt-face-p nil)))

(ert-deftest agent-shell-chat-agent-name-test ()
  "The agent label uses `:mode-line-name', falling back to \"Agent\"."
  (agent-shell-chat-mode-tests--with-shell
    (should (equal "Claude" (agent-shell-chat--agent-name))))
  (with-temp-buffer
    (setq-local agent-shell--state nil)
    (should (equal "Agent" (agent-shell-chat--agent-name)))))

(ert-deftest agent-shell-chat-labels-submitted-turn-test ()
  "A submitted turn boxes the prompt as `Me' and the response as the agent."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "hi there\n")
    (agent-shell-chat--relabel)
    (let ((me (agent-shell-chat-mode-tests--me-overlays))
          (agent (agent-shell-chat-mode-tests--agent-overlays)))
      (should (= 1 (length me)))
      (should (string-match-p "Me" (overlay-get (car me) 'before-string)))
      (should (= 1 (length agent)))
      (should (string-match-p "Claude" (overlay-get (car agent) 'before-string))))))

(ert-deftest agent-shell-chat-live-prompt-hidden-with-bar-test ()
  "The empty live prompt shows `Me', or is hidden when the bar is on."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel)
      (should (string-match-p
               "Me" (agent-shell-chat-mode-tests--label-string
                     (car (agent-shell-chat-mode-tests--me-overlays))))))
    (let ((agent-shell-prompt-bar-mode t))
      (agent-shell-chat--relabel)
      (should (equal
               "" (agent-shell-chat-mode-tests--label-string
                   (car (agent-shell-chat-mode-tests--me-overlays))))))))

(ert-deftest agent-shell-chat-agent-keeps-input-terminator-test ()
  "The agent overlay leaves the input's line terminator visible.
Hiding that newline with `display' would merge the input line into the
response for line motion such as `end-of-visual-line'."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello")
    (let ((terminator (point)))
      (insert "\n")
      (agent-shell-chat-mode-tests--marker)
      (insert "reply\n")
      (agent-shell-chat--relabel)
      (let ((agent (car (agent-shell-chat-mode-tests--agent-overlays))))
        ;; The overlay begins past the terminator, so it is not covered.
        (should (> (overlay-start agent) terminator))
        (should-not (get-char-property terminator 'display))))))

(ert-deftest agent-shell-chat-agent-keeps-terminator-restored-test ()
  "A restored turn keeps the input terminator that follows the marker.
Restored input abuts the marker (\"input<marker>\\n\") rather than
preceding it (\"input\\n<marker>\"); the terminator after the marker must
stay visible so line motion does not merge the input into the response."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "restored input?")
    (agent-shell-chat-mode-tests--marker)
    (let ((terminator (point)))
      (insert "\n\nreply\n")
      (agent-shell-chat--relabel)
      (let ((agent (car (agent-shell-chat-mode-tests--agent-overlays))))
        ;; The overlay begins past the terminator, leaving it visible.
        (should (> (overlay-start agent) terminator))
        (should-not (get-char-property terminator 'display))))))

(ert-deftest agent-shell-chat-me-keeps-response-terminator-test ()
  "The `Me' overlay after a response keeps the response's last line terminator.
Hiding it would merge the last output line into the label for line motion
such as `end-of-visual-line'."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "the reply")
    (let ((terminator (point)))
      (insert "\n\n")
      (agent-shell-chat-mode-tests--prompt "Claude> ")
      (agent-shell-chat--relabel)
      ;; The live prompt's overlay begins past the response terminator.
      (let ((me (car (last (agent-shell-chat-mode-tests--me-overlays)))))
        (should (> (overlay-start me) terminator))
        (should-not (get-char-property terminator 'display))))))

(ert-deftest agent-shell-chat-live-prompt-excludes-replaced-output-test ()
  "Replacing output before the live prompt does not hide its replacement.

An inline control replaces its fragment body by deleting the old body
before inserting the new one.  The live prompt's whitespace overlay
survives because one structural newline remains.  Text inserted at that
overlay's front must stay outside its empty `display', as \"new form\"
does here."
  (agent-shell-chat-mode-tests--with-shell
    (let ((body-start (point)))
      (insert "old form\n")
      (let ((body-end (point)))
        (insert "\n\n")
        (agent-shell-chat-mode-tests--prompt "Claude> ")
        (agent-shell-chat--relabel)
        (let ((surplus
               (seq-find
                (lambda (overlay)
                  (eq
                   (overlay-get overlay 'agent-shell-chat--tag)
                   'me-surplus))
                (overlays-in (point-min) (point-max)))))
          (should surplus)
          (delete-region body-start body-end)
          (should (= body-start (overlay-start surplus)))
          (goto-char body-start)
          (insert "new form\n\n")
          (should-not
           (equal ""
                  (get-char-property body-start 'display))))))))

(ert-deftest agent-shell-chat-label-is-before-string-test ()
  "The `Me' label renders as a `before-string' with an empty `display'.
Like the agent label, this keeps the cursor from landing on it during
vertical motion (a `display' string is backed by buffer positions)."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (agent-shell-chat--relabel)
    (dolist (me (agent-shell-chat-mode-tests--me-overlays))
      (should (equal "" (overlay-get me 'display)))
      (should (string-match-p "Me" (agent-shell-chat-mode-tests--label-string me))))))

(ert-deftest agent-shell-chat-errors-outside-agent-shell-test ()
  "Enabling the mode outside an `agent-shell' buffer signals a `user-error'.
The mode stays off after the failed attempt."
  (with-temp-buffer
    (should-error (agent-shell-chat-mode 1) :type 'user-error)
    (should-not agent-shell-chat-mode)))

(ert-deftest agent-shell-chat-preserves-code-block-top-padding-test ()
  "A response opening with a code block keeps the panel's tinted top padding.
The agent overlay stops before the `:extend' padding instead of hiding it
with `display'."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    ;; Response opens directly with a code block panel's tinted top padding.
    (insert (propertize "\n" 'face 'agent-shell-chat-mode-tests--panel))
    (let ((panel-top (1- (point))))
      (insert (propertize "elisp\ncode\n" 'face 'agent-shell-chat-mode-tests--panel))
      (agent-shell-chat--relabel)
      (let ((agent (car (agent-shell-chat-mode-tests--agent-overlays))))
        ;; The overlay ends before the panel padding, leaving it visible.
        (should (<= (overlay-end agent) panel-top))
        (should-not (get-char-property panel-top 'display))))))

(ert-deftest agent-shell-chat-no-duplicate-agent-overlays-test ()
  "Relabeling a restored turn does not accumulate duplicate agent overlays.
A restored turn's overlay starts past the marker, so the reuse search must
cover the whole span rather than the marker alone."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "restored input?")
    (agent-shell-chat-mode-tests--marker)
    (insert "\n\nthe reply\n")
    (agent-shell-chat--relabel)
    (agent-shell-chat--relabel)
    (agent-shell-chat--relabel)
    (should (= 1 (length (agent-shell-chat-mode-tests--agent-overlays))))))

(ert-deftest agent-shell-chat-gc-stale-label-test ()
  "Relabeling removes a `Me' label whose prompt run no longer exists.
A live prompt a `session/push' deletes can leave the overlay behind (it
covers blank lines outside the deleted text), so the sweep must drop it."
  (agent-shell-chat-mode-tests--with-shell
    (insert "\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (agent-shell-chat--relabel)
    (should (= 1 (length (agent-shell-chat-mode-tests--me-overlays))))
    ;; Strip the prompt face so the run vanishes (as reworking/removing the
    ;; prompt would), leaving the overlay stranded.
    (remove-text-properties (point-min) (point-max) '(font-lock-face nil))
    (agent-shell-chat--relabel)
    (should (= 0 (length (agent-shell-chat-mode-tests--me-overlays))))))

(ert-deftest agent-shell-chat-gc-stale-agent-label-test ()
  "Relabeling sweeps an agent label no marker accounts for.
The agent side sweeps like the `Me' side does, so a label the relabel
does not reach is dropped rather than left rendering."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "typed\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n")
    (agent-shell-chat--relabel)
    (should (= 1 (length (agent-shell-chat-mode-tests--agent-overlays))))
    ;; Strand one away from any marker, so relabeling neither reuses nor
    ;; reaches it and only the sweep can remove it.
    (let ((stale (make-overlay (point-min) (+ (point-min) 3))))
      (overlay-put stale 'agent-shell-chat--tag 'agent)
      (overlay-put stale 'before-string " Claude "))
    (should (= 2 (length (agent-shell-chat-mode-tests--agent-overlays))))
    (agent-shell-chat--relabel)
    (should (= 1 (length (agent-shell-chat-mode-tests--agent-overlays))))))

(ert-deftest agent-shell-chat-invisible-terminator-pads-label-test ()
  "A `Me' label keeps its blank line when the content's newline is invisible.
A collapsed fragment hides its own trailing newline, so that newline cannot
end the content line.  The label must then pad with a full blank line of its
own rather than assume the hidden one separates it."
  (agent-shell-chat-mode-tests--with-shell
    (insert "collapsed output")
    ;; The fragment's trailing newline, hidden as a collapsed fragment does.
    (insert (propertize "\n" 'invisible t))
    (insert "\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "typed\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n")
    (agent-shell-chat--relabel)
    (let ((before (agent-shell-chat-mode-tests--label-string
                   (car (agent-shell-chat-mode-tests--me-overlays)))))
      ;; Two newlines: one ends the content line the hidden one could not,
      ;; the second is the blank line separating the label.
      (should (string-prefix-p "\n\n" before)))))

(ert-deftest agent-shell-chat-visible-terminator-single-lead-test ()
  "A `Me' label adds one newline when the content's newline is visible.
The visible terminator already ends the content line, so a second newline
would render two blank lines instead of one."
  (agent-shell-chat-mode-tests--with-shell
    (insert "plain output\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "typed\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n")
    (agent-shell-chat--relabel)
    (let ((before (overlay-get (car (agent-shell-chat-mode-tests--me-overlays))
                               'before-string)))
      (should (string-prefix-p "\n" before))
      (should-not (string-prefix-p "\n\n" before)))))

(ert-deftest agent-shell-chat-nothing-shown-at-prompt-test ()
  "Nothing is shown at the prompt position itself.

A string there holds point and the cursor on the row above, so moving up
into an input spanning several lines lands above the input rather than on
its first line.  The label rides the newline above instead, and the marker
travels as a `line-prefix'; neither occupies a buffer position, leaving
the input reachable."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "first line\n\nsecond line")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((me (car (last (agent-shell-chat-mode-tests--me-overlays)))))
      (should (equal "" (overlay-get me 'before-string)))
      (should (string-match-p "❯" (agent-shell-chat-mode-tests--marker-string me)))
      ;; The label rides the newline that closes the line above the prompt.
      (should (string-match-p "Me" (agent-shell-chat-mode-tests--label-string me)))
      (should (eq ?\n (char-before (overlay-start me)))))))

(ert-deftest agent-shell-chat-live-prompt-marker-not-line-prefix-test ()
  "The live prompt's marker is a `display', never a `line-prefix'.

A `line-prefix' belongs to the whole line, and the input's first line
shares its line with the covered prompt.  Redisplay then cannot take its
cheap single-line path: every edit re-lays the line out, and the input
visibly paints unindented before jumping right.

Drawing the marker as the covered prompt's `display' costs nothing per
edit, and still leaves the input's first line reachable by `previous-line'
because it stands on the prompt's own buffer positions rather than adding
any (contrast a `before-string', which would not -- see #786)."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "typing here")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((me (car (last (agent-shell-chat-mode-tests--me-overlays)))))
      (should (string-match-p "❯" (or (overlay-get me 'display) "")))
      (should-not (string-match-p "❯" (or (overlay-get me 'line-prefix) ""))))))

(ert-deftest agent-shell-chat-decorates-prompt-region-test ()
  "A prompt read outside the shell is hidden behind the `Me' label.
Laid out as the shell lays out its own live prompt: the label, one blank
line, then the marker the input follows."
  (with-temp-buffer
    (insert "Claude> ")
    (let ((before (overlay-get (agent-shell-chat--decorate-prompt-region
                                (point-min) (point-max))
                               'before-string)))
      (should (string-match-p "Me" before))
      (should (string-match-p "❯" before))
      (should (string-match-p "Me.*\n\n.*❯" before)))))

(ert-deftest agent-shell-chat-queued-prompt-skips-plain-shell-test ()
  "A queued prompt for a shell without chat mode is left undecorated."
  (with-temp-buffer
    (let ((shell (current-buffer)))
      (setq-local agent-shell-chat-mode nil)
      (with-temp-buffer
        (insert "Claude> ")
        (should-not (agent-shell-chat--decorate-queued-prompt
                     `((:shell-buffer . ,shell))))
        (should-not (overlays-in (point-min) (point-max)))))))

(ert-deftest agent-shell-chat-marker-renders-once-test ()
  "The prompt marker is shown once, however many rows the label spans.

With no newline above to carry it, the label renders on the prompt\\='s own
overlay and each of its blank lines becomes a row of that line.  A marker
travelling as a prefix would then repeat down every one of those rows,
which is what a cleared shell renders."
  (agent-shell-chat-mode-tests--with-shell
    ;; A prompt starting the buffer has no newline above it, which is what
    ;; clearing the shell leaves behind.
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((me (car (agent-shell-chat-mode-tests--me-overlays))))
      (should (= 1 (seq-count (lambda (string) (string-match-p "❯" string))
                              (list (or (overlay-get me 'before-string) "")
                                    (or (overlay-get me 'line-prefix) "")))))
      ;; Never from `wrap-prefix', which repeats on every row.
      (should-not (string-match-p "❯" (or (overlay-get me 'wrap-prefix) ""))))))

(ert-deftest agent-shell-chat-label-not-indented-on-own-line-test ()
  "A label rendering on the prompt\\='s line is not indented with the input.

With no newline above to carry it the label renders there, so an indent
meant for the input would shift the label too, leaving it out of line
with every other label in the buffer.  This is what a cleared shell
renders."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "typed\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n")
    (agent-shell-chat--relabel)
    (let ((me (car (agent-shell-chat-mode-tests--me-overlays))))
      ;; The label renders here, so nothing indents this line.
      (should (string-match-p "Me" (or (overlay-get me 'before-string) "")))
      (should (equal "" (overlay-get me 'line-prefix)))
      (should (equal "" (overlay-get me 'wrap-prefix))))))

(ert-deftest agent-shell-chat-overlays-carry-no-category-test ()
  "Chat overlays name themselves with their own property, never `category'.

A `category' hands redisplay every property its symbol carries, so a
value that is also a face name lends its internal face id and floods
`*Messages*' with \"Invalid face reference\".  Keeping the tag off
`category' removes the mechanism rather than avoiding the collision."
  (agent-shell-chat-mode-tests--with-shell
    ;; A buffer holding a label overlay of every tag.
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "typed\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n\n\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "draft\n")
    (agent-shell-chat--relabel)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (should (overlay-get overlay 'agent-shell-chat--tag))
      (should-not (overlay-get overlay 'category))
      ;; Keywords cannot name a face, so the id cannot leak either way.
      (should-not (facep (overlay-get overlay 'agent-shell-chat--tag))))))

(ert-deftest agent-shell-chat-sweeps-pre-property-overlays-test ()
  "Labels left by a version that used `category' are swept, not doubled up.

An upgrade reloads this file into a running session, where shells labeled
by the older version keep overlays relabeling no longer recognises: left
alone they render their label a second time."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "typed\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (agent-shell-chat--relabel)
    ;; Stand in for what the older version left behind.
    (dolist (category '(agent-shell-chat-me agent-shell-chat-me-label
                                            agent-shell-chat-me-input))
      (let ((stale (make-overlay (point-min) (1+ (point-min)))))
        (overlay-put stale 'category category)
        (overlay-put stale 'before-string " Me ")))
    (agent-shell-chat--relabel)
    (should-not (seq-find (lambda (overlay) (overlay-get overlay 'category))
                          (overlays-in (point-min) (point-max))))))

(ert-deftest agent-shell-chat-relabel-idempotent-test ()
  "Relabeling twice does not duplicate overlays."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "hi\n")
    (agent-shell-chat--relabel)
    (agent-shell-chat--relabel)
    (should (= 1 (length (agent-shell-chat-mode-tests--me-overlays))))
    (should (= 1 (length (agent-shell-chat-mode-tests--agent-overlays))))))

(ert-deftest agent-shell-chat-absorbs-leading-blank-lines-test ()
  "A submitted prompt's overlay swallows the input's leading blank lines."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((input-start (point)))
      (insert "\n\nPadded\n")
      (agent-shell-chat-mode-tests--marker)
      (insert "reply\n")
      (agent-shell-chat--relabel)
      (should (= (overlay-end (car (agent-shell-chat-mode-tests--me-overlays)))
                 (save-excursion
                   (goto-char input-start)
                   (skip-chars-forward " \t\n")
                   (point)))))))

(ert-deftest agent-shell-chat-live-prompt-shows-marker-test ()
  "The live prompt shows the `❯' marker, faced `default' (not the prompt face)."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel)
      (let ((display (agent-shell-chat-mode-tests--marker-string
                      (car (agent-shell-chat-mode-tests--me-overlays)))))
        (should (string-match-p "❯" display))
        ;; The marker must not inherit the covered prompt face.
        (should (eq 'default
                    (get-text-property (string-match "❯" display) 'face display)))))))

(ert-deftest agent-shell-chat-live-prompt-keeps-marker-while-typing-test ()
  "The live prompt keeps the `❯' marker after text is typed into it.
The marker is keyed off being the last (live) prompt, not off empty input,
so it does not vanish mid-type when a relabel runs."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "half-typed input")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((me (car (agent-shell-chat-mode-tests--me-overlays))))
      (should (string-match-p "❯" (agent-shell-chat-mode-tests--marker-string me)))
      ;; No indent overlay claims the live prompt's in-progress input.
      (should-not (seq-filter (lambda (overlay)
                                (eq (overlay-get overlay 'agent-shell-chat--tag)
                                    'me-input))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest agent-shell-chat-submitted-input-first-line-indented-test ()
  "A submitted turn's first input line lines up with the rest of it.

That line is shared with the prompt, which is covered, so its indent has
to come from the prompt's own overlay: `line-prefix' is read at the start
of a line, and that is where the covered prompt sits."
  (agent-shell-chat-mode-tests--with-shell
    ;; A turn above, so the label rides the newline it leaves and this line
    ;; carries the input alone.
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "earlier\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "earlier reply\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((input (point)))
      (insert "first line\nsecond line\n")
      (agent-shell-chat-mode-tests--marker)
      (insert "reply\n")
      (agent-shell-chat--relabel)
      (let ((first-line (get-char-property
                         (save-excursion (goto-char input)
                                         (line-beginning-position))
                         'line-prefix))
            (second-line (get-char-property
                          (save-excursion (goto-char input)
                                          (forward-line 1)
                                          (line-beginning-position))
                          'line-prefix)))
        (should (equal agent-shell-chat--body-indent first-line))
        (should (equal first-line second-line))))))

(ert-deftest agent-shell-chat-submitted-input-indented-test ()
  "A submitted turn's input carries the response body indent, the live one none."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "hi there\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (agent-shell-chat--relabel)
    (let ((input (seq-filter (lambda (overlay)
                               (eq (overlay-get overlay 'agent-shell-chat--tag)
                                   'me-input))
                             (overlays-in (point-min) (point-max)))))
      ;; Only the submitted turn gets an indent overlay; the live prompt's
      ;; draft is covered by its own.
      (should (= 1 (length input)))
      (should (equal agent-shell-chat--body-indent
                     (overlay-get (car input) 'line-prefix))))))

(ert-deftest agent-shell-chat-live-draft-indented-test ()
  "The live prompt's draft carries the body indent below its first line.
The marker indents the first line; this indents the lines under it, which
the overlay covering the prompt text cannot reach.  It is in place before
anything is typed and takes in what follows, since no relabel runs per
keystroke."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((draft (agent-shell-chat-mode-tests--draft-overlay)))
      (should draft)
      (should (equal agent-shell-chat--body-indent
                     (overlay-get draft 'line-prefix)))
      (should (equal agent-shell-chat--body-indent
                     (overlay-get draft 'wrap-prefix)))
      ;; Empty while nothing is typed, and grows with what is.
      (should (= (overlay-start draft) (overlay-end draft)))
      (goto-char (point-max))
      (insert "first line\nsecond line")
      (should (= (overlay-end draft) (point-max))))))

(ert-deftest agent-shell-chat-draft-empty-last-line-indented-test ()
  "A newline in the draft leaves its empty last line indented too.
That line starts at end of buffer, where there is no character to carry
the draft's `line-prefix', so the caret would sit flush left until the
next one arrived.  A string standing there indents it meanwhile, and
steps aside once a character can carry the prefix.  The draft's own
modification hooks keep this in step, since no relabel runs while a
prompt is being typed."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((draft (agent-shell-chat-mode-tests--draft-overlay)))
      (should draft)
      (goto-char (point-max))
      (should (equal "" (overlay-get draft 'after-string)))
      (insert "one")
      (should (equal "" (overlay-get draft 'after-string)))
      (insert "\n")
      (should (equal agent-shell-chat--body-indent
                     (overlay-get draft 'after-string)))
      ;; The first character of the new line takes over.
      (insert "t")
      (should (equal "" (overlay-get draft 'after-string)))
      ;; Deleting back to the empty line brings it back.
      (delete-char -1)
      (should (equal agent-shell-chat--body-indent
                     (overlay-get draft 'after-string)))
      ;; A relabel mid-draft leaves it alone.
      (let ((agent-shell-prompt-bar-mode nil))
        (agent-shell-chat--relabel))
      (should (equal agent-shell-chat--body-indent
                     (overlay-get draft 'after-string))))))

(ert-deftest agent-shell-chat-draft-stops-at-submission-test ()
  "The draft overlay stops growing once the turn is submitted.
It rear-advances so it covers what is typed without waiting for a
relabel, but the end-of-prompt marker and the response arrive at end of
buffer too.  Taking those in would indent the agent's output as though it
were still the draft, until the next relabel dropped the overlay."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((draft (agent-shell-chat-mode-tests--draft-overlay)))
      (should draft)
      (goto-char (point-max))
      (insert "my question\n")
      (agent-shell-chat-mode-tests--marker)
      (insert "\nstreamed reply\n")
      (should (equal "my question\n"
                     (buffer-substring-no-properties (overlay-start draft)
                                                     (overlay-end draft))))
      ;; No stray indent left standing at the marker either.
      (should (equal "" (overlay-get draft 'after-string))))))

(ert-deftest agent-shell-chat-quoted-marker-keeps-prompt-live-test ()
  "Text matching the end-of-prompt marker does not end the live prompt.
The marker is recognised by the `shell-maker--marker' property shell-maker
writes, not by its characters, so a prompt mentioning the delimiter (as
one asking about it would) is still the live prompt and still shows its
marker."
  (agent-shell-chat-mode-tests--with-shell
    (insert "an earlier turn\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "why does <shell-maker-end-of-prompt> appear?")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (should (string-match-p
             "❯" (agent-shell-chat-mode-tests--marker-string
                  (car (last (agent-shell-chat-mode-tests--me-overlays))))))))

(ert-deftest agent-shell-chat-quoted-marker-not-a-response-test ()
  "A response quoting the end-of-prompt marker is labeled once.
Agents write the delimiter back when asked about it.  Those characters
carry no `shell-maker--marker' property, and treating them as a boundary
renders a second agent label partway through the one response."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "explain the delimiter\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "\nshell-maker writes <shell-maker-end-of-prompt> after input.\n\n")
    (agent-shell-chat--relabel)
    (should (= 1 (length (agent-shell-chat-mode-tests--agent-overlays))))))

(ert-deftest agent-shell-chat-empty-submission-hidden-test ()
  "An empty submission (a prompt with another below it) is not labeled.
Only the live prompt shows an empty `Me', and neither claims input."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((me (agent-shell-chat-mode-tests--me-overlays)))
      (should (= 2 (length me)))
      ;; Empty submission: hidden.
      (should (equal "" (overlay-get (nth 0 me) 'before-string)))
      ;; Live prompt: `Me' and `❯'.
      (should (string-match-p "❯" (agent-shell-chat-mode-tests--marker-string (nth 1 me))))
      ;; Neither empty prompt claims input.
      (should-not (seq-filter (lambda (overlay)
                                (eq (overlay-get overlay 'agent-shell-chat--tag)
                                    'me-input))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest agent-shell-chat-stacked-empty-submissions-hidden-test ()
  "Consecutive empty submissions are all hidden; only the live prompt shows."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((me (agent-shell-chat-mode-tests--me-overlays)))
      (should (equal "" (overlay-get (nth 0 me) 'before-string)))
      (should (equal "" (overlay-get (nth 1 me) 'before-string)))
      ;; Only the live prompt is labeled.
      (should (string-match-p "❯" (agent-shell-chat-mode-tests--marker-string (nth 2 me)))))))

(ert-deftest agent-shell-chat-empty-response-not-labeled-test ()
  "A completed turn with no response text is not given an agent label."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (agent-shell-chat--relabel)
    ;; The empty response gets no agent label.
    (should-not (agent-shell-chat-mode-tests--agent-overlays))))

(ert-deftest agent-shell-chat-active-turn-labeled-test ()
  "A just-submitted turn is labeled before its output streams.
The marker sits at end of buffer with no response yet; it is the active
turn, not a completed empty one, so the agent label shows immediately."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (agent-shell-chat--relabel)
    (should (= 1 (length (agent-shell-chat-mode-tests--agent-overlays))))))

(ert-deftest agent-shell-chat-code-block-padding-preserved-test ()
  "A prompt after a code block panel keeps the panel's tinted padding.
The overlay starts past the `:extend' background and drops its `line-prefix'."
  (agent-shell-chat-mode-tests--with-shell
    (insert (propertize "code line" 'face 'agent-shell-chat-mode-tests--panel))
    (let ((panel-end (point)))
      ;; Tinted padding newlines (part of the panel).
      (insert (propertize "\n\n" 'face 'agent-shell-chat-mode-tests--panel))
      (insert "\n")
      (agent-shell-chat-mode-tests--prompt "Claude> ")
      (agent-shell-chat--relabel)
      (let ((me (car (agent-shell-chat-mode-tests--me-overlays))))
        ;; The overlay does not swallow the panel's tinted padding.
        (should (>= (overlay-start me) panel-end))
        (should-not (agent-shell-chat--extends-bg-p
                     (get-text-property (overlay-start me) 'face)))
        ;; It drops any inherited tinted gutter: the prefix is the marker
        ;; (faced `default') or nothing at all.
        (let ((prefix (overlay-get me 'line-prefix)))
          (should (or (equal "" prefix) (string-match-p "❯" prefix)))
          (unless (equal "" prefix)
            (should (eq 'default (get-text-property 0 'face prefix)))))))))

(ert-deftest agent-shell-chat-extends-bg-p-test ()
  "`agent-shell-chat--extends-bg-p' recognizes an `:extend' background face."
  (should (agent-shell-chat--extends-bg-p 'agent-shell-chat-mode-tests--panel))
  (should (agent-shell-chat--extends-bg-p '(agent-shell-chat-mode-tests--panel)))
  (should-not (agent-shell-chat--extends-bg-p 'agent-shell-chat-mode-tests--plain))
  (should-not (agent-shell-chat--extends-bg-p nil)))

(ert-deftest agent-shell-chat-prompt-runs-test ()
  "`agent-shell-chat--prompt-runs' collects each prompt run in buffer order."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "hi\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((runs (agent-shell-chat--prompt-runs)))
      (should (= 2 (length runs)))
      (should (< (cdr (nth 0 runs)) (car (nth 1 runs)))))))

(ert-deftest agent-shell-chat-label-after-interrupted-turn-test ()
  "A label after an interrupted turn keeps one blank line before it.
An interrupted turn appends its notice right before the end-of-prompt
marker, so the turn ends mid-line, and its empty response carries no
agent label whose pad would separate the next label."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "do the thing[Request interrupted by user]")
    (agent-shell-chat-mode-tests--marker)
    (insert "\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((before (agent-shell-chat-mode-tests--label-string
                   (car (last (agent-shell-chat-mode-tests--me-overlays))))))
      ;; Ends the interrupted line, then leaves one blank line.
      (should (string-prefix-p "\n\n Me " (substring-no-properties before)))
      (should-not (string-prefix-p "\n\n\n" (substring-no-properties before))))))

(ert-deftest agent-shell-chat-label-after-own-line-marker-test ()
  "A label after a marker that starts its line keeps one blank line before it.
The marker's own line is already empty, so a single newline ends it and
leaves exactly one blank line (a full pad would leave two)."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "do the thing\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((before (substring-no-properties
                   (agent-shell-chat-mode-tests--label-string
                    (car (last (agent-shell-chat-mode-tests--me-overlays)))))))
      (should (string-prefix-p "\n Me " before))
      (should-not (string-prefix-p "\n\n" before)))))

(ert-deftest agent-shell-chat-marker-starts-line-p-test ()
  "The marker is recognized as starting its line only when a newline precedes it."
  (agent-shell-chat-mode-tests--with-shell
    (insert "text\n")
    (agent-shell-chat-mode-tests--marker)
    (should (agent-shell-chat--marker-starts-line-p (point))))
  (agent-shell-chat-mode-tests--with-shell
    (insert "text[Request interrupted by user]")
    (agent-shell-chat-mode-tests--marker)
    (should-not (agent-shell-chat--marker-starts-line-p (point)))))

(ert-deftest agent-shell-chat-label-after-empty-submissions-test ()
  "A label stacked on empty submissions keeps one blank line before it.
An empty submission renders nothing, so it emits no pad to separate the
label below it: that label reuses the lead the hidden run would have
used, which holds however many empty submissions stack up."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "\nreply\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let* ((overlays (agent-shell-chat-mode-tests--me-overlays))
           (before (lambda (overlay)
                     (substring-no-properties
                      (agent-shell-chat-mode-tests--label-string overlay)))))
      (should (= 4 (length overlays)))
      ;; Both empty submissions stay hidden.
      (should (equal "" (funcall before (nth 1 overlays))))
      (should (equal "" (funcall before (nth 2 overlays))))
      ;; The live label still keeps exactly one blank line above it.
      (let ((live (funcall before (nth 3 overlays))))
        (should (string-prefix-p "\n Me " live))
        (should-not (string-prefix-p "\n\n" live))))))

(provide 'agent-shell-chat-mode-tests)
;;; agent-shell-chat-mode-tests.el ends here
