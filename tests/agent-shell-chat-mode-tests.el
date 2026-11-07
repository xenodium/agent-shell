;;; agent-shell-chat-mode-tests.el --- Tests for agent-shell-chat-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-chat-mode)

;;; Code:

;; Declared special so tests can dynamically bind it (the prompt bar need
;; not be loaded); chat-mode reads it with `bound-and-true-p'.
(defvar agent-shell-prompt-bar-mode)

;; Likewise for `agent-shell' itself, which is not loaded here.
(defvar agent-shell-show-busy-indicator)

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

The label's rows are laid over the head of the covered prompt, a row to
each buffer position, so every one is somewhere `window-start' can land.
Joins them back into the single string they render as, so callers assert
what is shown rather than how it is stored.

Falls back to the label carried whole on the newline above, and then to
ME itself where the run has no newline above to carry it."
  (let ((rows (sort (seq-filter
                     (lambda (overlay)
                       (eq (overlay-get overlay 'agent-shell-chat--tag) 'me-label))
                     (overlays-in (save-excursion (goto-char (overlay-start me))
                                                  (line-beginning-position))
                                  (overlay-start me)))
                    (lambda (a b) (< (overlay-start a) (overlay-start b))))))
    (cond (rows (mapconcat #'agent-shell-chat-mode-tests--row-string rows ""))
          ((seq-some (lambda (overlay)
                       (and (eq (overlay-get overlay 'agent-shell-chat--tag) 'me-label)
                            (= (overlay-end overlay) (overlay-start me))
                            (overlay-get overlay 'before-string)))
                     (overlays-in (max (point-min) (1- (overlay-start me)))
                                  (1+ (overlay-start me)))))
          (t (overlay-get me 'before-string)))))

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

(defun agent-shell-chat-mode-tests--row-string (overlay)
  "Return the label row OVERLAY draws, as it renders.
A row is normally a `display', but one holding an image is drawn as a
`before-string' instead (see `agent-shell-chat--row-props')."
  (concat (or (overlay-get overlay 'before-string) "")
          (or (overlay-get overlay 'display) "")))

(defun agent-shell-chat-mode-tests--label-row-p (overlay)
  "Return non-nil when OVERLAY draws a row of a label.
A label is drawn a row to each buffer position, as a `display'.  An
overlay standing in for hidden text (a prompt run, the end-of-prompt
marker) displays \"\" instead, and draws nothing."
  (not (string-empty-p (or (overlay-get overlay 'display) ""))))

(defun agent-shell-chat-mode-tests--agent-label-string (agent)
  "Return the label rendered for AGENT, a response overlay.

The label's rows are laid over the head of the span the overlay covers
the rest of, a row to each buffer position, so every one is somewhere
`window-start' can land.  Joins them back into the string they render as.

Rows carry the response's own tag (so a version drawing the label whole
sweeps them too), and are picked out by the label they draw: a
response's `display' only ever stands in for the hidden marker, and is
empty.

Falls back to AGENT's own `before-string' where the label is carried
whole there instead."
  (let ((rows (sort (seq-filter
                     (lambda (overlay)
                       (and (eq (overlay-get overlay 'agent-shell-chat--tag) 'agent)
                            (agent-shell-chat-mode-tests--label-row-p overlay)))
                     (overlays-in (save-excursion (goto-char (overlay-start agent))
                                                  (line-beginning-position))
                                  (overlay-start agent)))
                    (lambda (a b) (< (overlay-start a) (overlay-start b))))))
    (if rows
        (mapconcat #'agent-shell-chat-mode-tests--row-string rows "")
      (overlay-get agent 'before-string))))

(defun agent-shell-chat-mode-tests--agent-overlays ()
  "Return the response overlays in the current buffer.
Leaves out the label's rows, which share the response's tag (see
`agent-shell-chat-mode-tests--agent-label-string')."
  (seq-filter (lambda (overlay)
                (and (eq (overlay-get overlay 'agent-shell-chat--tag) 'agent)
                     (not (agent-shell-chat-mode-tests--label-row-p overlay))))
              (overlays-in (point-min) (point-max))))

(defun agent-shell-chat-mode-tests--column-of (text)
  "Return the screen column TEXT starts at, or nil when it is not shown.
Read from a real window: the column a line renders at comes from the
`line-prefix' in force where its display row starts, which no buffer
property can be read for on its own."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((found (search-forward text nil t))
                (window (selected-window)))
      (goto-char (- found (length text)))
      (redisplay t)
      (car (posn-col-row (posn-at-point (point) window))))))

(defun agent-shell-chat-mode-tests--turn-labels ()
  "Return the turn labels chat mode draws, in buffer order.
Read a position at a time, as a reader walking the buffer does, so a
label reporting itself more than once shows up as a repeat."
  (let ((found nil)
        (pos (point-min)))
    (while (< pos (point-max))
      (when-let* ((label (agent-shell-chat--turn-label-at pos)))
        (push label found))
      (setq pos (1+ pos)))
    (nreverse found)))

(defun agent-shell-chat-mode-tests--prompt (text)
  "Insert a shell prompt run displaying TEXT, as shell-maker fontifies it."
  (insert (propertize text 'font-lock-face
                      '(comint-highlight-prompt comint-highlight-prompt)
                      'field 'output)))

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

(ert-deftest agent-shell-chat-agent-name-test ()
  "The agent label uses `:mode-line-name', falling back to \"Agent\"."
  (agent-shell-chat-mode-tests--with-shell
    (should (equal "Claude" (agent-shell-chat--agent-name))))
  (with-temp-buffer
    (setq-local agent-shell--state nil)
    (should (equal "Agent" (agent-shell-chat--agent-name)))))

(ert-deftest agent-shell-chat-boxed-label-test ()
  "The default label pads its text and faces it by role."
  (let ((me (agent-shell-chat-boxed-label '((:text . "Me") (:role . user))))
        (agent (agent-shell-chat-boxed-label '((:text . "Claude") (:role . agent)))))
    (should (equal " Me " me))
    (should (eq 'agent-shell-chat-me-label (get-text-property 0 'face me)))
    (should (equal " Claude " agent))
    (should (eq 'agent-shell-chat-agent-label (get-text-property 0 'face agent)))))

(ert-deftest agent-shell-chat-label-function-test ()
  "Labels are drawn by `agent-shell-chat-label-function'."
  (let* ((labels nil)
         (agent-shell-chat-label-function
          (lambda (label)
            (push label labels)
            (format "<%s>" (map-elt label :text)))))
    (agent-shell-chat-mode-tests--with-shell
      (agent-shell-chat-mode-tests--prompt "Claude> ")
      (insert "hello\n")
      (agent-shell-chat-mode-tests--marker)
      (insert "hi there\n")
      (agent-shell-chat--relabel)
      (should (member '((:text . "Me") (:role . user) (:prompt-start . 1)) labels))
      (should (member '((:text . "Claude") (:role . agent) (:prompt-start . 1)) labels))
      (should (string-match-p
               "<Me>" (agent-shell-chat-mode-tests--label-string
                       (car (agent-shell-chat-mode-tests--me-overlays)))))
      (should (string-match-p
               "<Claude>" (agent-shell-chat-mode-tests--agent-label-string
                           (car (agent-shell-chat-mode-tests--agent-overlays))))))))

(ert-deftest agent-shell-chat-label-function-prompt-start-test ()
  "Both labels of a turn are told where its prompt starts; the live prompt is not."
  (let* ((labels nil)
         (agent-shell-chat-label-function
          (lambda (label)
            (push label labels)
            (map-elt label :text))))
    (agent-shell-chat-mode-tests--with-shell
      (agent-shell-chat-mode-tests--prompt "Claude> ")
      (insert "one\n")
      (agent-shell-chat-mode-tests--marker)
      (insert "first\n\n")
      (agent-shell-chat-mode-tests--prompt "Claude> ")
      (insert "two\n")
      (agent-shell-chat-mode-tests--marker)
      (insert "second\n\n")
      (agent-shell-chat-mode-tests--prompt "Claude> ")
      (agent-shell-chat--relabel)
      (should (member '((:text . "Me") (:role . user) (:prompt-start . 1)) labels))
      (should (member '((:text . "Claude") (:role . agent) (:prompt-start . 1)) labels))
      (should (member '((:text . "Me") (:role . user) (:prompt-start . 47)) labels))
      (should (member '((:text . "Claude") (:role . agent) (:prompt-start . 47)) labels))
      ;; The live prompt is labeled, but stands for no turn yet.
      (should (member '((:text . "Me") (:role . user) (:prompt-start . nil)) labels)))))

(ert-deftest agent-shell-chat-row-props-test ()
  "A row holding a `display' is drawn as a `before-string'."
  (should (equal '((display . " Me \n") (before-string . ""))
                 (agent-shell-chat--row-props " Me \n")))
  (let ((row (concat (propertize " Me " 'display '(space :width 4)) "\n")))
    (should (equal "\n"
                   (map-elt (agent-shell-chat--row-props row) 'display)))
    (should (equal " Me "
                   (map-elt (agent-shell-chat--row-props row) 'before-string))))
  (let ((row (propertize " Me " 'display '(space :width 4))))
    (should (equal ""
                   (map-elt (agent-shell-chat--row-props row) 'display)))))

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
      (should (string-match-p
               "Claude" (agent-shell-chat-mode-tests--agent-label-string (car agent)))))))

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
      ;; One newline ends the content line the hidden one could not.  The
      ;; blank line separating the label is the newline the rows are laid
      ;; past, so the label closes with a blank line of its own instead.
      (should (string-prefix-p "\n Me " before))
      (should-not (string-prefix-p "\n\n" before))
      (should (string-suffix-p "\n\n" before)))))

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
its first line.  The label is drawn over the covered prompt instead, and
the marker with it; a `display' stands on buffer positions rather than
adding any, leaving the input reachable."
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
      ;; The label is drawn on the prompt this overlay covers the rest of,
      ;; a row to each position, so it starts past those rows.
      (should (string-match-p "Me" (agent-shell-chat-mode-tests--label-string me)))
      (should (< (save-excursion (goto-char (overlay-start me))
                                 (line-beginning-position))
                 (overlay-start me))))))

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

(ert-deftest agent-shell-chat-heals-label-drawn-whole-test ()
  "A label an earlier version drew whole is cleared, not drawn beside.

Labels used to be drawn as one multi-row `before-string' and are now
drawn a row to each buffer position, as a `display'.  Overlays are
reused by tag and keep every property a relabel does not write, so the
string a rolled-forward (or rolled-back) shell carries would otherwise
go on rendering next to the rows laid over it."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "typed\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (agent-shell-chat--relabel)
    ;; Stand in for the shape the earlier version left behind: the label
    ;; carried whole on a `before-string', with no `display' row.
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (memq (overlay-get overlay 'agent-shell-chat--tag) '(me-label agent))
        (overlay-put overlay 'display nil)
        (overlay-put overlay 'before-string "\n STALE \n\n")))
    (agent-shell-chat--relabel)
    (should-not (seq-find (lambda (overlay)
                            (string-match-p
                             "STALE" (or (overlay-get overlay 'before-string) "")))
                          (overlays-in (point-min) (point-max))))
    ;; Each label is back to being drawn once, as rows.
    (should (string-match-p "Me" (agent-shell-chat-mode-tests--label-string
                                  (car (agent-shell-chat-mode-tests--me-overlays)))))
    (should (string-match-p "Claude" (agent-shell-chat-mode-tests--agent-label-string
                                      (car (agent-shell-chat-mode-tests--agent-overlays)))))))

(ert-deftest agent-shell-chat-labels-tagged-for-earlier-sweeps-test ()
  "Every label overlay carries a tag the version before this one sweeps.

Rolling the package back leaves that version relabeling: it reuses one
overlay per span and sweeps the rest by tag.  A tag it never heard of
survives its sweep, its relabel and even turning the mode off, drawing
the label a second time for good.  The label rows are tagged as the
overlay they were split out of is, so they are swept either way."
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
      (should (memq (overlay-get overlay 'agent-shell-chat--tag)
                    ;; What the version before this one sweeps.
                    '(me me-label me-surplus me-input me-draft agent))))))

(ert-deftest agent-shell-chat-response-first-line-indented-test ()
  "The response's first line carries the indent the rest of the body has.

That line starts its display row inside the overlay standing in for the
hidden marker, where the label's last row left off, so it takes its
prefix from that overlay rather than from the body text it runs into.
Without one it renders flush left while the response below it is
indented."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "typed\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "\n")
    ;; The response, indented as `agent-shell-ui--indent-text' indents it.
    (insert (propertize "reply\nsecond line\n"
                        'line-prefix "  " 'wrap-prefix "  "))
    (agent-shell-chat--relabel)
    (let ((agent (car (agent-shell-chat-mode-tests--agent-overlays))))
      (should (equal "  " (overlay-get agent 'line-prefix)))
      (should (equal "  " (overlay-get agent 'wrap-prefix))))))

(ert-deftest agent-shell-chat-response-first-line-column-test ()
  "The response's first line renders at the column the rest of it does.

Needs a real window to measure, so it is skipped in batch."
  (skip-unless (not noninteractive))
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "typed\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "\n")
    (insert (propertize "reply\nsecond line\n"
                        'line-prefix "  " 'wrap-prefix "  "))
    (agent-shell-chat--relabel)
    (let ((window (selected-window)))
      (set-window-buffer window (current-buffer))
      (set-window-start window (point-min))
      (should (= (agent-shell-chat-mode-tests--column-of "second line")
                 (agent-shell-chat-mode-tests--column-of "reply"))))))

(ert-deftest agent-shell-chat-draft-indent-clears-marker-test ()
  "A draft's later lines are indented past the marker its first line follows.

The first line shares the prompt's row and starts after the marker drawn
there, so an indent of the body's width alone would leave every line
below it two columns to the left of the line it continues (see #806)."
  (should (= (string-width (agent-shell-chat--draft-indent))
             (+ (string-width agent-shell-chat--body-indent)
                (string-width agent-shell-chat--prompt))))
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "first line\nsecond line")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((draft (agent-shell-chat-mode-tests--draft-overlay)))
      (should draft)
      (should (equal (agent-shell-chat--draft-indent)
                     (overlay-get draft 'line-prefix)))
      (should (equal (agent-shell-chat--draft-indent)
                     (overlay-get draft 'wrap-prefix))))))

(ert-deftest agent-shell-chat-draft-lines-share-a-column-test ()
  "Every line of a draft renders at the column its first line starts at.

Needs a real window to measure, so it is skipped in batch."
  (skip-unless (not noninteractive))
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "typed\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "\n")
    (insert (propertize "reply\n" 'line-prefix "  " 'wrap-prefix "  "))
    (insert "\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "first line\nsecond line")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((window (selected-window)))
      (set-window-buffer window (current-buffer))
      (set-window-start window (point-min))
      (should (= (agent-shell-chat-mode-tests--column-of "first line")
                 (agent-shell-chat-mode-tests--column-of "second line"))))))

(ert-deftest agent-shell-chat-turn-label-at-reads-drawn-rows-test ()
  "Each turn reports its label once, however the label is drawn.

Copied turns are headed from this (see
`agent-shell--buffer-markdown-substring'), so a label drawn a row to
each buffer position has to report itself, and report itself once: the
rows padding it out are blank, and the live prompt's marker is a glyph
to type at rather than a name."
  (agent-shell-chat-mode-tests--with-shell
    (setq-local agent-shell-chat-mode t)
    (dotimes (i 2)
      (agent-shell-chat-mode-tests--prompt "Claude> ")
      (insert (format "question %d\n" i))
      (agent-shell-chat-mode-tests--marker)
      (insert (format "\nreply %d\n\n" i)))
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (should (equal '("Me" "Claude" "Me" "Claude" "Me")
                   (agent-shell-chat-mode-tests--turn-labels)))))

(ert-deftest agent-shell-chat-labels-response-with-no-room-test ()
  "A response the marker leaves no room before is labeled, not an error.

A restored turn writes \"input<marker>\\n\", so a label's span can come
out empty.  An `evaporate' overlay is deleted the moment it is empty,
which would leave relabeling holding an overlay with no buffer."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "typed")
    (agent-shell-chat-mode-tests--marker)
    (insert "\nreply\n")
    (agent-shell-chat--relabel)
    (let ((agent (car (agent-shell-chat-mode-tests--agent-overlays))))
      (should (overlay-buffer agent))
      (should (string-match-p
               "Claude" (agent-shell-chat-mode-tests--agent-label-string agent))))
    ;; Relabeling again neither errors nor draws the label a second time.
    (agent-shell-chat--relabel)
    (should (= 1 (length (agent-shell-chat-mode-tests--agent-overlays))))))

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

(ert-deftest agent-shell-chat-relabels-after-fragment-test ()
  "A fragment rendered above the live prompt shows without an event.

It lands at the prompt's start, inside the label overlay starting there,
whose `display' draws the label in its place.  A notice like a declined
steer fires no event, so it stayed hidden until something else relabeled
\(agent-shell-js#47)."
  (with-temp-buffer
    (setq-local agent-shell--state '((:agent-config . ((:mode-line-name . "Claude"))))
                agent-shell-chat-mode t)
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil)
          (shown (lambda ()
                   (agent-shell-chat--displayed-substring (point-min) (point-max)))))
      (cl-letf (((symbol-function 'agent-shell-subscribe-to) #'ignore)
                ((symbol-function 'agent-shell-unsubscribe) #'ignore))
        (agent-shell-chat--enable)
        (save-excursion
          (goto-char (- (point-max) (length "Claude> ")))
          (insert "Note: declined.\n\n"))
        (should-not (string-match-p "declined" (funcall shown)))
        (run-hook-with-args 'agent-shell-section-functions nil)
        (should (timerp agent-shell-chat--relabel-timer))
        (cancel-timer agent-shell-chat--relabel-timer)
        (agent-shell-chat--relabel-buffer (current-buffer))
        (should (string-match-p "declined" (funcall shown)))
        (agent-shell-chat--disable)))))

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

(defun agent-shell-chat-mode-tests--beat (status value)
  "Set the shell's heartbeat to STATUS at VALUE, as a heartbeat tick does."
  (setf (map-elt agent-shell--state :heartbeat)
        (list (cons :status status) (cons :value value))))

(defun agent-shell-chat-mode-tests--busy-marker (beat)
  "Return the live marker as drawn on BEAT while busy.
Read from `agent-shell-prompt-busy-frames', so the frames can change
without these tests.

For example, on beat 1 returns \"/ ❯ \"."
  (concat (seq-elt agent-shell-prompt-busy-frames beat) " " agent-shell-chat--prompt))

(ert-deftest agent-shell-chat-live-marker-animates-while-busy-test ()
  "A busy agent animates the live prompt's marker, a frame per beat.
Drawn as the covered prompt's `display', where the label rides the newline
above, and back to the plain marker once the heartbeat ends."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil)
          (agent-shell-show-busy-indicator t))
      (agent-shell-chat-mode-tests--beat 'busy 0)
      (agent-shell-chat--relabel)
      (let ((me (car (last (agent-shell-chat-mode-tests--me-overlays)))))
        (should (equal (overlay-get me 'display)
                       (agent-shell-chat-mode-tests--busy-marker 0)))
        (agent-shell-chat-mode-tests--beat 'busy 1)
        (agent-shell-chat--animate-live-marker)
        (should (equal (overlay-get me 'display)
                       (agent-shell-chat-mode-tests--busy-marker 1)))
        (agent-shell-chat-mode-tests--beat 'ended nil)
        (agent-shell-chat--animate-live-marker)
        (should (equal (overlay-get me 'display) "  ❯ "))))))

(ert-deftest agent-shell-chat-live-marker-animates-beside-label-test ()
  "The marker animates where it is drawn after the `Me' label, too.
With no newline above to ride, the label and marker share the prompt's
`before-string', and a tick rewrites the marker alone."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil)
          (agent-shell-show-busy-indicator t))
      (agent-shell-chat-mode-tests--beat 'busy 0)
      (agent-shell-chat--relabel)
      (let ((me (car (agent-shell-chat-mode-tests--me-overlays))))
        (should (string-suffix-p (concat "\n" (agent-shell-chat-mode-tests--busy-marker 0))
                                 (overlay-get me 'before-string)))
        (agent-shell-chat-mode-tests--beat 'busy 1)
        (agent-shell-chat--animate-live-marker)
        (should (string-suffix-p (concat " Me \n\n" (agent-shell-chat-mode-tests--busy-marker 1))
                                 (overlay-get me 'before-string)))))))

(ert-deftest agent-shell-chat-live-marker-aligns-past-frame-test ()
  "The space after a busy frame aligns the marker to the body indent.
A frame's glyph can come from a fallback font wider than a column, which
would otherwise push the marker right."
  (agent-shell-chat-mode-tests--with-shell
    (let ((agent-shell-show-busy-indicator t))
      (agent-shell-chat-mode-tests--beat 'busy 0)
      (should (equal (get-text-property 1 'display (agent-shell-chat--live-marker))
                     '(space :align-to (2 . width)))))))

(ert-deftest agent-shell-prompt-busy-frames-accepts-list-test ()
  "Busy frames can be set as a list as well as a vector."
  (agent-shell-chat-mode-tests--with-shell
    (let ((agent-shell-show-busy-indicator t)
          (agent-shell-prompt-busy-frames '("a" "b" "c")))
      (agent-shell-chat-mode-tests--beat 'busy 4)
      (should (equal (agent-shell-chat--busy-frame) "b")))))

(ert-deftest agent-shell-prompt-busy-frames-accepts-string-test ()
  "Busy frames set as a string show as is on every beat."
  (agent-shell-chat-mode-tests--with-shell
    (let ((agent-shell-show-busy-indicator t)
          (agent-shell-prompt-busy-frames "busy"))
      (agent-shell-chat-mode-tests--beat 'busy 0)
      (should (equal (agent-shell-chat--busy-frame) "busy"))
      (agent-shell-chat-mode-tests--beat 'busy 3)
      (should (equal (agent-shell-chat--busy-frame) "busy")))))

(ert-deftest agent-shell-prompt-busy-frames-accepts-function-test ()
  "Busy frames can come from a function, returning a string or frames."
  (agent-shell-chat-mode-tests--with-shell
    (let ((agent-shell-show-busy-indicator t)
          (agent-shell-prompt-busy-frames (lambda () "connecting")))
      (agent-shell-chat-mode-tests--beat 'busy 3)
      (should (equal (agent-shell-chat--busy-frame) "connecting"))
      (setq agent-shell-prompt-busy-frames (lambda () '("a" "b")))
      (should (equal (agent-shell-chat--busy-frame) "b"))
      (setq agent-shell-prompt-busy-frames #'ignore)
      (should-not (agent-shell-chat--busy-frame)))))

(ert-deftest agent-shell-prompt-busy-frames-accepts-empty-test ()
  "Empty busy frames show nothing, rather than dividing by zero each beat."
  (agent-shell-chat-mode-tests--with-shell
    (let ((agent-shell-show-busy-indicator t))
      (agent-shell-chat-mode-tests--beat 'busy 3)
      (dolist (frames (list [] "" (lambda () [])))
        (let ((agent-shell-prompt-busy-frames frames))
          (should-not (agent-shell-chat--busy-frame)))))))

(ert-deftest agent-shell-chat-live-marker-spaces-wide-frame-test ()
  "A frame wider than a column is followed by a plain space, not aligned.
It can't fit the body indent, so aligning would leave no space before
the marker."
  (agent-shell-chat-mode-tests--with-shell
    (let ((agent-shell-show-busy-indicator t)
          (agent-shell-prompt-busy-frames "busy"))
      (agent-shell-chat-mode-tests--beat 'busy 0)
      (let ((marker (agent-shell-chat--live-marker)))
        (should (equal (substring-no-properties marker) "busy ❯ "))
        (should-not (get-text-property 4 'display marker))))))

(ert-deftest agent-shell-chat-live-marker-frame-faced-secondary-test ()
  "A busy frame is faced `agent-shell-secondary', the marker `default'.
The frame recedes behind the marker, which stays as it is when idle."
  (agent-shell-chat-mode-tests--with-shell
    (let ((agent-shell-show-busy-indicator t))
      (agent-shell-chat-mode-tests--beat 'busy 0)
      (let ((marker (agent-shell-chat--live-marker)))
        (should (eq (get-text-property 0 'face marker) 'agent-shell-secondary))
        (should (eq (get-text-property (string-match "❯" marker) 'face marker)
                    'default))))))

(ert-deftest agent-shell-chat-submitted-prompt-stops-animating-test ()
  "Only the live prompt animates: submitting it leaves no marker to redraw."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil)
          (agent-shell-show-busy-indicator t))
      (agent-shell-chat-mode-tests--beat 'busy 0)
      (agent-shell-chat--relabel)
      ;; Animated once, so the live prompt's overlay is cached.
      (agent-shell-chat--animate-live-marker)
      (insert "hello\n")
      (agent-shell-chat-mode-tests--marker)
      (agent-shell-chat--relabel)
      (should-not (seq-find (lambda (overlay)
                              (overlay-get overlay 'agent-shell-chat--marker-head))
                            (overlays-in (point-min) (point-max))))
      (should-not (agent-shell-chat--animate-live-marker)))))

(ert-deftest agent-shell-chat-live-marker-follows-text-above-test ()
  "The live marker keeps animating as text above it comes and goes.
The overlay drawing it is cached between ticks, and moves with the text
around it rather than staying at the position it was found at."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil)
          (agent-shell-show-busy-indicator t))
      (agent-shell-chat-mode-tests--beat 'busy 0)
      (agent-shell-chat--relabel)
      (agent-shell-chat--animate-live-marker)
      (let ((me (car (last (agent-shell-chat-mode-tests--me-overlays)))))
        ;; Output arriving above the prompt, and some of it removed again.
        (save-excursion
          (goto-char (point-min))
          (insert "streamed above\n"))
        (save-excursion
          (goto-char (point-min))
          (delete-char 9))
        (agent-shell-chat-mode-tests--beat 'busy 1)
        (agent-shell-chat--animate-live-marker)
        (should (equal (overlay-get me 'display)
                       (agent-shell-chat-mode-tests--busy-marker 1)))
        ;; Still spanning the tail of the live prompt it was drawn over.
        (should (string-suffix-p (buffer-substring-no-properties
                                  (overlay-start me) (overlay-end me))
                                 "Claude> "))))))

(ert-deftest agent-shell-chat-live-marker-moves-to-next-prompt-test ()
  "Once a prompt is submitted, the next live prompt's marker animates.
The cached overlay belongs to the submitted turn by then, and is dropped
for the new prompt's."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil)
          (agent-shell-show-busy-indicator t))
      (agent-shell-chat-mode-tests--beat 'busy 0)
      (agent-shell-chat--relabel)
      (agent-shell-chat--animate-live-marker)
      (let ((submitted (car (agent-shell-chat-mode-tests--me-overlays))))
        (insert "hello\n")
        (agent-shell-chat-mode-tests--marker)
        (insert "reply\n\n")
        (agent-shell-chat-mode-tests--prompt "Claude> ")
        (agent-shell-chat--relabel)
        (agent-shell-chat-mode-tests--beat 'busy 1)
        (agent-shell-chat--animate-live-marker)
        (let ((live (car (last (agent-shell-chat-mode-tests--me-overlays)))))
          (should-not (eq live submitted))
          (should (equal (overlay-get live 'display)
                         (agent-shell-chat-mode-tests--busy-marker 1)))
          (should-not (string-match-p "❯" (or (overlay-get submitted 'display) ""))))))))

(ert-deftest agent-shell-chat-submitted-input-first-line-indented-test ()
  "A submitted turn's first input line lines up with the rest of it.

That line is shared with the prompt, which is covered, so its indent has
to come from the prompt's own overlay.  `line-prefix' is read where a
display row starts, and the label's rows take the head of the covered
prompt, so the input's row starts where that overlay does."
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
                         (overlay-start
                          (car (last (agent-shell-chat-mode-tests--me-overlays))))
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
  "The live prompt's draft carries the draft indent below its first line.
The marker indents the first line; this indents the lines under it to the
same column, which the overlay covering the prompt text cannot reach.  It is in place before
anything is typed and takes in what follows, since no relabel runs per
keystroke."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((draft (agent-shell-chat-mode-tests--draft-overlay)))
      (should draft)
      (should (equal (agent-shell-chat--draft-indent)
                     (overlay-get draft 'line-prefix)))
      (should (equal (agent-shell-chat--draft-indent)
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
      (should (equal (agent-shell-chat--draft-indent)
                     (overlay-get draft 'after-string)))
      ;; The first character of the new line takes over.
      (insert "t")
      (should (equal "" (overlay-get draft 'after-string)))
      ;; Deleting back to the empty line brings it back.
      (delete-char -1)
      (should (equal (agent-shell-chat--draft-indent)
                     (overlay-get draft 'after-string)))
      ;; A relabel mid-draft leaves it alone.
      (let ((agent-shell-prompt-bar-mode nil))
        (agent-shell-chat--relabel))
      (should (equal (agent-shell-chat--draft-indent)
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

(ert-deftest agent-shell-chat-draft-reindent-survives-narrowing-test ()
  "The draft re-indents itself under a caller's narrowing.
Its hooks fire during whatever edit provoked them, and a caller rendering
the response above the prompt narrows to end before it (see
`agent-shell--update-fragment') while the draft rear-advances toward end
of buffer.  Reading the draft's bounds under that restriction signals
`args-out-of-range' unless they are widened back first."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (goto-char (point-max))
    (insert "typed\n")
    (let ((draft (agent-shell-chat-mode-tests--draft-overlay)))
      (should draft)
      (save-restriction
        ;; Narrowed to end where the draft begins, leaving the draft's own
        ;; end outside it, as a caller rendering above the prompt does.
        (narrow-to-region (point-min) (overlay-start draft))
        (goto-char (point-max))
        (insert "streamed"))
      ;; Still covering everything typed, and still indenting the empty
      ;; last line the newline left.
      (should (= (overlay-end draft) (point-max)))
      (should (equal (agent-shell-chat--draft-indent)
                     (overlay-get draft 'after-string))))))

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

(ert-deftest agent-shell-chat-labels-scroll-one-line-at-a-time-test ()
  "Scrolling a line at a time advances past every label.

A label drawn as one multi-row string hangs all its rows off the single
buffer position it is attached to.  `window-start' can only ever be a
buffer position, so a scroll of fewer rows than the string spans has
nowhere to land and stops advancing for good.  Drawing a row to each
position keeps every one reachable.

Needs a real window to scroll, so it is skipped in batch."
  (skip-unless (not noninteractive))
  (agent-shell-chat-mode-tests--with-shell
    (dotimes (i 40)
      (agent-shell-chat-mode-tests--prompt "Claude> ")
      (insert (format "question %d\n" i))
      (agent-shell-chat-mode-tests--marker)
      (insert (format "\nreply %d\n\n" i)))
    (agent-shell-chat--relabel)
    (let ((window (selected-window)))
      (set-window-buffer window (current-buffer))
      (goto-char (point-min))
      (set-window-start window (point-min))
      (redisplay t)
      (let ((last -1) (stalled nil) (n 0))
        (while (and (< n 400) (not stalled)
                    (not (pos-visible-in-window-p (point-max) window)))
          (ignore-errors (scroll-up-command 1))
          (redisplay t)
          (when (= (window-start window) last)
            (setq stalled (line-number-at-pos (window-start window))))
          (setq last (window-start window) n (1+ n)))
        (should-not stalled)))))

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
      ;; Ends the interrupted line.  The blank line above is the newline
      ;; the rows are laid past, so the label closes with one of its own.
      (should (string-prefix-p "\n Me " (substring-no-properties before)))
      (should-not (string-prefix-p "\n\n" (substring-no-properties before)))
      (should (string-suffix-p "\n\n" (substring-no-properties before))))))

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
      ;; The marker's line is already empty and the rows are laid past the
      ;; newline closing it, so the label opens straight onto its own row.
      (should (string-prefix-p " Me " before))
      (should-not (string-prefix-p "\n" before))
      (should (string-suffix-p "\n\n" before)))))

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
      ;; The live label still keeps exactly one blank line above it, which
      ;; is the newline its rows are laid past; it closes with one of its own.
      (let ((live (funcall before (nth 3 overlays))))
        (should (string-prefix-p " Me " live))
        (should-not (string-prefix-p "\n" live))
        (should (string-suffix-p "\n\n" live))))))

(provide 'agent-shell-chat-mode-tests)
;;; agent-shell-chat-mode-tests.el ends here
