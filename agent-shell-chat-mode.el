;;; agent-shell-chat-mode.el --- Chat-style labels for agent-shell. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

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
;; `agent-shell-chat-mode' relabels the shell so it reads like a chat:
;; each submitted user turn is boxed `Me' and each response is boxed with
;; the agent's name.  Labels are overlays, so the buffer text is untouched
;; (a `display' overlay replaces the visible comint prompt for `Me', and a
;; `before-string' overlay renders the agent label at the invisible
;; `<shell-maker-end-of-prompt>' marker).
;;
;; The live prompt awaiting input shows `Me' too, so you can type straight
;; into the shell.  When `agent-shell-prompt-bar-mode' is enabled, input
;; flows through that bar instead, so the live prompt is hidden.
;;
;; Toggle it with `M-x agent-shell-chat-mode'.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'map)
(require 'seq)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defvar agent-shell-prompt-queue-setup-minibuffer-functions)

(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")

(defvar agent-shell--state)
;; Soft reference: `agent-shell-prompt-bar-mode' may be unbound when the
;; prompt bar is not loaded.  Read it with `bound-and-true-p'.
(defvar agent-shell-prompt-bar-mode)

;; Forward-declared: used before the `define-minor-mode' at the end.
(defvar agent-shell-chat-mode)

;;; Customization

(defcustom agent-shell-chat-mode-enabled t
  "Whether a new agent shell enables `agent-shell-chat-mode' by default.
When non-nil, starting a shell turns the (global) chat mode on, so that
shell and any others render as a chat.  Toggling the mode off by hand is
overridden the next time a shell starts."
  :type 'boolean
  :group 'agent-shell)

;;; Constants

(defconst agent-shell-chat--prompt "❯ "
  "Prompt marker shown on the live shell prompt while it awaits input.
Cleared the instant the prompt is submitted, since the overlay then
renders the submitted turn instead.")

(defconst agent-shell-chat--body-indent "  "
  "Indent that lines the prompt input up with the response body.
Mirrors the two-column base `line-prefix' the response carries (see
`agent-shell-ui--indent-text'); hiding the comint prompt would otherwise
drop the input flush to column 0.")

;;; Faces

(defface agent-shell-chat-me-label
  '((t :inherit (bold font-lock-keyword-face) :inverse-video t :box t))
  "Face for the user (\"Me\") chat label.
`:inverse-video' fills the badge with the foreground color (text inverts
to the background); `:box' t adds a border in the foreground color."
  :group 'agent-shell)

(defface agent-shell-chat-agent-label
  '((t :inherit (bold font-lock-function-name-face) :inverse-video t :box t))
  "Face for the agent chat label.
`:inverse-video' fills the badge with the foreground color (text inverts
to the background); `:box' t adds a border in the foreground color."
  :group 'agent-shell)

;;; State

(defvar-local agent-shell-chat--labeled nil
  "Non-nil once chat labels have been applied to this shell buffer.")

(defvar-local agent-shell-chat--subscription nil
  "Event subscription token keeping chat labels in sync, or nil.")

(defvar-local agent-shell-chat--relabel-timer nil
  "Pending coalesced relabel timer for this buffer, or nil.")

;;; Labels

(defun agent-shell-chat--label (text face)
  "Return TEXT padded and propertized with FACE, as a chat label.

FACE carries the box (see `agent-shell-chat-me-label').

For example, (agent-shell-chat--label \"Me\" \\='agent-shell-chat-me-label)
returns \" Me \" in that face."
  (propertize (format " %s " text) 'face face))

(defun agent-shell-chat--agent-name ()
  "Return the attached agent's display name for the response label.

For example, with a mode-line name of \"Claude\" returns \"Claude\";
with none available, returns \"Agent\"."
  (or (map-nested-elt agent-shell--state '(:agent-config :mode-line-name))
      "Agent"))

(defun agent-shell-chat--prompt-face-p (value)
  "Return non-nil when a `font-lock-face' VALUE marks a shell prompt.
A live prompt carries `comint-highlight-prompt'; a restored or echoed
prompt carries `agent-shell-prompt' (which inherits it).  Either may be
repeated.

For example, \\='comint-highlight-prompt, \\='agent-shell-prompt, and
\\='(comint-highlight-prompt comint-highlight-prompt) all return non-nil,
while \\='default returns nil."
  (let ((faces (if (listp value) value (list value))))
    (or (memq 'comint-highlight-prompt faces)
        (memq 'agent-shell-prompt faces))))

(defun agent-shell-chat--extends-bg-p (face)
  "Return non-nil when FACE paints an `:extend' background past end of line.
FACE is a `face' text-property value (a face symbol or list of them).  A
code block's padding carries such a face (`agent-shell-markdown-source-block'),
so this marks whitespace the prompt overlay must not swallow."
  (seq-some (lambda (f)
              (and (facep f) (eq (face-attribute f :extend nil t) t)))
            (if (proper-list-p face) face (list face))))

(defun agent-shell-chat--marker-starts-line-p (pos)
  "Return non-nil when the end-of-prompt marker ending at POS starts its line.
POS is just past the marker's last character.  An interrupted turn
appends its notice right before the marker, leaving the marker mid-line,
so a label following it needs a full pad rather than a single newline."
  (let ((beg pos))
    (while (and (> beg (point-min))
                (get-text-property (1- beg) 'shell-maker--marker))
      (setq beg (1- beg)))
    (or (= beg (point-min))
        (eq (char-before beg) ?\n))))

(cl-defun agent-shell-chat--ensure-overlay
    (&key tag beg end props front-advance rear-advance
          (anchor-beg beg) (anchor-end end))
  "Ensure a TAG overlay spans BEG..END carrying PROPS.

TAG is a symbol naming what the overlay is for (`me', `agent' and so
on), held in an `agent-shell-chat--tag' property of its own rather
than in `category': a `category' hands redisplay every property its
symbol carries, so a value that also names a face lends its internal face
id and floods `*Messages*' with \"Invalid face reference\".  A property
of our own carries nothing.

PROPS is an alist of overlay property to value.  Reuses an existing TAG
overlay overlapping ANCHOR-BEG..ANCHOR-END (moving it when the span
changed), otherwise creates one.  Reusing, and writing only what
changed, leaves an unchanged buffer untouched: relabeling runs on every
agent event, and each overlay write dirties its span for redisplay.

ANCHOR-BEG..ANCHOR-END default to the span, and are widened only where
an overlay is expected to sit somewhere its span no longer covers.

FRONT-ADVANCE and REAR-ADVANCE control whether text inserted at the
corresponding boundary remains outside or enters the overlay.  Live
prompt decoration uses FRONT-ADVANCE so fragments inserted immediately
before it do not become prompt chrome or disappear behind its `display'
properties.  With REAR-ADVANCE non-nil
the overlay takes in text inserted at its end and is not `evaporate'd
while empty, so it can hold properties over an input still being typed:
relabeling is event-driven, so an overlay that stopped at the caret
would never grow to cover what follows it."
  (let ((overlay (or (seq-find (lambda (overlay)
                                 (eq (overlay-get overlay 'agent-shell-chat--tag) tag))
                               (overlays-in anchor-beg (max anchor-end (1+ anchor-beg))))
                     (let ((created
                            (make-overlay beg end nil
                                          front-advance rear-advance)))
                       (overlay-put created 'agent-shell-chat--tag tag)
                       (unless rear-advance
                         (overlay-put created 'evaporate t))
                       created))))
    (unless (and (= (overlay-start overlay) beg) (= (overlay-end overlay) end))
      (move-overlay overlay beg end))
    (map-do (lambda (property value)
              (unless (equal (overlay-get overlay property) value)
                (overlay-put overlay property value)))
            props)
    overlay))

(defun agent-shell-chat--displayed-substring (start end)
  "Return what the buffer shows between START and END, chat overlays applied.

Chat mode hides the shell prompt and shell-maker's marker behind overlay
`display', and draws its \"Me\"/agent labels with `before-string', so the
buffer text and what the user sees disagree.  A copy should follow the
screen, and `buffer-substring' alone cannot: overlays are not text.

Only chat mode's own overlays are substituted, found by the
`agent-shell-chat--tag' they already carry.  That keeps this out of the
business of resolving overlapping overlays by priority, and means an
image `display' put here by anything else is never stringified.

`after-string' is left out: chat mode uses it for draft indentation,
which is layout padding like the `line-prefix' a copy already drops.

Returns `buffer-substring' unchanged when chat mode is off.

For example, over a labelled turn whose buffer text is \"Claude> hi\"
but which shows

  Me

  hi

returns \"Me\\n\\nhi\"."
  (if (bound-and-true-p agent-shell-chat-mode)
      (let ((pieces nil)
            (pos start))
        (while (< pos end)
          (let* ((overlay (seq-find (lambda (candidate)
                                      (and (overlay-get candidate 'agent-shell-chat--tag)
                                           (= (overlay-start candidate) pos)))
                                    (overlays-in pos (min end (1+ pos)))))
                 (display (and overlay (overlay-get overlay 'display)))
                 (next (min end (next-overlay-change pos))))
            (when-let* ((before (and overlay (overlay-get overlay 'before-string))))
              (push before pieces))
            (cond ((stringp display)
                   (push display pieces)
                   ;; Skip what the display stands in for, inner overlays
                   ;; included.  `next' guarantees progress on an empty overlay.
                   (setq pos (max next (min end (overlay-end overlay)))))
                  (t
                   (push (buffer-substring pos next) pieces)
                   (setq pos next)))))
        (mapconcat #'identity (nreverse pieces)))
    (buffer-substring start end)))

(defun agent-shell-chat--tagged-overlays-at (position)
  "Return chat mode's own overlays starting at POSITION.
Found by the `agent-shell-chat--tag' they carry, so overlays put here by
anything else are left out."
  (when (bound-and-true-p agent-shell-chat-mode)
    (seq-filter (lambda (candidate)
                  (and (overlay-get candidate 'agent-shell-chat--tag)
                       (= (overlay-start candidate) position)))
                (overlays-in position (1+ position)))))

(defun agent-shell-chat--turn-label-at (position)
  "Return the turn label chat mode draws at POSITION, or nil for none.

Read from the overlay's `agent-shell-chat--tag' rather than its
`before-string', which wraps the name in layout (blank lines, the prompt
glyph).  An overlay drawing no label carries an empty `before-string'
rather than none, so emptiness is what rules it out: counting it would
label a turn twice.

For example, at a submitted turn returns \"Me\"."
  (when-let* ((labelled (seq-find
                         (lambda (candidate)
                           (and (memq (overlay-get candidate 'agent-shell-chat--tag)
                                      '(me me-label agent))
                                (not (string-empty-p
                                      (or (overlay-get candidate 'before-string) "")))))
                         (agent-shell-chat--tagged-overlays-at position))))
    (if (eq (overlay-get labelled 'agent-shell-chat--tag) 'agent)
        (agent-shell-chat--agent-name)
      "Me")))

(defun agent-shell-chat--hidden-range-at (position)
  "Return the range chat mode draws over at POSITION, or nil for none.

The text is hidden by an overlay `display' standing in its place, which
is why a reader must not take it: it is not on screen.

Keys `:start' and `:end' bound the hidden text, `:end' exclusive as in
`buffer-substring', so a reader resumes there.

For example, over a buffer reading \"Claude> question\" whose prompt is
drawn over:

  ((:start . 1) (:end . 9))

putting the resume point at the \"q\" of \"question\"."
  (when-let* ((hiding (seq-find (lambda (candidate)
                                  (stringp (overlay-get candidate 'display)))
                                (agent-shell-chat--tagged-overlays-at position))))
    (list (cons :start (overlay-start hiding))
          (cons :end (overlay-end hiding)))))

(defun agent-shell-chat--draft-tail-indent (beg end)
  "Return the indent a draft spanning BEG..END needs on its last line.

A `line-prefix' hangs off the character a display row starts from, and a
draft ending in a newline has none there: that row starts at end of
buffer.  The caret would sit flush left until the first character landed
to carry the prefix.  A string standing at that position indents the row
instead, and gives way (to \"\") the moment there is a character for the
prefix itself.

For example, over a draft of \"one\\n\" returns the body indent, and over
\"one\" returns \"\"."
  (if (and (> end beg) (eq (char-before end) ?\n))
      agent-shell-chat--body-indent
    ""))

(defun agent-shell-chat--draft-changed (draft after &rest _)
  "Re-indent DRAFT's last line once a change to it has landed.

Runs from DRAFT's own modification hooks, AFTER being non-nil once the
change is in.  Kept off the relabel path: relabeling is event-driven and
coalesced, so none runs between the newline that empties the last line
and the character that fills it.

Stops DRAFT at the end-of-prompt marker once the turn is submitted.  The
marker and the response that follows it both arrive at end of buffer,
which a rear-advancing overlay takes in, indenting the response as though
it were still being typed until the next relabel drops the overlay.

For example, over a DRAFT covering \"one\\n\" sets its `after-string' to
the body indent, and over \"one\" sets it to \"\".  Once a submission has
made that \"one\\n<marker>\", DRAFT is left ending before the marker, with
an `after-string' of \"\"."
  (when after
    (let* ((submitted (text-property-any (overlay-start draft)
                                         (overlay-end draft)
                                         'shell-maker--marker t))
           (indent (if submitted
                       ""
                     (agent-shell-chat--draft-tail-indent
                      (overlay-start draft) (overlay-end draft)))))
      (when submitted
        (move-overlay draft (overlay-start draft) submitted))
      (unless (equal (overlay-get draft 'after-string) indent)
        (overlay-put draft 'after-string indent)))))

(defun agent-shell-chat--search-marker-forward ()
  "Search forward for shell-maker's end-of-prompt marker.
Returns non-nil when one is found, leaving point and the match data where
`re-search-forward' does, so callers read the bounds from the match.

Only shell-maker's own marker counts, identified by the
`shell-maker--marker' property it carries.  An agent quoting
\"<shell-maker-end-of-prompt>\" back in its response writes the same
characters without that property, and labeling those would open a second
response inside the one being read.

For example, over a propertized marker returns non-nil with point just
past it, and over the same text unpropertized returns nil."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward "<shell-maker-end-of-prompt>" nil t))
      (setq found (get-text-property (match-beginning 0) 'shell-maker--marker)))
    found))

(defun agent-shell-chat--gc-overlays (tags kept)
  "Delete label overlays of TAGS not in KEPT (a list of overlays).
Removes stale labels whose prompt run or marker was deleted (e.g. a live
prompt a `session/push' removed), which relabeling would not otherwise
reach."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (and (memq (overlay-get overlay 'agent-shell-chat--tag) tags)
               (not (memq overlay kept)))
      (delete-overlay overlay))))

(defun agent-shell-chat--prompt-runs ()
  "Return each prompt-face run in the current buffer as a list of (BEG . END).
In buffer order.  Consecutive prompts (e.g. an empty submission leaves a
stale prompt above the fresh one) are separate entries, since input or
whitespace separates them."
  (save-excursion
    (goto-char (point-min))
    (let ((runs '())
          (pos (point-min)))
      (while (< pos (point-max))
        (let ((run-end (or (next-single-property-change pos 'font-lock-face)
                           (point-max))))
          (when (agent-shell-chat--prompt-face-p
                 (get-text-property pos 'font-lock-face))
            (push (cons pos run-end) runs))
          (setq pos run-end)))
      (nreverse runs))))

(defun agent-shell-chat--label-prompts ()
  "Overlay each prompt run in the current buffer.

Every prompt run shows a `Me' label.  A submitted turn's input follows
as buffer text; an empty submission (RET on an empty prompt reprints a
fresh prompt below it) shows a bare `Me'.  The last prompt is the live
one and also shows the prompt marker so it can be typed into, unless
`agent-shell-prompt-bar-mode' is on (then input flows through the bar and
the live prompt is hidden).

Input is bounded by the next prompt, not just the
`<shell-maker-end-of-prompt>' marker (which only appears once a response
starts): `Me' shows the instant a prompt is submitted, and an empty
prompt does not claim the fresh prompt below as its input.

Blank lines around the prompt collapse to exactly one on each side.  The
label rides the one above rather than covering it, so that nothing is
shown at the prompt itself, and the marker travels as a `line-prefix':
a string standing at the prompt holds point and the cursor on the row
above, putting the first line of a multi-line input out of reach of
`previous-line'.  Updates in place."
  (save-excursion
    (let ((runs (agent-shell-chat--prompt-runs))
          (prev-end nil)
          (pending-lead nil)
          (kept nil))
      (while runs
        (let* ((pos (caar runs))
               (run-end (cdar runs))
               (next-pos (caadr runs))
               (limit (or next-pos (point-max)))
               ;; Matched by property rather than by text: an agent quoting
               ;; "<shell-maker-end-of-prompt>" back writes the same
               ;; characters without it, and reading those as a boundary
               ;; would end the turn (and hide the live prompt's marker)
               ;; mid-response.
               (marker-pos (text-property-any run-end limit
                                              'shell-maker--marker t))
               (input-end (or marker-pos limit))
               (blank (string-blank-p
                       (buffer-substring-no-properties run-end input-end)))
               ;; The live prompt is the last one with no response yet: no
               ;; prompt and no end-of-prompt marker follow it.  (A submitted
               ;; turn awaiting its reprinted prompt is last but has a marker.)
               (live (and (null next-pos) (null marker-pos)))
               (raw-start (save-excursion
                            (goto-char pos)
                            (skip-chars-backward " \t\n")
                            ;; Do not swallow into the previous prompt run (an
                            ;; empty submission leaves a stale one right above).
                            (when (and prev-end (< (point) prev-end))
                              (goto-char prev-end))
                            ;; Leave a code block's tinted padding (an `:extend'
                            ;; background) to the panel: stepping back out of it
                            ;; keeps the overlay off those newlines, so the
                            ;; panel keeps its padding and cannot bleed across
                            ;; the label.
                            (while (and (< (point) pos)
                                        (agent-shell-chat--extends-bg-p
                                         (get-text-property (point) 'face)))
                              (forward-char 1))
                            (point)))
               ;; Classify what precedes the label: drives the leading pad and
               ;; whether a line terminator must be kept.
               (stacked (and prev-end (= raw-start prev-end)))
               (after-marker (and (> raw-start (point-min))
                                  (get-text-property (1- raw-start) 'shell-maker--marker)))
               (after-panel (and (> raw-start (point-min))
                                 (agent-shell-chat--extends-bg-p
                                  (get-text-property (1- raw-start) 'face))))
               ;; After response body (the normal case) keep that content's
               ;; line terminator visible: hiding it would merge the last
               ;; output line into the label for line motion (e.g.
               ;; `end-of-visual-line').  The other cases already leave a
               ;; visible terminator (a stale prompt, marker, or panel newline).
               (keep-term (and (not stacked) (not after-marker) (not after-panel)
                               (< raw-start pos) (eq (char-after raw-start) ?\n)
                               ;; Only a visible terminator ends the content
                               ;; line.  A collapsed fragment hides its own
                               ;; trailing newline, and leaving that outside
                               ;; the label spends the single leading newline
                               ;; ending the line instead of separating it,
                               ;; butting the label against the content.
                               (not (get-char-property raw-start 'invisible))))
               (start (if keep-term (1+ raw-start) raw-start))
               ;; The live prompt keeps its in-progress input (never swallow
               ;; it); a submitted turn swallows the input's leading blank lines.
               (end (if (or blank live) run-end
                      (save-excursion (goto-char run-end)
                                      (skip-chars-forward " \t\n")
                                      (point))))
               (me-label (agent-shell-chat--label
                          "Me" 'agent-shell-chat-me-label))
               ;; Face the padding and marker `default' so they do not inherit
               ;; the covered text's face: a display string's unfaced chars
               ;; take the face of the text they replace, and after a code
               ;; block that is the tinted source-block background.
               (pad (propertize "\n\n" 'face 'default))
               ;; Leading blank lines before the label.  `pad' (two newlines)
               ;; renders one blank line when `start' is mid-line after content.
               ;; Several cases need fewer, to keep it at exactly one:
               (lead (cond
                      ;; Directly stacked on the previous prompt run (empty
                      ;; submissions in a row).  That run is hidden (an empty
                      ;; submission is unlabeled), so it emitted no pad to
                      ;; separate this label: reuse the lead it would have
                      ;; used, which keeps one blank line however many empty
                      ;; submissions stack up.
                      (stacked (or pending-lead ""))
                      ;; Directly after an end-of-prompt marker: the response
                      ;; between it and this prompt is empty, so it carries no
                      ;; agent label whose pad would separate them.  A marker
                      ;; starting its line needs one newline; a turn
                      ;; interrupted mid-line (its notice sits right before the
                      ;; marker) needs the full pad to end that line first.
                      (after-marker (if (agent-shell-chat--marker-starts-line-p raw-start)
                                        (propertize "\n" 'face 'default)
                                      pad))
                      ;; After a code block panel: its tinted padding already
                      ;; separates the label, so one newline keeps exactly one.
                      (after-panel (propertize "\n" 'face 'default))
                      ;; Terminator kept visible: it ends the content line, so
                      ;; one newline adds the single blank line.
                      (keep-term (propertize "\n" 'face 'default))
                      (t pad)))
               ;; Whether this run is labeled at all: an empty submission
               ;; carries no `Me', and the prompt bar takes the live prompt
               ;; over entirely.
               (labeled (cond ((and live (bound-and-true-p
                                          agent-shell-prompt-bar-mode))
                               nil)
                              (live t)
                              (blank nil)
                              (t t)))
               ;; The newline closing the line above the prompt, when the run
               ;; covers one.  The label rides it, so nothing is left standing
               ;; at the prompt itself: a `before-string' there holds point and
               ;; cursor on the row above, putting a multi-line input's first
               ;; line out of reach of `previous-line'.
               (label-nl (and labeled (< start pos) (eq (char-before pos) ?\n)
                              (1- pos)))
               ;; The live prompt's marker, shown before the input whether or
               ;; not text has been typed yet.  Keying this off `blank' would
               ;; drop it the instant the user starts typing.  Carried as the
               ;; covered prompt's `display', standing on its buffer positions.
               (marker (when (and live labeled)
                         (propertize (concat agent-shell-chat--body-indent
                                             agent-shell-chat--prompt)
                                     'face 'default)))
               ;; Indents the prompt's own line, which the input's first line
               ;; shares.  An unlabeled run has no input to line up.
               (input-indent (if (and labeled (not blank))
                                 agent-shell-chat--body-indent
                               ""))
               ;; The label, closed by the newline it rides rather than by the
               ;; second half of `pad'.
               (before (cond ((not labeled) "")
                             (label-nl (concat lead me-label
                                               (propertize "\n" 'face
                                                           'default)))
                             (t (concat lead me-label pad)))))
          ;; Collapse whatever blank lines precede the one the label rides.
          (when (and label-nl (> label-nl start))
            (push
             (agent-shell-chat--ensure-overlay
              :tag 'me-surplus :beg start :end label-nl
              :front-advance t
              :props (list (cons 'display "")
                           (cons 'line-prefix "")
                           (cons 'wrap-prefix "")))
             kept))
          ;; Carry the label on the newline above, left visible so it keeps
          ;; closing its line.
          (when label-nl
            (push
             (agent-shell-chat--ensure-overlay
              :tag 'me-label :beg label-nl :end pos
              :front-advance t
              :props (list (cons 'before-string before)
                           (cons 'line-prefix "")
                           (cons 'wrap-prefix "")))
             kept))
          (push
           (agent-shell-chat--ensure-overlay
            :tag 'me :beg (if label-nl pos start) :end end
            :front-advance t
            ;; Anchor on the prompt run, which the span may start before: the
            ;; span's start flips with `label-nl', and reuse has to survive
            ;; that flip rather than strand the overlay it should have moved.
            :anchor-beg pos :anchor-end run-end
            ;; Replace the covered prompt: with the live prompt's marker when
            ;; there is one, otherwise with \"\" to hide it.
            ;;
            ;; Where a newline above carries the label, no *inserted* string
            ;; stands at this position: a `before-string' here would keep
            ;; `previous-line' from settling on the input's first line.  The
            ;; marker is safe as a `display' because it stands on the covered
            ;; prompt's own positions rather than adding any, so vertical
            ;; motion behaves as it did with the prompt text visible.
            ;;
            ;; It must not be a `line-prefix': that belongs to the whole line,
            ;; and the input's first line shares this one, so redisplay cannot
            ;; take its cheap single-line path -- every edit re-lays the line
            ;; out and the input visibly paints unindented before jumping
            ;; right.  `line-prefix' is left to `input-indent' alone, which
            ;; only ever applies to a submitted turn, and still drops any
            ;; tinted gutter inherited from the covered text.
            ;;
            ;; With no newline above, the label renders here instead and each
            ;; of its blank lines becomes a row of this line.  The marker
            ;; rejoins the label, and no prefix is set at all: either would
            ;; repeat down every row, marking or indenting the label along
            ;; with the input.
            :props (list (cons 'before-string
                               (if label-nl "" (concat before (or marker ""))))
                         (cons 'display (if label-nl (or marker "") ""))
                         (cons 'line-prefix
                               (if (and label-nl (not marker)) input-indent ""))
                         (cons 'wrap-prefix (if label-nl input-indent ""))))
           kept)
          ;; Indent the live prompt's draft below its first line, which the
          ;; marker indents.  The overlay above covers the prompt text alone,
          ;; so no row starting past it -- a wrapped one, or one a typed
          ;; newline began -- ever sees a prefix of its own.  Rear-advancing
          ;; and anchored at the input's start: it has to be in place, and
          ;; grow, as characters arrive, since no relabel runs while typing.
          (when (and live labeled)
            (push
             (agent-shell-chat--ensure-overlay
              :tag 'me-draft :beg run-end :end (point-max)
              :rear-advance t
              :props (list (cons 'line-prefix agent-shell-chat--body-indent)
                           (cons 'wrap-prefix agent-shell-chat--body-indent)
                           ;; Indents a last line left empty by a newline,
                           ;; which the prefix cannot reach.  The hooks keep
                           ;; it in step with what is typed.
                           (cons 'after-string
                                 (agent-shell-chat--draft-tail-indent
                                  run-end (point-max)))
                           (cons 'modification-hooks
                                 (list #'agent-shell-chat--draft-changed))
                           (cons 'insert-in-front-hooks
                                 (list #'agent-shell-chat--draft-changed))
                           (cons 'insert-behind-hooks
                                 (list #'agent-shell-chat--draft-changed))))
             kept))
          ;; Indent a submitted turn's input so it aligns with the response
          ;; body.  The live prompt (input flows after the marker) and empty
          ;; prompts have no input to indent.
          (unless (or blank live)
            ;; End at the input's last real character, not `input-end': the
            ;; agent label's `before-string' renders in the trailing newline
            ;; before the marker, and would inherit this `line-prefix'.
            (let ((input-last (save-excursion (goto-char input-end)
                                              (skip-chars-backward " \t\n")
                                              (point))))
              (push
               (agent-shell-chat--ensure-overlay
                :tag 'me-input :beg end :end input-last
                :props (list (cons 'line-prefix agent-shell-chat--body-indent)
                             (cons 'wrap-prefix agent-shell-chat--body-indent)))
               kept)))
          ;; A hidden label emits no pad of its own, so hand its lead to
          ;; whichever label renders next.
          (setq pending-lead (and (not labeled) lead))
          (setq prev-end run-end)
          (setq runs (cdr runs))))
      ;; Drop stale labels whose prompt run was deleted (e.g. a live prompt a
      ;; `session/push' removed) and which no label above reached.
      (agent-shell-chat--gc-overlays '(me me-label me-surplus me-input me-draft)
                                     kept)
      ;; Labels from before chat overlays stopped using `category': an upgrade
      ;; reloads this file into a running session (see
      ;; `package--reload-previously-loaded'), where relabeling no longer
      ;; recognises them and they render the label a second time.
      ;; TODO: Remove after 2026-09-28.
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (memq (overlay-get overlay 'category)
                    '(agent-shell-chat-me agent-shell-chat-me-label
                                          agent-shell-chat-me-surplus
                                          agent-shell-chat-me-input))
          (delete-overlay overlay))))))

(defun agent-shell-chat--label-responses ()
  "Overlay the agent label before every response in the current buffer.

Anchored on the invisible `<shell-maker-end-of-prompt>' marker.  Hides
the marker and the extra blank lines around it with a `display' of \"\",
and renders the label via a `before-string' padded by one blank line on
each side.  Keeps the input's line terminator visible: hiding that
newline would merge the input line into the response for line motion
\(e.g. `end-of-visual-line').  Updates in place; idempotent."
  (save-excursion
    (goto-char (point-min))
    (let ((label (agent-shell-chat--label
                  (agent-shell-chat--agent-name)
                  'agent-shell-chat-agent-label))
          (kept nil))
      (while (agent-shell-chat--search-marker-forward)
        (let* ((mbeg (match-beginning 0))
               (mend (match-end 0))
               (end (save-excursion
                      (goto-char mend)
                      ;; Swallow the response's leading blank lines, but stop
                      ;; at a code block panel's tinted top padding (an
                      ;; `:extend' background) so the panel keeps its internal
                      ;; padding rather than having it hidden by `display'.
                      (while (and (< (point) (point-max))
                                  (memq (char-after) '(?\s ?\t ?\n))
                                  (not (agent-shell-chat--extends-bg-p
                                        (get-text-property (point) 'face))))
                        (forward-char 1))
                      ;; Do not swallow the whitespace before a following prompt
                      ;; (an empty response): it belongs to that prompt's
                      ;; spacing, and swallowing it would overlap the `Me'
                      ;; overlay.  Keep the label anchored at the marker.
                      (if (agent-shell-chat--prompt-face-p
                           (get-text-property (point) 'font-lock-face))
                          mend
                        (point))))
               ;; Start just past the input's line terminator: the first
               ;; newline after the input, whether it precedes the marker
               ;; (live turns: \"input\\n<marker>\") or follows it (restored
               ;; turns: \"input<marker>\\n\").  That newline stays visible
               ;; while extra blank lines are hidden; hiding it would merge
               ;; the input line into the response for line motion (e.g.
               ;; `end-of-visual-line').  Bounded by `end' so the search stops
               ;; before the response body.  The marker hides itself.
               (start (save-excursion
                        (goto-char mbeg)
                        (skip-chars-backward " \t\n")
                        (if (re-search-forward "\n" end t) (point) mbeg)))
               ;; With the terminator kept, one leading newline pads the label;
               ;; without one keep two.
               (before (concat (if (and (> start (point-min))
                                        (eq (char-before start) ?\n))
                                   "\n"
                                 "\n\n")
                               label "\n\n"))
               ;; A completed turn with no response text (a tool-only turn, or
               ;; a restored empty turn) is not labeled: another marker or the
               ;; next prompt follows the marker with only whitespace between.
               ;; A marker at end of buffer is the active, just-submitted turn
               ;; (its output has not streamed yet), so it IS labeled.
               (response-empty (save-excursion
                                 (goto-char mend)
                                 (skip-chars-forward " \t\n")
                                 (or (get-text-property (point) 'shell-maker--marker)
                                     (agent-shell-chat--prompt-face-p
                                      (get-text-property (point) 'font-lock-face))))))
          ;; A turn with no response is not labeled; its stale overlay, if
          ;; any, is dropped by the `--gc-overlays' sweep below.  Widen the
          ;; anchor back to the marker: a restored turn's overlay starts past
          ;; it (keeping the terminator visible), and the span alone would
          ;; miss the overlay it should have reused.
          (unless response-empty
            (push
             (agent-shell-chat--ensure-overlay
              :tag 'agent :beg start :end end
              :anchor-beg mbeg :anchor-end end
              :props (list (cons 'before-string before) (cons 'display "")))
             kept))))
      (agent-shell-chat--gc-overlays '(agent) kept)
      ;; TODO: Remove after 2026-09-28 (see `agent-shell-chat--label-prompts').
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (eq (overlay-get overlay 'category) 'agent-shell-chat-agent)
          (delete-overlay overlay))))))

(defun agent-shell-chat--relabel ()
  "Apply the `Me' and agent labels to the current buffer (idempotent).
Scans the whole buffer; cheap in practice since it walks property
changes and skips already-labeled runs, but could be scoped to the
active turn if it ever shows on very long conversations."
  (agent-shell-chat--label-prompts)
  (agent-shell-chat--label-responses))

(defun agent-shell-chat--relabel-all ()
  "Relabel every labeled shell buffer.
Used after `agent-shell-prompt-bar-mode' toggles, so the live prompt
flips between hidden and `Me' immediately across all shells."
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'agent-shell-chat--labeled buffer)
      (with-current-buffer buffer
        (agent-shell-chat--relabel)))))

(defun agent-shell-chat--relabel-buffer (buffer)
  "Relabel BUFFER, clearing its pending relabel timer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq agent-shell-chat--relabel-timer nil)
      (agent-shell-chat--relabel))))

(defun agent-shell-chat--schedule-relabel (&rest _)
  "Schedule a coalesced, deferred relabel of the current buffer.

Deferred so the triggering change's own text properties (e.g. the prompt
face shell-maker applies after inserting) are in place; coalesced so a
burst yields a single relabel.  Runs from the event subscription (which
covers submissions, streaming, turn completion and `session-restored')
and from `shell-maker-finish-output-hook' (which covers `clear')."
  (when (and agent-shell-chat--labeled
             (not agent-shell-chat--relabel-timer))
    (setq agent-shell-chat--relabel-timer
          (run-at-time 0 nil #'agent-shell-chat--relabel-buffer (current-buffer)))))

(defun agent-shell-chat--decorate-prompt-region (beg end)
  "Hide the shell prompt between BEG and END behind the `Me' label.

Used where the prompt is read outside the shell, so it reads the way the
shell renders its own.  Returns the overlay.

For example, over a minibuffer reading \"Claude> \", the prompt is
replaced by the label, a blank line, and the marker the input follows:

   Me

    \N{U+276F} "
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'agent-shell-chat--tag 'me)
    (overlay-put overlay 'display "")
    ;; Laid out as the shell lays out its own live prompt, without its
    ;; leading pad: nothing sits above this one to separate it from.
    (overlay-put overlay 'before-string
                 (concat (agent-shell-chat--label "Me" 'agent-shell-chat-me-label)
                         (propertize "\n\n" 'face 'default)
                         (propertize (concat agent-shell-chat--body-indent
                                             agent-shell-chat--prompt)
                                     'face 'default)))
    overlay))

(defun agent-shell-chat--decorate-queued-prompt (event)
  "Label the queued prompt being read for EVENT\\='s shell.

Runs from `agent-shell-prompt-queue-setup-minibuffer-functions' with the
minibuffer current, and leaves shells with chat mode off alone.

EVENT is an alist as that hook documents, for example:

  \\='((:shell-buffer . #<buffer Claude Agent @ agent-shell>))"
  (when-let* ((shell-buffer (map-elt event :shell-buffer))
              ((buffer-local-value 'agent-shell-chat-mode shell-buffer)))
    (agent-shell-chat--decorate-prompt-region (point-min) (minibuffer-prompt-end))))

(defun agent-shell-chat--enable ()
  "Turn on chat labels in the current buffer and keep them in sync.

Backfills existing turns, subscribes to shell events so a coalesced
relabel tracks submissions, streaming responses, turn completion and
reloads (`session-restored'), and adds a buffer-local
`shell-maker-finish-output-hook' so `clear' and the other internal
commands (which reprint the prompt with no `agent-shell' event) relabel
too."
  (unless agent-shell-chat--labeled
    (setq-local agent-shell-chat--labeled t)
    (agent-shell-chat--relabel)
    (setq-local agent-shell-chat--subscription
                (agent-shell-subscribe-to
                 :shell-buffer (current-buffer)
                 :on-event #'agent-shell-chat--schedule-relabel))
    (add-hook 'shell-maker-finish-output-hook
              #'agent-shell-chat--schedule-relabel nil t)
    (add-hook 'agent-shell-prompt-queue-setup-minibuffer-functions
              #'agent-shell-chat--decorate-queued-prompt)))

(defun agent-shell-chat--disable ()
  "Remove chat labels, subscription, timer and hook from the current buffer."
  (remove-hook 'shell-maker-finish-output-hook
               #'agent-shell-chat--schedule-relabel t)
  (when agent-shell-chat--subscription
    (agent-shell-unsubscribe :subscription agent-shell-chat--subscription))
  (when (timerp agent-shell-chat--relabel-timer)
    (cancel-timer agent-shell-chat--relabel-timer))
  (dolist (tag '(me me-label me-surplus me-input me-draft agent))
    (remove-overlays (point-min) (point-max) 'agent-shell-chat--tag tag))
  ;; Labels from before chat overlays stopped using `category'.
  ;; TODO: Remove after 2026-09-28 (see `agent-shell-chat--label-prompts').
  (dolist (category '(agent-shell-chat-me agent-shell-chat-me-label
                                          agent-shell-chat-me-surplus
                                          agent-shell-chat-me-input
                                          agent-shell-chat-agent))
    (remove-overlays (point-min) (point-max) 'category category))
  ;; The minibuffer hook is global, so it goes once the last shell drops it.
  (unless (seq-find (lambda (buffer)
                      (buffer-local-value 'agent-shell-chat-mode buffer))
                    (buffer-list))
    (remove-hook 'agent-shell-prompt-queue-setup-minibuffer-functions
                 #'agent-shell-chat--decorate-queued-prompt))
  (kill-local-variable 'agent-shell-chat--subscription)
  (kill-local-variable 'agent-shell-chat--relabel-timer)
  (kill-local-variable 'agent-shell-chat--labeled))

;;; Mode

;;;###autoload
(define-minor-mode agent-shell-chat-mode
  "Toggle chat-style `Me'/agent labels in the current `agent-shell' buffer.

Each submitted turn is boxed `Me' and each response the agent's name.
The live prompt shows `Me' so you can type into the shell; when
`agent-shell-prompt-bar-mode' is on it is hidden, since input flows
through the bar instead.

Enable it for new shells by default with `agent-shell-chat-mode-enabled'."
  :lighter nil
  :group 'agent-shell
  (cond
   ((not agent-shell-chat-mode)
    (agent-shell-chat--disable))
   ((derived-mode-p 'agent-shell-mode)
    (agent-shell-chat--enable))
   (t
    ;; Undo the toggle before erroring so the mode does not read as on.
    (setq agent-shell-chat-mode nil)
    (user-error "Not in an `agent-shell' buffer"))))

;; Shells labeled by a version that named overlays with `category' keep
;; overlays relabeling no longer recognises, which render their label a
;; second time.  A package upgrade reloads this file into the running
;; session (see `package--reload-previously-loaded'), so relabel there and
;; then rather than leaving those shells wrong until their next turn.
;; TODO: Remove after 2026-09-28.
(dolist (buffer (buffer-list))
  (when (buffer-local-value 'agent-shell-chat--labeled buffer)
    (with-current-buffer buffer
      ;; Demoted: this runs while the package loads, so one odd shell
      ;; reports itself rather than taking the load down with it.
      (with-demoted-errors "agent-shell-chat: %S"
        (agent-shell-chat--relabel)))))

(provide 'agent-shell-chat-mode)

;;; agent-shell-chat-mode.el ends here
