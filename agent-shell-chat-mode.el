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

(require 'agent-shell-faces)
(require 'agent-shell-prompt)
(require 'map)
(require 'seq)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defvar agent-shell-prompt-queue-setup-minibuffer-functions)
(defvar agent-shell-section-functions)

(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")

(defvar agent-shell--state)
(defvar agent-shell-show-busy-indicator)
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

(defcustom agent-shell-prompt-busy-frames
  '("|" "/" "-" "\\")
  "Frames animating the live prompt's marker while the agent works.
A list or vector of strings, drawn in the body indent ahead of the
marker, a string shown as is, without animating, or a function
returning either (or nil to show nothing).  For example,
\\='(\"·\" \"•\" \"●\" \"•\") or \"(busy)\".

Outside `agent-shell-chat-mode', drawn ahead of the shell prompt's
chevron instead.  For example, \"Claude|> \".

A function takes no arguments and is called in the shell buffer on
every heartbeat tick, so keep it cheap.  For example, to show
\"(connecting)\" until the session starts, and \"(busy)\" after:

  (lambda ()
    (if (agent-shell-session-id) \"(busy)\" \"(connecting)\"))

The body indent fits a single column, so a wider frame (like \"(busy)\")
is followed by a plain space, pushing the marker right while the agent
works.

ASCII by default, so the buffer's own font draws them.  Anything a font
may lack comes from a fallback font instead, which can draw it wider
than a column or with a taller line, bouncing the prompt as frames
change.  Braille, the usual spinner, is one such (and may show unraised
dots)."
  :type '(choice (string :tag "Static text")
                 (repeat :tag "Frames" string)
                 (function :tag "Function"))
  :group 'agent-shell)

(defcustom agent-shell-chat-label-function #'agent-shell-chat-boxed-label
  "Function returning a chat label (\"Me\" or the agent's name).

Called with an alist, for example:

  \\='((:text . \"Me\")
    (:role . user)
    (:prompt-start . 1234))

:role is `user' for the \"Me\" label and `agent' for the agent's.

:prompt-start is where the turn's prompt starts (as in
`agent-shell-prompt-ranges'), shared by both of its labels.  nil for the
live prompt, which is not a turn until submitted.  Current for this call
only, as text above can still change, so keep any per-turn details (e.g.
times recorded from `agent-shell-subscribe-to' events) as text
properties there, which travel with the turn, rather than keyed by the
number.

Called on every relabel, so should be cheap.

Should return a single-line string, propertized as the label is to be
drawn.  A `display' property is honored, so the label can be drawn as an
image (e.g. an SVG badge).

See `agent-shell-chat-boxed-label' for the default."
  :type 'function
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

(defvar-local agent-shell-chat--live-marker-overlay nil
  "Overlay last found drawing the live prompt's marker, or nil.
A hint only: see `agent-shell-chat--find-live-marker-overlay'.")

;;; Labels

(defun agent-shell-chat--label (text role &optional prompt-start)
  "Return the chat label for TEXT, as `agent-shell-chat-label-function' draws it.

ROLE is `user' or `agent'.  PROMPT-START is where the turn's prompt
starts, or nil when the label stands for no turn yet.

For example, (agent-shell-chat--label \"Me\" \\='user) returns \" Me \" in
`agent-shell-chat-me-label' by default."
  (funcall agent-shell-chat-label-function
           (list (cons :text text)
                 (cons :role role)
                 (cons :prompt-start prompt-start))))

(defun agent-shell-chat-boxed-label (label)
  "Return LABEL's text padded and propertized as a boxed badge.

The default `agent-shell-chat-label-function'.  The face is picked by
LABEL's :role (see `agent-shell-chat-me-label' and
`agent-shell-chat-agent-label'), and carries the box.

For example, over \\='((:text . \"Me\") (:role . user)) returns \" Me \"
in `agent-shell-chat-me-label'."
  (propertize (format " %s " (map-elt label :text))
              'face (if (eq (map-elt label :role) 'user)
                        'agent-shell-chat-me-label
                      'agent-shell-chat-agent-label)))

(defun agent-shell-chat--row-props (row)
  "Return overlay properties drawing label ROW on the position it covers.

A row is normally the overlay's `display'.  Emacs ignores `display'
properties nested inside a `display' string, though, so a row holding
one (e.g. a label drawn as an image by `agent-shell-chat-label-function')
is drawn as a `before-string' instead, with `display' left to its line
terminator.

For example, over \" Me \\n\" in plain text returns
\((display . \" Me \\n\") (before-string . \"\"))."
  (if (text-property-not-all 0 (length row) 'display nil row)
      (let ((terminated (string-suffix-p "\n" row)))
        (list (cons 'display (if terminated "\n" ""))
              (cons 'before-string (if terminated
                                       (substring row 0 -1)
                                     row))))
    (list (cons 'display row)
          (cons 'before-string ""))))

(defun agent-shell-chat--busy-frame ()
  "Return the busy frame for the heartbeat's current beat, or nil when idle.

For example, on the heartbeat's third beat returns \"-\".  With
`agent-shell-prompt-busy-frames' set to a string, or to a function
returning one, returns that string on every beat.  Empty frames, like
[] or \"\", return nil."
  (when-let* (((bound-and-true-p agent-shell-show-busy-indicator))
              (heartbeat (map-elt agent-shell--state :heartbeat))
              ((eq (map-elt heartbeat :status) 'busy))
              (frames (if (functionp agent-shell-prompt-busy-frames)
                          (funcall agent-shell-prompt-busy-frames)
                        agent-shell-prompt-busy-frames))
              ((not (seq-empty-p frames))))
    (if (stringp frames)
        frames
      (seq-elt frames (mod (map-elt heartbeat :value) (seq-length frames))))))

(defun agent-shell-chat--live-marker ()
  "Return the live prompt's marker, animated while the agent works.

For example, returns \"  ❯ \" when idle and \"| ❯ \" while busy, the
frame faced `agent-shell-secondary' so it recedes behind the marker.

A frame's glyph can come from a fallback font wider than a column, so
the space after it aligns the marker to the body indent rather than
trusting the frame's width.  Aligned in units of the buffer's own font,
which unlike bare columns follow `text-scale-adjust'.  A frame wider
than a column (like \"(busy)\") can't fit the indent, so a plain space
follows it instead."
  (if-let* ((frame (agent-shell-chat--busy-frame)))
      (concat (propertize frame 'face 'agent-shell-secondary)
              (propertize (concat (if (> (string-width frame) 1)
                                      " "
                                    (propertize " " 'display
                                                `(space :align-to
                                                        (,(string-width agent-shell-chat--body-indent)
                                                         . width))))
                                  agent-shell-chat--prompt)
                          'face 'default))
    (propertize (concat agent-shell-chat--body-indent agent-shell-chat--prompt)
                'face 'default)))

(defun agent-shell-chat--find-live-marker-overlay ()
  "Return the overlay drawing the live prompt's marker, or nil.

That overlay carries `agent-shell-chat--marker-head', which relabeling
clears the moment its prompt is submitted.  The overlay found last is
cached and trusted for as long as it still carries it: an overlay moves
with the text around it, so text inserted or deleted above leaves it
spanning the prompt.  One deleted, or whose prompt was submitted, sends
this back to scanning the buffer.

Runs on every heartbeat tick, where scanning would list every overlay in
the buffer each time."
  (if (and agent-shell-chat--live-marker-overlay
           (overlay-buffer agent-shell-chat--live-marker-overlay)
           (overlay-get agent-shell-chat--live-marker-overlay
                        'agent-shell-chat--marker-head))
      agent-shell-chat--live-marker-overlay
    (setq agent-shell-chat--live-marker-overlay
          (save-restriction
            (widen)
            (seq-find (lambda (overlay)
                        (overlay-get overlay 'agent-shell-chat--marker-head))
                      (overlays-in (point-min) (point-max)))))))

(defun agent-shell-chat--animate-live-marker ()
  "Redraw the live prompt's marker for the heartbeat's current beat.

Runs on every heartbeat tick, so it rewrites the one overlay drawing the
marker rather than relabeling the buffer.  That overlay carries the text
drawn ahead of the marker as `agent-shell-chat--marker-head': empty
where the marker is drawn alone as `display', and the `Me' label where
both are drawn as `before-string'."
  (when-let* ((overlay (agent-shell-chat--find-live-marker-overlay))
              (head (overlay-get overlay 'agent-shell-chat--marker-head))
              (property (if (string-empty-p head) 'display 'before-string))
              (drawn (concat head (agent-shell-chat--live-marker))))
    (unless (equal (overlay-get overlay property) drawn)
      (overlay-put overlay property drawn))))

(defun agent-shell-chat--agent-name ()
  "Return the attached agent's display name for the response label.

For example, with a mode-line name of \"Claude\" returns \"Claude\";
with none available, returns \"Agent\"."
  (or (map-nested-elt agent-shell--state '(:agent-config :mode-line-name))
      "Agent"))

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

(cl-defun agent-shell-chat--ensure-overlay (&key tag beg end props rear-advance
                                                 (anchor-beg beg)
                                                 (anchor-end end))
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

Writing only what it is handed also means a reused overlay keeps every
property PROPS leaves out, whoever wrote it.  A caller drawing a label
therefore spells out both of the properties a label can be drawn with
\(`display' and `before-string'), including the one it does not use, so
that relabeling heals an overlay a differently drawn label left behind
\(see `agent-shell-chat--label-rows').

ANCHOR-BEG..ANCHOR-END default to the span, and are widened only where
an overlay is expected to sit somewhere its span no longer covers.

With REAR-ADVANCE non-nil the overlay takes in text inserted at its end,
so it can hold properties over an input still being typed: relabeling is
event-driven, so an overlay that stopped at the caret would never grow
to cover what follows it."
  (let ((overlay (or (seq-find (lambda (overlay)
                                 (eq (overlay-get overlay 'agent-shell-chat--tag) tag))
                               (overlays-in anchor-beg (max anchor-end (1+ anchor-beg))))
                     (let ((created (make-overlay beg end nil nil rear-advance)))
                       (overlay-put created 'agent-shell-chat--tag tag)
                       created)))
        ;; `evaporate' is carried only while the span has text to
        ;; evaporate with.  BEG..END can be empty (a restored turn leaves
        ;; the marker with no room between it and the response it labels),
        ;; and an empty overlay is deleted the moment `evaporate' lands on
        ;; it, taking the label with it and leaving this holding an
        ;; overlay with no buffer at all.  Dropped before a move that
        ;; empties the span, taken back up after one that fills it, for
        ;; the same reason.  A rear-advancing overlay never carries it: it
        ;; starts out empty, waiting on input still being typed.
        (evaporates (and (not rear-advance) (< beg end))))
    (when (and (not evaporates) (overlay-get overlay 'evaporate))
      (overlay-put overlay 'evaporate nil))
    (unless (and (= (overlay-start overlay) beg) (= (overlay-end overlay) end))
      (move-overlay overlay beg end))
    (when (and evaporates (not (overlay-get overlay 'evaporate)))
      (overlay-put overlay 'evaporate t))
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

(defun agent-shell-chat--draws-name-p (overlay property)
  "Return non-nil when OVERLAY's PROPERTY draws a label's name.

A label is layout as much as name: blank lines above and below it, and
the rows carrying those are blank strings.  So is the string an overlay
drawing no label carries, which is empty rather than absent.  Only the
one row with a name in it counts, or a whole label would be read as one
per row.

For example, over a row displaying \" Claude \\n\" returns non-nil, and
over the blank row padding it out returns nil."
  (let ((value (overlay-get overlay property)))
    (and (stringp value)
         (not (string-blank-p value)))))

(defun agent-shell-chat--turn-label-at (position)
  "Return the turn label chat mode draws at POSITION, or nil for none.

Read from the overlay's `agent-shell-chat--tag' rather than from what it
draws, which wraps the name in layout (blank lines, the prompt glyph).

A label is drawn a row to each buffer position, as a `display', or
carried whole on a `before-string' where there are too few positions to
go around.  Either way exactly one string has the name in it (see
`agent-shell-chat--draws-name-p'), so a turn is labeled once.

The `me' overlay is read for its `before-string' alone: its `display'
carries the live prompt's marker, which is a glyph to type at rather
than a label.

For example, at a submitted turn returns \"Me\"."
  (when-let* ((labelled (seq-find
                         (lambda (candidate)
                           (when-let* ((tag (overlay-get candidate
                                                         'agent-shell-chat--tag))
                                       ((memq tag '(me me-label agent))))
                             (or (agent-shell-chat--draws-name-p
                                  candidate 'before-string)
                                 (and (not (eq tag 'me))
                                      (agent-shell-chat--draws-name-p
                                       candidate 'display)))))
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

(defun agent-shell-chat--draft-indent ()
  "Return the indent lining a draft's later lines up with its first.
The first line shares the prompt's row, starting past the marker drawn
there, so the lines below it clear the marker's width as well as the
body indent to start at the same column.  A submitted turn drops back to
`agent-shell-chat--body-indent', where the marker is gone and the
response body sits."
  (concat agent-shell-chat--body-indent
          (make-string (string-width agent-shell-chat--prompt) ?\s)))

(defun agent-shell-chat--draft-tail-indent (beg end)
  "Return the indent a draft spanning BEG..END needs on its last line.

A `line-prefix' hangs off the character a display row starts from, and a
draft ending in a newline has none there: that row starts at end of
buffer.  The caret would sit flush left until the first character landed
to carry the prefix.  A string standing at that position indents the row
instead, and gives way (to \"\") the moment there is a character for the
prefix itself.

For example, over a draft of \"one\\n\" returns the draft indent, and over
\"one\" returns \"\"."
  (if (and (> end beg) (eq (char-before end) ?\n))
      (agent-shell-chat--draft-indent)
    ""))

(defun agent-shell-chat--draft-changed (draft after &rest _)
  "Re-indent DRAFT's last line once a change to it has landed.

Runs from DRAFT's own modification hooks, AFTER being non-nil once the
change is in.  Kept off the relabel path: relabeling is event-driven and
coalesced, so none runs between the newline that empties the last line
and the character that fills it.

Widens before reading DRAFT's bounds: the hooks fire during whatever
edit provoked them, and a caller rendering above the prompt narrows to
end before it (see `agent-shell--update-fragment'), while DRAFT
rear-advances toward end of buffer.  Its bounds then lie outside that
restriction, which `text-property-any' rejects outright."
  (when after
    (save-restriction
      (widen)
      (agent-shell-chat--draft-reindent draft))))

(defun agent-shell-chat--draft-reindent (draft)
  "Set DRAFT's trailing indent, stopping it at a submitted turn's marker.

The marker and the response that follows it both arrive at end of
buffer, which a rear-advancing overlay takes in, indenting the response
as though it were still being typed until the next relabel drops the
overlay.

Split out of `agent-shell-chat--draft-changed' so the widening it needs
wraps every buffer position this reads.

For example, over a DRAFT covering \"one\\n\" sets its `after-string' to
the body indent, and over \"one\" sets it to \"\".  Once a submission has
made that \"one\\n<marker>\", DRAFT is left ending before the marker, with
an `after-string' of \"\"."
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
      (overlay-put draft 'after-string indent))))

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

(defun agent-shell-chat--label-rows (label)
  "Split LABEL into display rows, one string per row.

Each row is meant for a buffer position of its own.  A multi-row string
stacks every row on the single position it hangs from, and `window-start'
can only ever be a buffer position, so scrolling by fewer rows than the
string spans has nowhere to land and stops advancing (on graphical
frames; terminals advance through it fine).

Rows are drawn with `display' where the label used to be drawn with a
single `before-string'.  Both properties are written wherever either is,
so that a shell labeled by the other version heals on its next relabel
rather than showing its label twice: overlays are reused by tag, and a
reused one keeps whatever the version before wrote (see
`agent-shell-chat--ensure-overlay').

For example, over \"\\n Me \\n\" returns (\"\\n\" \" Me \\n\"), and over a
label with no newline returns it unchanged as a single row."
  (let ((pieces (split-string label "\n")))
    (append (mapcar (lambda (piece) (concat piece "\n")) (butlast pieces))
            (let ((tail (car (last pieces))))
              (unless (string-empty-p tail)
                (list tail))))))

(defun agent-shell-chat--gc-overlays (tags kept)
  "Delete label overlays of TAGS not in KEPT (a list of overlays).
Removes stale labels whose prompt run or marker was deleted (e.g. a live
prompt a `session/push' removed), which relabeling would not otherwise
reach."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (and (memq (overlay-get overlay 'agent-shell-chat--tag) tags)
               (not (memq overlay kept)))
      (delete-overlay overlay))))

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
    (let ((ranges (agent-shell-prompt-ranges))
          (prev-end nil)
          (pending-lead nil)
          (kept nil))
      (while ranges
        (let* ((range (car ranges))
               (pos (map-elt range :prompt-start))
               (run-end (map-elt range :prompt-end))
               (input-end (map-elt range :input-end))
               (blank (string-blank-p
                       (buffer-substring-no-properties
                        (map-elt range :input-start) input-end)))
               ;; The live prompt is the last one with no response yet: no
               ;; prompt and no end-of-prompt marker follow it.  (A submitted
               ;; turn awaiting its reprinted prompt is last but has a marker.)
               (live (and (null (cdr ranges))
                          (not (get-text-property input-end
                                                  'shell-maker--marker))))
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
               (me-label (agent-shell-chat--label "Me" 'user (unless live pos)))
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
                         (agent-shell-chat--live-marker)))
               ;; Indents the prompt's own line, which the input's first line
               ;; shares.  Where a marker heads that line, the first line starts
               ;; past it, so its wrapped rows clear the marker too.  An
               ;; unlabeled run has no input to line up.
               (input-indent (cond ((or (not labeled) blank) "")
                                   (marker (agent-shell-chat--draft-indent))
                                   (t agent-shell-chat--body-indent)))
               ;; The label, closed by the newline it rides rather than by the
               ;; second half of `pad'.
               (before (cond ((not labeled) "")
                             (label-nl (concat lead me-label
                                               (propertize "\n" 'face
                                                           'default)))
                             (t (concat lead me-label pad))))
               ;; One row per covered prompt position, where the prompt is
               ;; long enough to hold them; the overlay covering the prompt
               ;; then starts past the rows rather than under them.
               ;;
               ;; Laid past the newline above rather than before it, which
               ;; shifts the padding by one row in each direction: that
               ;; newline now closes the row `lead' opened with its first, and
               ;; the label needs a blank line of its own after it (the second
               ;; half of `pad') where it used to borrow that newline's.  What
               ;; `lead' accounts for beyond that first newline still holds.
               (label-rows (and label-nl
                                (agent-shell-chat--label-rows
                                 (concat (string-remove-prefix "\n" lead)
                                         me-label pad))))
               ;; Whether the covered prompt has a position for every row.
               (split (<= (length label-rows) (- run-end pos)))
               (label-start (if split (+ pos (length label-rows)) pos)))
          ;; Collapse whatever blank lines precede the one the label rides.
          (when (and label-nl (> label-nl start))
            (push
             (agent-shell-chat--ensure-overlay
              :tag 'me-surplus :beg start :end label-nl
              :props (list (cons 'display "")
                           (cons 'line-prefix "")
                           (cons 'wrap-prefix "")))
             kept))
          ;; Lay the label's rows over the head of the covered prompt, one row
          ;; per buffer position, so every row is somewhere `window-start' can
          ;; land (see `agent-shell-chat--label-rows').  The prompt is hidden
          ;; either way, so the rows cost nothing that was being shown.  Falls
          ;; back to carrying the whole label on the newline above when the
          ;; prompt is too short to hold a row apiece.
          (when label-nl
            (if split
                (seq-do-indexed
                 (lambda (row offset)
                   (push
                    (agent-shell-chat--ensure-overlay
                     :tag 'me-label
                     :beg (+ pos offset) :end (+ pos offset 1)
                     ;; Both `display' and `before-string' are spelled out
                     ;; so that a reused overlay cannot keep a label drawn
                     ;; the other way (see `agent-shell-chat--ensure-overlay').
                     :props (append
                             (agent-shell-chat--row-props row)
                             ;; Above the overlay covering the prompt, whose
                             ;; `line-prefix' would otherwise indent the label
                             ;; with the input it belongs beside.
                             (list (cons 'priority 100)
                                   (cons 'line-prefix "")
                                   (cons 'wrap-prefix ""))))
                    kept))
                 label-rows)
              (push
               (agent-shell-chat--ensure-overlay
                :tag 'me-label :beg label-nl :end pos
                :props (list (cons 'before-string before)
                             ;; Spelled out for the same reason, as `nil'
                             ;; rather than "": this overlay covers the
                             ;; newline above, which an empty `display'
                             ;; would hide along with it.
                             (cons 'display nil)
                             (cons 'line-prefix "")
                             (cons 'wrap-prefix "")))
               kept)))
          (push
           (agent-shell-chat--ensure-overlay
            :tag 'me :beg (if label-nl label-start start) :end end
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
                         (cons 'wrap-prefix (if label-nl input-indent ""))
                         ;; What is drawn ahead of the marker, so a heartbeat
                         ;; tick can redraw it alone (see
                         ;; `agent-shell-chat--animate-live-marker').  Cleared
                         ;; the moment the prompt is submitted.
                         (cons 'agent-shell-chat--marker-head
                               (when marker
                                 (if label-nl "" before)))))
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
              :props (list (cons 'line-prefix (agent-shell-chat--draft-indent))
                           (cons 'wrap-prefix (agent-shell-chat--draft-indent))
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
          (setq ranges (cdr ranges))))
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
    (let ((name (agent-shell-chat--agent-name))
          (ranges (agent-shell-prompt-ranges))
          (kept nil))
      (while (agent-shell-chat--search-marker-forward)
        ;; Step to the prompt this response answers: the last one before its
        ;; marker.  Markers are found in order, so the walk never backs up.
        (while (and (cadr ranges)
                    (< (map-elt (cadr ranges) :prompt-start) (match-beginning 0)))
          (setq ranges (cdr ranges)))
        (let* ((mbeg (match-beginning 0))
               (mend (match-end 0))
               (label (agent-shell-chat--label
                       name 'agent
                       (when-let* ((prompt-start (map-elt (car ranges) :prompt-start))
                                   ((< prompt-start mbeg)))
                         prompt-start)))
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
                      (if (agent-shell--prompt-face-p
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
                                     (agent-shell--prompt-face-p
                                      (get-text-property (point) 'font-lock-face))))))
          ;; A turn with no response is not labeled; its stale overlay, if
          ;; any, is dropped by the `--gc-overlays' sweep below.  Widen the
          ;; anchor back to the marker: a restored turn's overlay starts past
          ;; it (keeping the terminator visible), and the span alone would
          ;; miss the overlay it should have reused.
          (unless response-empty
            (let* ((rows (agent-shell-chat--label-rows before))
                   ;; As for the prompt label: a row to each buffer position,
                   ;; so every one is somewhere `window-start' can land.  The
                   ;; span opens on the marker, which is hidden either way and
                   ;; long enough to carry them.
                   (split (<= (length rows) (- end start)))
                   (body-start (if split (+ start (length rows)) start)))
              (when split
                (seq-do-indexed
                 (lambda (row offset)
                   (push
                    (agent-shell-chat--ensure-overlay
                     ;; Tagged as the response is, rather than with a tag of
                     ;; the rows' own: a version drawing the label whole
                     ;; sweeps by tag, and a tag it has never heard of
                     ;; survives that sweep, its relabel and even turning the
                     ;; mode off, drawing the label a second time for good.
                     ;; Sharing costs nothing: a row's span never overlaps
                     ;; the response's, and both spell out every property
                     ;; either draws with.
                     :tag 'agent
                     :beg (+ start offset) :end (+ start offset 1)
                     ;; `before-string' is spelled out to clear the label
                     ;; the version before carried whole on this overlay.
                     :props (append (agent-shell-chat--row-props row)
                                    (list (cons 'priority 100))))
                    kept))
                 rows))
              (push
               (agent-shell-chat--ensure-overlay
                :tag 'agent :beg body-start :end end
                :anchor-beg (if split body-start mbeg) :anchor-end end
                :props (list (cons 'before-string (if split "" before))
                             (cons 'display "")
                             ;; The response's first line starts a row inside
                             ;; this overlay, where the label's last row ended,
                             ;; so the row takes its prefix from here rather
                             ;; than from the body text it runs into: without
                             ;; one that line renders flush left while the rest
                             ;; of the response is indented.  Carried only
                             ;; where the label is split out into rows; with
                             ;; the label rendering here, its own rows would
                             ;; take the indent along with it.
                             (cons 'line-prefix
                                   (and split (get-text-property end 'line-prefix)))
                             (cons 'wrap-prefix
                                   (and split (get-text-property end 'wrap-prefix)))
                             ;; Sharing the rows' tag, this can be reused
                             ;; from one: clear the priority a row carries,
                             ;; which the response has no call for.
                             (cons 'priority nil)))
               kept)))))
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
covers submissions, streaming, turn completion and `session-restored'),
`shell-maker-finish-output-hook' (`clear') and
`agent-shell-section-functions' (fragments rendered with no event)."
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

    \N{U+276F}"
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'agent-shell-chat--tag 'me)
    (overlay-put overlay 'display "")
    ;; Laid out as the shell lays out its own live prompt, without its
    ;; leading pad: nothing sits above this one to separate it from.
    (overlay-put overlay 'before-string
                 (concat (agent-shell-chat--label "Me" 'user)
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
too.  A buffer-local `agent-shell-section-functions' relabels after
fragments rendered with no event, which the live prompt's label would
otherwise hide."
  (unless agent-shell-chat--labeled
    (setq-local agent-shell-chat--labeled t)
    ;; Chat mode draws its own busy frame (see `agent-shell-chat--live-marker').
    (agent-shell--clear-busy-prompt)
    (agent-shell-chat--relabel)
    (setq-local agent-shell-chat--subscription
                (agent-shell-subscribe-to
                 :shell-buffer (current-buffer)
                 :on-event #'agent-shell-chat--schedule-relabel))
    (add-hook 'shell-maker-finish-output-hook
              #'agent-shell-chat--schedule-relabel nil t)
    (add-hook 'agent-shell-section-functions
              #'agent-shell-chat--schedule-relabel nil t)
    (add-hook 'agent-shell-prompt-queue-setup-minibuffer-functions
              #'agent-shell-chat--decorate-queued-prompt)))

(defun agent-shell-chat--disable ()
  "Remove chat labels, subscription, timer and hooks from the current buffer."
  (remove-hook 'shell-maker-finish-output-hook
               #'agent-shell-chat--schedule-relabel t)
  (remove-hook 'agent-shell-section-functions
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
  (kill-local-variable 'agent-shell-chat--live-marker-overlay)
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

;; Shells labeled by an earlier version carry labels this one draws
;; differently: named by `category' rather than by a tag, or carried whole
;; on the newline above rather than a row to each buffer position.  Left
;; alone they render their label a second time.  A package upgrade reloads
;; this file into the running session (see
;; `package--reload-previously-loaded'), so relabel there and then rather
;; than leaving those shells wrong until their next turn.  Every relabel
;; heals them too (see `agent-shell-chat--ensure-overlay'), so this is the
;; first of two lines of defence rather than the only one.
;; TODO: Remove after 2026-10-09.
(dolist (buffer (buffer-list))
  (when (buffer-local-value 'agent-shell-chat--labeled buffer)
    (with-current-buffer buffer
      ;; Demoted: this runs while the package loads, so one odd shell
      ;; reports itself rather than taking the load down with it.
      (with-demoted-errors "agent-shell-chat: %S"
        (agent-shell-chat--relabel)))))

(provide 'agent-shell-chat-mode)

;;; agent-shell-chat-mode.el ends here
