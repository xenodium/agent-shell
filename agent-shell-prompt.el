;;; agent-shell-prompt.el --- Live prompt handling for agent-shell. -*- lexical-binding: t; -*-

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
;; Locate the live prompt at the end of an `agent-shell' buffer, take what
;; has been typed into it, and render above it.
;;
;; A shell keeps a prompt at the buffer end for the whole turn, not just
;; between turns (see `agent-shell--persistent-prompt'), so everything a
;; turn renders has to land above it.  `agent-shell--with-buffer-narrowed-to'
;; is how callers do that.
;;
;; Its own file so the macro is defined before the files expanding it are
;; compiled: `agent-shell-experimental' cannot require `agent-shell', which
;; requires it in turn.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'comint)
(require 'shell-maker)

(defvar agent-shell--persistent-prompt t
  "When non-nil, keep a writable prompt at the end of the shell at all times.

The prompt returns as soon as a submission is dispatched and stays for the
whole turn, so there is always somewhere to type.  Submitting from it
while the agent is busy queues the text (see `agent-shell-prompt-queue')
and clears the input, the way a TUI agent takes type-ahead.

Everything a turn renders then lands above that prompt, pushing
unsubmitted input down rather than writing over it.  Writing below it is
a bug, and both packages assert rather than let it happen quietly (see
`agent-shell--live-prompt-start' and `shell-maker-persistent-prompt').

Private while the feature settles.  Set it to nil to have the prompt
consumed on submission and printed again once the turn ends.")

(defun agent-shell--live-input-prompt-p (prompt)
  "Non-nil when PROMPT is a live input prompt at the end of the buffer.
PROMPT is a `comint-last-prompt' cons of (start . end) markers.  It's
live when nothing follows it (empty input area) or when everything
between its end and `point-max' is user input rather than agent output.
This tells a real prompt awaiting input, possibly with unsubmitted typed
text, apart from a stale prompt left mid-buffer while output streams
below it (where `comint-last-prompt' still points at the previous
prompt).  Output carries a `field' of `output'; typed input does not."
  (shell-maker-live-prompt-p prompt))

(defun agent-shell--live-prompt-start ()
  "Return the live input prompt's start marker, or nil when there is none.

Callers narrow to this position to render above the prompt, leaving the
prompt and any unsubmitted input after it below whatever they write.

Signals instead of returning nil while `agent-shell--persistent-prompt'
is on.  Everything renders above the prompt in that mode, so a missing
prompt means the next write lands in the input area, past text the user
is in the middle of typing.  Failing here names the write that lost the
prompt, rather than leaving a shell that scribbles over its own input."
  (cond
   ;; A caller further up the stack already narrowed above the prompt, so
   ;; the accessible `point-max' is the prompt's start.  There is nothing
   ;; left to push down, and nothing can land below the prompt from here,
   ;; so this is not the missing prompt the assert below is looking for.
   ((and comint-last-prompt
         (>= (marker-position (car comint-last-prompt)) (point-max)))
    nil)
   ((agent-shell--live-input-prompt-p comint-last-prompt)
    (car comint-last-prompt))
   (agent-shell--persistent-prompt
    (error "No live prompt to render above (buffer: %s)" (buffer-name)))))

(defmacro agent-shell--with-buffer-narrowed-to (prompt-start &rest body)
  "Run BODY with the buffer narrowed to everything before PROMPT-START.

PROMPT-START is a marker, typically a live prompt's start as returned by
`agent-shell--live-prompt-start'.  Pass nil to run BODY unnarrowed.  A
marker rather than a position, because the flip below needs one.

Narrowing puts `point-max' at the marker, so whatever BODY appends lands
before it.  The marker is flipped to rear-advancing for the duration, so
it (and a live prompt's unsubmitted input, which trails it) is pushed
down past the new text rather than stranded inside it.  Restored
afterwards: a marker left rear-advancing would swallow the next
character typed at the prompt.

Point is put back too.  Narrowing clamps it to the marker, and a BODY
that appends leaves it there, so once the restriction lifts point would
sit on the prompt with anything typed at it stranded ahead -- the cursor
jumping away mid-sentence.  A marker carries point through the text
inserted above it.

For example, in a buffer ending `answer\nClaude> typed', with
PROMPT-START at the `C', a BODY inserting `more' leaves
`answer\nmore\nClaude> typed'."
  (declare (indent 1) (debug (form body)))
  (let ((start (gensym "prompt-start"))
        (insertion-type (gensym "insertion-type"))
        (saved-point (gensym "saved-point")))
    `(let* ((,start ,prompt-start)
            (,insertion-type (and ,start (marker-insertion-type ,start)))
            (,saved-point (and ,start (copy-marker (point)))))
       (when ,start
         (set-marker-insertion-type ,start t))
       (unwind-protect
           (save-restriction
             (when ,start
               (narrow-to-region (point-min) (marker-position ,start)))
             ,@body)
         (when ,start
           (set-marker-insertion-type ,start ,insertion-type))
         (when ,saved-point
           (goto-char ,saved-point)
           (set-marker ,saved-point nil))))))

(defun agent-shell--take-prompt-input ()
  "Delete and return the text typed at the live prompt, or nil when empty.

The live prompt's input area runs from the prompt's end to `point-max':
a live prompt has nothing but typed text after it (see
`agent-shell--live-input-prompt-p').  Returns the text trimmed, and
leaves the prompt itself in place, so the shell keeps somewhere to type.

For example, in a buffer ending with

  Claude> list the files

leaves it ending with

  Claude>

and returns \"list the files\".  Returns nil for an empty or
whitespace-only input area, having cleared it all the same."
  (when-let* ((prompt comint-last-prompt)
              ((agent-shell--live-input-prompt-p prompt))
              (start (marker-position (cdr prompt)))
              ((< start (point-max)))
              (input (string-trim (buffer-substring-no-properties start (point-max)))))
    (delete-region start (point-max))
    (unless (string-empty-p input)
      input)))

(defun agent-shell--point-in-live-input-p ()
  "Non-nil when point sits in the live prompt's input area.

The input area runs from the live prompt's end to `point-max': a live
prompt has nothing but typed text after it (see
`agent-shell--live-input-prompt-p').  Nil when the prompt nearest the
buffer end is stale, which is what a shell mid-turn looks like without
`agent-shell--persistent-prompt'.

For example, with the buffer ending in `Claude> list', point anywhere
from just after `Claude> ' to `point-max' answers non-nil, and point up
in the transcript above answers nil."
  (when-let* ((prompt comint-last-prompt)
              ((agent-shell--live-input-prompt-p prompt))
              (input-start (marker-position (cdr prompt))))
    (>= (point) input-start)))

(provide 'agent-shell-prompt)

;;; agent-shell-prompt.el ends here
