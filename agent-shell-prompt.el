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
;; between turns (see `agent-shell-persistent-prompt-enabled'), so
;; everything a turn renders has to land above it.  `agent-shell--with-buffer-narrowed-to'
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
(eval-when-compile (require 'cl-lib))

(defcustom agent-shell-persistent-prompt-enabled t
  "Whether a shell keeps a writable prompt at the buffer end at all times.

When non-nil, the prompt returns as soon as a submission is dispatched
and stays for the whole turn, so there is always somewhere to type.  What
submitting into a working agent then does is up to
`agent-shell-busy-submit-default-function', which queues by default, the
way a TUI agent takes type-ahead.

Everything a turn renders lands above that prompt, pushing unsubmitted
input down rather than writing over it.

When nil, the prompt is consumed on submission and printed again once the
turn ends, so there is nowhere to type mid-turn.  The viewport's compose
buffer is there either way, and submitting from it mid-turn routes the
same."
  :type 'boolean
  :group 'agent-shell)

(defun agent-shell--live-input-prompt-p (prompt)
  "Non-nil when PROMPT is a live input prompt at the end of the buffer.
PROMPT is a `comint-last-prompt' cons of (start . end) markers.  It's
live when nothing follows it (empty input area) or when everything
between its end and `point-max' is user input rather than agent output.
This tells a real prompt awaiting input, possibly with unsubmitted typed
text, apart from a stale prompt left mid-buffer while output streams
below it (where `comint-last-prompt' still points at the previous
prompt).  Output carries a `field' of `output'; typed input does not.

A zero-length span is not a prompt.  `erase-buffer' and
`comint-clear-buffer' collapse both markers onto the same position
rather than unsetting them, and reading that as a live prompt would have
`agent-shell--finish-output' skip the prompt a cleared buffer needs."
  (when-let* ((prompt (or prompt comint-last-prompt))
              (start (marker-position (car prompt)))
              (end (marker-position (cdr prompt)))
              ((< start end))
              (max (point-max))
              ;; When narrowed above the prompt, `end' sits past the
              ;; accessible `point-max' and `text-property-any' would get
              ;; inverted bounds.  Treat that as not-live so callers fall
              ;; back to inserting at the narrowed `point-max' (still above
              ;; the prompt).
              ((<= end max)))
    (or (= end max)
        (not (text-property-any end max 'field 'output)))))

(defun agent-shell--live-prompt-start ()
  "Return the live input prompt's start marker, or nil when there is none.

Callers narrow to this position to render above the prompt, leaving the
prompt and any unsubmitted input after it below whatever they write.

Signals instead of returning nil while `agent-shell-persistent-prompt-enabled'
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
   (agent-shell-persistent-prompt-enabled
    (error "No live prompt to render above (buffer: %s).  \
Please report this at https://github.com/xenodium/agent-shell/issues.  \
Recover with M-x agent-shell-reload, which resumes the session in a fresh \
shell, or opt out via `agent-shell-persistent-prompt-enabled'"
           (buffer-name)))))

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

(defun agent-shell--prompt-input-start ()
  "Return where the live prompt's input area begins, or nil when there is none.

The area runs from there to `point-max': a live prompt has nothing but
typed text after it (see `agent-shell--live-input-prompt-p')."
  (when-let* ((prompt comint-last-prompt)
              ((agent-shell--live-input-prompt-p prompt)))
    (marker-position (cdr prompt))))

(defun agent-shell--prompt-input ()
  "Return the text typed at the live prompt, trimmed, or nil when empty.

Reads without disturbing the buffer, so a caller can decide whether the
prompt is going anywhere before clearing it with
`agent-shell--clear-prompt-input'.  Keeps text properties, including
pasted image previews, when the prompt is queued or steered.

For example, in a buffer ending with

  Claude> list the files

returns \"list the files\"."
  (when-let* ((start (agent-shell--prompt-input-start))
              ((< start (point-max)))
              (input (string-trim (buffer-substring start (point-max))))
              ((not (string-empty-p input))))
    input))

(defun agent-shell--clear-prompt-input ()
  "Clear what is typed at the live prompt, leaving the prompt itself.

The shell keeps somewhere to type, so a buffer ending

  Claude> list the files

ends

  Claude>"
  (when-let* ((start (agent-shell--prompt-input-start))
              ((< start (point-max))))
    (delete-region start (point-max))))

(defun agent-shell--take-prompt-input ()
  "Clear and return the text typed at the live prompt, or nil when empty.

`agent-shell--prompt-input' and `agent-shell--clear-prompt-input' in one
step, for callers that want the text out of the way before they act.
Callers that might not act at all should use the two separately, so what
the user typed survives untouched when nothing comes of it."
  (prog1 (agent-shell--prompt-input)
    (agent-shell--clear-prompt-input)))

(defun agent-shell--point-in-live-input-p ()
  "Non-nil when point sits in the live prompt's input area.

The input area runs from the live prompt's end to `point-max': a live
prompt has nothing but typed text after it (see
`agent-shell--live-input-prompt-p').  Nil when the prompt nearest the
buffer end is stale, which is what a shell mid-turn looks like without
`agent-shell-persistent-prompt-enabled'.

For example, with the buffer ending in `Claude> list', point anywhere
from just after `Claude> ' to `point-max' answers non-nil, and point up
in the transcript above answers nil."
  (when-let* ((prompt comint-last-prompt)
              ((agent-shell--live-input-prompt-p prompt))
              (input-start (marker-position (cdr prompt))))
    (>= (point) input-start)))

(defun agent-shell--print-prompt ()
  "Print a prompt at the end of the shell buffer.

Goes through `shell-maker--output-filter' so the prompt is fontified and
bracketed by `comint-last-prompt' exactly as one shell-maker prints
itself; nothing else sets that up, and `agent-shell--live-input-prompt-p'
reads it."
  (shell-maker--output-filter (shell-maker--process)
                              (shell-maker-prompt shell-maker--config)))

(cl-defun agent-shell--finish-output (&key config success)
  "Finish a turn in CONFIG's shell, recording SUCCESS.

Hands off to `shell-maker-finish-output' whenever the shell needs a
prompt printed -- a fresh buffer, one just cleared, or a turn that
consumed its prompt on submission.

Takes over only when a prompt is already waiting for input, which is
what `agent-shell-persistent-prompt-enabled' arranges.  Printing then would
stack a second prompt below the first and below anything typed at it, so
this does the rest of what `shell-maker-finish-output' does and no more:
clears the busy flag, records the input ring on SUCCESS, and runs
`shell-maker-finish-output-hook' so observers redrawing on a settled
buffer (chat mode's labels) still fire.

Keep that branch in step with `shell-maker-finish-output'."
  (if (not (agent-shell--live-input-prompt-p comint-last-prompt))
      (shell-maker-finish-output :config config :success success)
    (setq shell-maker--busy nil)
    (when success
      (shell-maker--write-input-ring-history config))
    (run-hooks 'shell-maker-finish-output-hook)))

(provide 'agent-shell-prompt)

;;; agent-shell-prompt.el ends here
