;;; agent-shell-block-motion.el --- Move by blocks with beginning-of-defun. -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alvaro Ramirez

;; Author: Lex https://github.com/OSadovy
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
;; Provides `beginning-of-defun-function' and `end-of-defun-function'
;; for shell and viewport buffers, so `C-M-a' and `C-M-e' move by
;; block: a message, thought, tool call, plan, submitted prompt, source
;; block or table.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-chat-mode)
(require 'agent-shell-prompt)
(require 'agent-shell-ui)

(defconst agent-shell--block-properties
  '(agent-shell-ui-state
    agent-shell-markdown-source-block-body
    agent-shell-markdown-table-source
    font-lock-face)
  "Text properties marking the blocks `beginning-of-defun' moves by.
`font-lock-face' is where comint marks submitted prompts.")

(defun agent-shell--property-range (position property)
  "Return the run of PROPERTY holding POSITION, or nil.

For example, with PROPERTY on the three characters from 5, a POSITION
of 6 returns:

  ((:start . 5)
   (:end . 8))"
  (when (get-text-property position property)
    `((:start . ,(previous-single-property-change
                  (1+ position) property nil (point-min)))
      (:end . ,(next-single-property-change
                position property nil (point-max))))))

(defun agent-shell--submitted-prompt-range (position)
  "Return the range of the submitted prompt holding POSITION, or nil.

In the form:

  ((:start . PROMPT-LINE-START)
   (:end . INPUT-END))

Submitted input is the text comint highlights with
`comint-highlight-input'; the prompt still being composed has no such
text, so it is never one."
  (let ((input (if (agent-shell--prompt-face-p
                    (get-text-property position 'font-lock-face))
                   ;; On the prompt text, the input starts where it ends.
                   (next-single-property-change
                    position 'font-lock-face nil (point-max))
                 position)))
    (when-let* (((memq 'comint-highlight-input
                       (ensure-list (get-text-property input 'font-lock-face))))
                (range (agent-shell--property-range input 'font-lock-face)))
      `((:start . ,(save-excursion
                     (goto-char (map-elt range :start))
                     (pos-bol)))
        (:end . ,(map-elt range :end))))))

(defun agent-shell--block-ranges-at (position)
  "Return the ranges of the blocks holding the character at POSITION.

Blocks are rendered fragments (messages, thoughts, tool calls, plans
and the like), fenced source blocks, tables and submitted prompts.
Ranges are listed outermost first.  Only a fragment holds other
blocks, so listing it first is all that order takes.

For example, inside a source block in a message, returns:

  (((:start . MESSAGE-START) (:end . MESSAGE-END))
   ((:start . BLOCK-START) (:end . BLOCK-END)))"
  (when (< position (point-max))
    (delq nil
          (list
           (when (get-text-property position 'agent-shell-ui-state)
             ;; `agent-shell-ui--block-range' searches from point.
             (save-excursion
               (goto-char position)
               (agent-shell-ui--block-range :position position)))
           (agent-shell--property-range
            position 'agent-shell-markdown-source-block-body)
           (agent-shell--property-range
            position 'agent-shell-markdown-table-source)
           (agent-shell--submitted-prompt-range position)))))

(defun agent-shell--block-boundary (position direction)
  "Return the nearest change of a block property from POSITION.
DIRECTION is `forward' or `backward'."
  (if (eq direction 'forward)
      (seq-min (seq-map (lambda (property)
                          (next-single-char-property-change position property))
                        agent-shell--block-properties))
    (seq-max (seq-map (lambda (property)
                        (previous-single-char-property-change position property))
                      agent-shell--block-properties))))

(defun agent-shell--previous-block-start (position)
  "Return the start of the block before POSITION, or nil.

Inside a block, that is its own start, the innermost one's where
blocks nest.  Otherwise it is the start of the last visible block
ending before POSITION, the outermost one's, so a source block that
closes a message yields the message."
  (if-let* ((enclosing (seq-filter (lambda (range)
                                     (< (map-elt range :start) position))
                                   (agent-shell--block-ranges-at position))))
      (map-elt (car (last enclosing)) :start)
    (let ((pos position)
          start)
      (while (and (not start) (> pos (point-min)))
        (if-let* ((outermost (car (agent-shell--block-ranges-at (1- pos)))))
            (if (invisible-p (map-elt outermost :start))
                ;; In a collapsed group: carry on above it.
                (setq pos (map-elt outermost :start))
              (setq start (map-elt outermost :start)))
          (setq pos (agent-shell--block-boundary pos 'backward))))
      start)))

(defun agent-shell--next-block-start (position)
  "Return the start of the next visible block after POSITION, or nil.

Blocks nested in one holding POSITION count, so a source block further
down the message at point comes next.  Others are passed over with the
block holding them."
  (let ((pos position)
        start)
    (while (and (not start) (< pos (point-max)))
      (let ((later (seq-find (lambda (range)
                               (> (map-elt range :start) position))
                             (agent-shell--block-ranges-at pos))))
        (cond
         ((null later)
          (setq pos (agent-shell--block-boundary pos 'forward)))
         ((invisible-p (map-elt later :start))
          (setq pos (map-elt later :end)))
         (t
          (setq start (map-elt later :start))))))
    start))

(defun agent-shell--beginning-of-block (&optional arg)
  "Move to the start of the ARGth block back, or forward if ARG is negative.

As `beginning-of-defun-function', this makes \\[beginning-of-defun]
and \\[end-of-defun] move by blocks: a message, thought, tool call,
plan, submitted prompt, source block or table.  Return non-nil when
point moved by all of ARG."
  (let ((block-start (if (< (or arg 1) 0)
                         #'agent-shell--next-block-start
                       #'agent-shell--previous-block-start))
        (remaining (abs (or arg 1)))
        start)
    (while (and (> remaining 0)
                (setq start (funcall block-start (point))))
      (goto-char start)
      (setq remaining (1- remaining)))
    (zerop remaining)))

(defun agent-shell--end-of-block ()
  "Move to the end of the block starting at point.
Failing that, to the end of the innermost block holding point.  Used
as `end-of-defun-function'."
  (when-let* ((ranges (agent-shell--block-ranges-at (point)))
              (range (or (seq-find (lambda (range)
                                     (= (map-elt range :start) (point)))
                                   ranges)
                         (car (last ranges)))))
    (goto-char (map-elt range :end))))

(provide 'agent-shell-block-motion)

;;; agent-shell-block-motion.el ends here
