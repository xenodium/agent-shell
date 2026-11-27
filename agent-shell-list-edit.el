;;; agent-shell-list-edit.el --- Markdown list editing minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alvaro Ramirez

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
;; A small minor mode for editing markdown-style bullet and numbered
;; lists.  RET continues the current item (or breaks out when the item
;; is empty), TAB indents, and `<backtab>' de-indents.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; Support the work https://github.com/sponsors/xenodium

;;; Code:

(require 'map)
(require 'subr-x)

(defconst agent-shell-list-edit--indent-step 2
  "Number of spaces used when indenting/de-indenting a list item.")

(defconst agent-shell-list-edit--bullet-re
  (rx line-start
      (group (zero-or-more (any " \t")))
      (group (any "-*+"))
      " "
      (group (zero-or-more not-newline)))
  "Regexp matching a markdown bullet list item.
Group 1 is the leading indent, group 2 is the bullet character,
group 3 is the item content.")

(defconst agent-shell-list-edit--numbered-re
  (rx line-start
      (group (zero-or-more (any " \t")))
      (group (one-or-more digit))
      "."
      " "
      (group (zero-or-more not-newline)))
  "Regexp matching a markdown numbered list item.
Group 1 is the leading indent, group 2 is the number,
group 3 is the item content.")

(defun agent-shell-list-edit--at-item ()
  "Return an alist describing the list item on the current line, or nil.

The alist contains :type (\\='bullet or \\='numbered), :indent, :marker
and :content.

For example, a line \"  - hello\" returns:

  ((:type . bullet) (:indent . \"  \") (:marker . \"-\") (:content . \"hello\"))

And \"1. step\" returns:

  ((:type . numbered) (:indent . \"\") (:marker . \"1\") (:content . \"step\"))"
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at agent-shell-list-edit--bullet-re)
      `((:type . bullet)
        (:indent . ,(match-string-no-properties 1))
        (:marker . ,(match-string-no-properties 2))
        (:content . ,(match-string-no-properties 3))))
     ((looking-at agent-shell-list-edit--numbered-re)
      `((:type . numbered)
        (:indent . ,(match-string-no-properties 1))
        (:marker . ,(match-string-no-properties 2))
        (:content . ,(match-string-no-properties 3)))))))

(defun agent-shell-list-edit-newline ()
  "Insert a newline, continuing the current list item if applicable.

On an empty list item, remove the whole prefix (indent + marker) and
break out of the list."
  (declare (modes agent-shell-list-edit-mode))
  (interactive)
  (let ((item (agent-shell-list-edit--at-item)))
    (cond
     ((and item (string-empty-p (string-trim (map-elt item :content))))
      (beginning-of-line)
      (delete-region (point) (line-end-position))
      (newline))
     (item
      (newline)
      (pcase (map-elt item :type)
        ('bullet
         (insert (map-elt item :indent)
                 (map-elt item :marker)
                 " "))
        ('numbered
         (insert (map-elt item :indent)
                 (number-to-string
                  (1+ (string-to-number (map-elt item :marker))))
                 ". "))))
     (t (newline)))))

(defun agent-shell-list-edit-indent-line ()
  "Indent the current list item.

Adds `agent-shell-list-edit--indent-step' spaces at the start of the
line.  When point is not on a list item, falls back to
`indent-for-tab-command'."
  (declare (modes agent-shell-list-edit-mode))
  (interactive)
  (if (agent-shell-list-edit--at-item)
      (save-excursion
        (beginning-of-line)
        (insert (make-string agent-shell-list-edit--indent-step ?\s)))
    (indent-for-tab-command)))

(defun agent-shell-list-edit-dedent-line ()
  "De-indent the current list item.

Removes `agent-shell-list-edit--indent-step' spaces from the start of
the line.  When point is not on an indented list item, does nothing."
  (declare (modes agent-shell-list-edit-mode))
  (interactive)
  (when-let* ((indent (map-elt (agent-shell-list-edit--at-item) :indent))
              ((>= (length indent) agent-shell-list-edit--indent-step)))
    (save-excursion
      (beginning-of-line)
      (delete-char agent-shell-list-edit--indent-step))))

(defvar agent-shell-list-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-shell-list-edit-newline)
    (define-key map (kbd "TAB") #'agent-shell-list-edit-indent-line)
    (define-key map (kbd "<backtab>") #'agent-shell-list-edit-dedent-line)
    map)
  "Keymap for `agent-shell-list-edit-mode'.")

(define-minor-mode agent-shell-list-edit-mode
  "Minor mode for editing markdown-style lists.

Continues bullet (\"- \", \"* \", \"+ \") and numbered (\"1. \") items
on RET.  Pressing RET on an empty list item removes the marker and
breaks out of the list.  TAB and \\`<backtab>' indent and de-indent
the current item."
  :lighter " ListEdit"
  :interactive (agent-shell-viewport-edit-mode)
  :keymap agent-shell-list-edit-mode-map)

(provide 'agent-shell-list-edit)

;;; agent-shell-list-edit.el ends here
