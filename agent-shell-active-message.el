;;; agent-shell-active-message.el --- Active message utilities -*- lexical-binding: t; -*-

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
;; Provides a minibuffer progress message for agent-shell.

;;; Code:

(require 'map)

(eval-when-compile
  (require 'cl-lib))

(cl-defun agent-shell-active-message-show (&key text)
  "Show a minibuffer active message displaying TEXT.

Returns an active message alist for use with
`agent-shell-active-message-hide'."
  (let* ((reporter (make-progress-reporter (or text "Loading...")))
         (timer (run-at-time 0 0.1
                             (lambda ()
                               (progress-reporter-update reporter)))))
    (list (cons :reporter reporter)
          (cons :timer timer))))

(cl-defun agent-shell-active-message-hide (&key active-message)
  "Hide ACTIVE-MESSAGE previously shown with
`agent-shell-active-message-show'."
  (when active-message
    (when-let* ((timer (map-elt active-message :timer)))
      (when (timerp timer)
        (cancel-timer timer)))
    (when-let* ((reporter (map-elt active-message :reporter)))
      (progress-reporter-done reporter)
      (message nil))))

(provide 'agent-shell-active-message)

;;; agent-shell-active-message.el ends here
