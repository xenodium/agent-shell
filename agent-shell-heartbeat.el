;;; agent-shell-heartbeat.el --- Heartbeat utilities -*- lexical-binding: t; -*-

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
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Commentary:
;;
;; Provides heartbeat functionality for agent-shell.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)

(cl-defun agent-shell-heartbeat-make (&key start (beats-per-second 10) on-heartbeat)
  "Create a heartbeat timer.

START when non-nil, starts internal timer immediately.
BEATS-PER-SECOND determines the timer frequency.
ON-HEARTBEAT is a callback function invoked on each timer tick.

Of the form:

  (lambda (heartbeat status)
    ...)

STATUS is one of: `started', `busy', or `ended'.
HEARTBEAT is nil when status is `started' or `ended', otherwise current value.

Returns an alist of the form:

  ((:heartbeat-timer)
   (:beats-per-second)
   (:on-heartbeat)
   (:value)
   (:status))"
  (unless on-heartbeat
    (error "Missing required argument: :on-heartbeat"))
  (let* ((heartbeat-state (list (cons :heartbeat-timer nil)
                                (cons :beats-per-second beats-per-second)
                                (cons :on-heartbeat on-heartbeat)
                                (cons :value nil)
                                (cons :status nil))))
    (when start
      (agent-shell-heartbeat-start :heartbeat heartbeat-state))
    heartbeat-state))

(cl-defun agent-shell-heartbeat-start (&key heartbeat)
  "Start a heartbeat timer if not already started.

HEARTBEAT is the heartbeat state alist.

Returns the heartbeat alist with the timer started."
  (when heartbeat
    (unless (and (map-elt heartbeat :heartbeat-timer)
                 (timerp (map-elt heartbeat :heartbeat-timer)))
      (map-put! heartbeat :status 'started)
      (map-put! heartbeat :value 0)
      (funcall (map-elt heartbeat :on-heartbeat)
               (map-elt heartbeat :value)
               (map-elt heartbeat :status))
      (map-put! heartbeat :heartbeat-timer
                (run-at-time 0 (/ 1.0 (map-elt heartbeat :beats-per-second))
                             (lambda ()
                               (when-let* ((value (map-elt heartbeat :value)))
                                 (map-put! heartbeat :status 'busy)
                                 (map-put! heartbeat :value (1+ value))
                                 (funcall (map-elt heartbeat :on-heartbeat)
                                          (map-elt heartbeat :value)
                                          (map-elt heartbeat :status)))))))
    heartbeat))

(cl-defun agent-shell-heartbeat-stop (&key heartbeat)
  "Stop a heartbeat timer.

HEARTBEAT is the heartbeat state alist.

Returns the heartbeat with latest state."
  (when heartbeat
    (when-let* ((timer (map-elt heartbeat :heartbeat-timer)))
      (when (timerp timer)
        (cancel-timer timer))
      (map-put! heartbeat :heartbeat-timer nil))
    (map-put! heartbeat :status 'ended)
    (map-put! heartbeat :value nil)
    (when-let* ((callback (map-elt heartbeat :on-heartbeat)))
      (funcall callback nil 'ended))
    heartbeat))

(provide 'agent-shell-heartbeat)

;;; agent-shell-heartbeat.el ends here
