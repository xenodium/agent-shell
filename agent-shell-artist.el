;;; agent-shell-artist.el --- Scratch artist buffer with fenced insert. -*- lexical-binding: t; -*-

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

;;; Code:

(require 'artist)
(require 'map)
(require 'ring)
(require 'transient)

(defvar-local agent-shell-artist--target-buffer nil
  "Buffer to insert the art into when committing.")

(defvar-local agent-shell-artist--target-point nil
  "Insertion point in the target buffer.")

(defvar agent-shell-artist-history-ring (make-ring 32)
  "Ring of previously committed art strings (newest at index 0).")

(defvar-local agent-shell-artist--history-pos nil
  "Current position in `agent-shell-artist-history-ring'.

nil if not currently navigating history.")

(defvar agent-shell-artist--op-info
  '((pen-line       . ((:key . "p") (:label . "Pen")            (:category . "Draw")  (:column . draw)  (:command . artist-select-op-pen-line)))
    (line           . ((:key . "l") (:label . "Line")           (:category . "Draw")  (:column . draw)  (:command . artist-select-op-line)))
    (s-line         . ((:key . "L") (:label . "Straight line")  (:category . "Draw")  (:column . draw)  (:command . artist-select-op-straight-line)))
    (rect           . ((:key . "r") (:label . "Rectangle")      (:category . "Draw")  (:column . draw)  (:command . artist-select-op-rectangle)))
    (square         . ((:key . "R") (:label . "Square")         (:category . "Draw")  (:column . draw)  (:command . artist-select-op-square)))
    (ellipse        . ((:key . "e") (:label . "Ellipse")        (:category . "Draw")  (:column . draw)  (:command . artist-select-op-ellipse)))
    (circle         . ((:key . "o") (:label . "Circle")         (:category . "Draw")  (:column . draw)  (:command . artist-select-op-circle)))
    (polyline       . ((:key . "z") (:label . "Poly-line")      (:category . "Draw")  (:column . draw)  (:command . artist-select-op-poly-line)))
    (spolyline      . ((:key . "Z") (:label . "Straight poly")  (:category . "Draw")  (:column . draw)  (:command . artist-select-op-straight-poly-line)))
    (text-thru      . ((:key . "t") (:label . "Text see-thru")  (:category . "Type")  (:column . draw)  (:command . artist-select-op-text-see-thru)))
    (text-ovwrt     . ((:key . "T") (:label . "Text overwrite") (:category . "Type")  (:column . draw)  (:command . artist-select-op-text-overwrite)))
    (spray-can      . ((:key . "s") (:label . "Spray can")      (:category . "Spray") (:column . draw)  (:command . artist-select-op-spray-can)))
    (flood-fill     . ((:key . "f") (:label . "Flood fill")     (:category . "Fill")  (:column . draw)  (:command . artist-select-op-flood-fill)))
    (erase-char     . ((:key . "d") (:label . "Char")           (:category . "Erase") (:column . erase) (:command . artist-select-op-erase-char)))
    (erase-rect     . ((:key . "D") (:label . "Rectangle")      (:category . "Erase") (:column . erase) (:command . artist-select-op-erase-rectangle)))
    (vaporize-line  . ((:key . "v") (:label . "Line")           (:category . "Erase") (:column . erase) (:command . artist-select-op-vaporize-line)))
    (vaporize-lines . ((:key . "V") (:label . "Connected lines") (:category . "Erase") (:column . erase) (:command . artist-select-op-vaporize-lines)))
    (cut-r          . ((:key . "x") (:label . "Cut rectangle")  (:category . "Cut")   (:column . clip)  (:command . artist-select-op-cut-rectangle)))
    (copy-r         . ((:key . "w") (:label . "Copy rectangle") (:category . "Copy")  (:column . clip)  (:command . artist-select-op-copy-rectangle)))
    (paste          . ((:key . "y") (:label . "Paste")          (:category . "Paste") (:column . clip)  (:command . artist-select-op-paste))))
  "Alist mapping `artist-curr-go' symbol to a property alist.
Each entry's value alist holds :key, :label, :category, :column, :command.

For example, the entry for the rectangle op is:
  (rect . ((:key . \"r\") (:label . \"Rectangle\") (:category . \"Draw\")
           (:column . draw) (:command . artist-select-op-rectangle)))")

(defvar agent-shell-artist--toggle-info
  '((artist-rubber-banding    . ((:key . "b") (:label . "Rubber-banding")
                                 (:command . artist-toggle-rubber-banding)))
    (artist-borderless-shapes . ((:key . "B") (:label . "Borderless shapes")
                                 (:command . artist-toggle-borderless-shapes)))
    (artist-trim-line-endings . ((:key . "i") (:label . "Trim line endings")
                                 (:command . artist-toggle-trim-line-endings))))
  "Alist mapping toggle variable symbol to a property alist.
Each entry's value alist holds :key, :label, :command.")

(defun agent-shell-artist--current-op-info ()
  "Return (CATEGORY . LABEL) for the current artist op."
  (if-let* ((props (map-elt agent-shell-artist--op-info artist-curr-go)))
      (cons (map-elt props :category) (map-elt props :label))
    (cons "Tool" (symbol-name artist-curr-go))))

(defvar agent-shell-artist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-shell-artist-commit)
    (define-key map (kbd "C-c C-k") #'agent-shell-artist-cancel)
    (define-key map (kbd "M-p") #'agent-shell-artist-previous-history)
    (define-key map (kbd "M-n") #'agent-shell-artist-next-history)
    (define-key map (kbd "C-c C-h") #'agent-shell-artist-help)
    map)
  "Keymap for `agent-shell-artist-mode'.")

(define-minor-mode agent-shell-artist-mode
  "Minor mode for `agent-shell' scratch artist buffers.

Adds commit/cancel bindings on top of `artist-mode'."
  :lighter " AS-Art"
  :interactive (artist-mode)
  :keymap agent-shell-artist-mode-map)

(defun agent-shell-artist-insert-text-drawing ()
  "Open the scratch artist buffer to draw and insert text from."
  (interactive)
  (let ((target (current-buffer))
        (point (point))
        (buf (get-buffer-create "*agent-shell-artist*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (unless (bound-and-true-p artist-mode)
        (artist-mode 1))
      (artist-select-op-rectangle)
      (unless (bound-and-true-p agent-shell-artist-mode)
        (agent-shell-artist-mode 1))
      (setq agent-shell-artist--target-buffer target
            agent-shell-artist--target-point point
            agent-shell-artist--history-pos nil)
      (setq header-line-format
            `("  "
              (:eval (let ((info (agent-shell-artist--current-op-info)))
                       (concat
                        (propertize (concat (car info) ":")
                                    'face 'mode-line-emphasis)
                        " "
                        (cdr info)
                        "  ")))
              ,(substitute-command-keys
                (concat "\\<agent-shell-artist-mode-map>"
                        "\\[agent-shell-artist-commit] exit and insert  "
                        "\\[agent-shell-artist-cancel] cancel  "
                        "\\[agent-shell-artist-previous-history] previous  "
                        "\\[agent-shell-artist-next-history] next  "
                        "\\[agent-shell-artist-help] help")))))
    (pop-to-buffer buf)))

(defun agent-shell-artist--ops-in-column (column)
  "Return entries of `agent-shell-artist--op-info' belonging to COLUMN."
  (seq-filter (lambda (entry)
                (eq (map-elt (cdr entry) :column) column))
              agent-shell-artist--op-info))

(defun agent-shell-artist--column-pad (entries)
  "Return one column wider than the longest :label in ENTRIES.

For example:
  (agent-shell-artist--column-pad
   \\='((rect . ((:label . \"Rectangle\")))
     (line . ((:label . \"Line\")))))
  => 10"
  (1+ (seq-max
       (seq-map (lambda (entry) (length (map-elt (cdr entry) :label)))
                entries))))

(defun agent-shell-artist--mark-active (active label pad)
  "Pad LABEL to PAD columns and append `[✓]' when ACTIVE, `[ ]' otherwise.

For example:
  (agent-shell-artist--mark-active t \"Rectangle\" 11)
  => \"Rectangle   [✓]\""
  (concat (format (format "%%-%ds" pad) label)
          (if active "[✓]" "[ ]")))

(defun agent-shell-artist--op-suffix (entries)
  "Return transient suffix specs built from op ENTRIES.

ENTRIES is an `agent-shell-artist--op-info'-shaped alist (SYM . PROPS).
Each result is (KEY DESC-FN COMMAND) ready to splice into
`transient-define-prefix'."
  (let ((pad (agent-shell-artist--column-pad entries)))
    (seq-map (lambda (entry)
               (let ((sym (car entry))
                     (props (cdr entry)))
                 `(,(map-elt props :key)
                   (lambda ()
                     (agent-shell-artist--mark-active
                      (eq artist-curr-go ',sym)
                      ,(map-elt props :label)
                      ,pad))
                   ,(map-elt props :command))))
             entries)))

(defun agent-shell-artist--toggle-suffix (entries)
  "Return transient suffix specs built from toggle ENTRIES.

ENTRIES is an `agent-shell-artist--toggle-info'-shaped alist (VAR . PROPS).
Each result is (KEY DESC-FN COMMAND :transient t)."
  (let ((pad (agent-shell-artist--column-pad entries)))
    (seq-map (lambda (entry)
               (let ((var (car entry))
                     (props (cdr entry)))
                 `(,(map-elt props :key)
                   (lambda ()
                     (agent-shell-artist--mark-active
                      (symbol-value ',var)
                      ,(map-elt props :label)
                      ,pad))
                   ,(map-elt props :command) :transient t)))
             entries)))

(eval
 `(transient-define-prefix agent-shell-artist-help ()
    "Show all interactive commands for `agent-shell-artist-mode'."
    [["Buffer"
      ("C-c C-c" "Commit and insert" agent-shell-artist-commit)
      ("C-c C-k" "Cancel" agent-shell-artist-cancel)]
     ["History"
      ("M-p" "Previous art" agent-shell-artist-previous-history)
      ("M-n" "Next art" agent-shell-artist-next-history)]
     ["Toggles"
      ,@(agent-shell-artist--toggle-suffix
         agent-shell-artist--toggle-info)]]
    [["Drawing"
      ,@(agent-shell-artist--op-suffix
         (agent-shell-artist--ops-in-column 'draw))]
     ["Erase"
      ,@(agent-shell-artist--op-suffix
         (agent-shell-artist--ops-in-column 'erase))]
     ["Clipboard"
      ,@(agent-shell-artist--op-suffix
         (agent-shell-artist--ops-in-column 'clip))]]
    (interactive nil agent-shell-artist-mode)
    (transient-setup 'agent-shell-artist-help)))

(defun agent-shell-artist--show-history-at (pos)
  "Replace buffer contents with history ring entry at POS."
  (let ((art (ring-ref agent-shell-artist-history-ring pos))
        (inhibit-read-only t))
    (erase-buffer)
    (insert art)
    (goto-char (point-min))))

(defun agent-shell-artist-previous-history ()
  "Show older art from `agent-shell-artist-history-ring'."
  (declare (modes agent-shell-artist-mode))
  (interactive)
  (let ((len (ring-length agent-shell-artist-history-ring))
        (next (if agent-shell-artist--history-pos
                  (1+ agent-shell-artist--history-pos)
                0)))
    (cond ((zerop len)
           (user-error "Artist history is empty"))
          ((>= next len)
           (user-error "Beginning of artist history"))
          (t
           (setq agent-shell-artist--history-pos next)
           (agent-shell-artist--show-history-at next)))))

(defun agent-shell-artist-next-history ()
  "Show newer art from `agent-shell-artist-history-ring'."
  (declare (modes agent-shell-artist-mode))
  (interactive)
  (let ((len (ring-length agent-shell-artist-history-ring))
        (next (and agent-shell-artist--history-pos
                   (1- agent-shell-artist--history-pos))))
    (cond ((zerop len)
           (user-error "Artist history is empty"))
          ((or (null next) (< next 0))
           (user-error "End of artist history"))
          (t
           (setq agent-shell-artist--history-pos next)
           (agent-shell-artist--show-history-at next)))))

(defun agent-shell-artist-commit ()
  "Insert scratch art into the calling buffer wrapped in a fence."
  (declare (modes agent-shell-artist-mode))
  (interactive)
  (unless (buffer-live-p agent-shell-artist--target-buffer)
    (user-error "Calling buffer is gone"))
  (let* ((art (string-trim (buffer-substring-no-properties
                            (point-min) (point-max))
                           "\\(?:[ \t]*\n\\)+"
                           "[ \t\n]+"))
         (target agent-shell-artist--target-buffer)
         ;; Marker with insertion-type nil so it stays before the
         ;; inserted text.  Used to anchor both buffer-point and the
         ;; target window's window-point after the insert, since some
         ;; major modes (comint-based shells) otherwise leave point
         ;; after the insertion.
         (anchor (with-current-buffer target (copy-marker (point) nil)))
         (target-window (get-buffer-window target)))
    (unless (string-empty-p art)
      (ring-insert agent-shell-artist-history-ring art))
    (with-current-buffer target
      (save-excursion
        (goto-char anchor)
        (insert (format "

%s

" art)))
      (goto-char anchor))
    (quit-window t)
    (when (window-live-p target-window)
      (set-window-point target-window anchor))
    (set-marker anchor nil)))

(defun agent-shell-artist-cancel ()
  "Discard the scratch art and close the buffer."
  (declare (modes agent-shell-artist-mode))
  (interactive)
  (quit-window t))

(provide 'agent-shell-artist)

;;; agent-shell-artist.el ends here
