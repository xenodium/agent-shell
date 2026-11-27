;;; agent-shell-diff.el --- A quick way to query/display a diff. -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'cl-lib))
(require 'diff)
(require 'diff-mode)
(require 'agent-shell-faces)

(defvar-local agent-shell-diff--on-exit nil
  "Function to call when the diff buffer is killed.

This variable is automatically set by :on-exit from `agent-shell-diff'
and can be temporarily let-bound to nil to prevent the
on-exit callback from running when the buffer is killed.")

(defvar-local agent-shell-diff--file nil
  "Buffer-local file path associated with the diff.")

(defvar-local agent-shell-diff--accept-all-command nil
  "Buffer-local command to accept all changes in the diff.")

(defvar-local agent-shell-diff--reject-all-command nil
  "Buffer-local command to reject all changes in the diff.")

(defvar agent-shell-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'diff-hunk-next)
    (define-key map (kbd "p") #'diff-hunk-prev)
    (define-key map (kbd "y") #'agent-shell-diff-accept-all)
    (define-key map (kbd "C-c C-c") #'agent-shell-diff-reject-all)
    (define-key map (kbd "RET") #'agent-shell-diff-open-file)
    (define-key map (kbd "q") #'kill-current-buffer)
    map)
  "Keymap for `agent-shell-diff-mode'.")

(define-derived-mode agent-shell-diff-mode diff-mode "Agent-Shell-Diff"
  "Major mode for `agent-shell' diff buffers.
Derives from `diff-mode'.  Provides `agent-shell-diff-accept-all'
and `agent-shell-diff-reject-all' commands that can be rebound
via `agent-shell-diff-mode-map'."
  :group 'agent-shell
  :interactive nil
  ;; Don't inherit diff-mode-map (some bindings can be destructive).
  (set-keymap-parent agent-shell-diff-mode-map nil)
  (setq buffer-read-only t))

(defun agent-shell-diff-kill-buffer (buffer)
  "Kill diff BUFFER, suppressing any `agent-shell-diff--on-exit' callback.
If BUFFER is not live, do nothing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq agent-shell-diff--on-exit nil))
    (kill-buffer buffer)))

(defun agent-shell-diff-accept-all ()
  "Accept all changes in the current diff buffer."
  (declare (modes agent-shell-diff-mode))
  (interactive)
  (if agent-shell-diff--accept-all-command
      (let ((buf (current-buffer)))
        (funcall agent-shell-diff--accept-all-command)
        (when (buffer-live-p buf)
          (let ((agent-shell-diff--on-exit nil))
            (kill-buffer buf))))
    (user-error "No accept command available in this buffer")))

(defun agent-shell-diff-reject-all ()
  "Reject all changes in the current diff buffer."
  (declare (modes agent-shell-diff-mode))
  (interactive)
  (if agent-shell-diff--reject-all-command
      (let ((buf (current-buffer)))
        (when (funcall agent-shell-diff--reject-all-command)
          (when (buffer-live-p buf)
            (let ((agent-shell-diff--on-exit nil))
              (kill-buffer buf)))))
    (user-error "No reject command available in this buffer")))

(cl-defun agent-shell-diff (&key diffs on-exit on-accept on-reject title)
  "Display one or more diffs in a buffer.

Creates a new buffer showing the differences using
`agent-shell-diff-mode'.  The buffer is read-only.

DIFFS is a list of alists, each with :old, :new and :file keys, as
returned by `agent-shell--make-diff-infos'.  A single diff is passed as
a one-element list.  When DIFFS holds more than one file, each is shown
as its own section preceded by a header naming the file.

When the buffer is killed, calls ON-EXIT with no arguments.

Returns the newly created diff buffer.

Arguments:
  :DIFFS     - List of ((:old . _) (:new . _) (:file . _)) alists
  :ON-EXIT   - Function called with no arguments when buffer is killed
  :ON-ACCEPT - Command to accept all changes
  :ON-REJECT - Command to reject all changes
  :TITLE     - Optional title to display in header line"
  (let* ((first-file (map-elt (car diffs) :file))
         (title (or title
                    (when (and first-file (not (cdr diffs)))
                      (file-name-nondirectory first-file))))
         (diff-buffer (generate-new-buffer "*agent-shell-diff*"))
         (calling-window (selected-window))
         (calling-buffer (current-buffer))
         (interrupt-key (where-is-internal 'agent-shell-interrupt
                                           (current-local-map) t)))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (let ((inhibit-read-only t)
                  (diff-mode-read-only nil))
              (erase-buffer)
              ;; Set mode before inserting diff so diff-no-select
              ;; doesn't reset font-lock (see #316).
              (agent-shell-diff-mode)
              (agent-shell-diff--insert-diffs diffs diff-buffer)
              ;; Add overlays to hide scary text.
              (save-excursion
                (goto-char (point-min))
                ;; Hide --- and +++ lines
                (while (re-search-forward "^\\(---\\|\\+\\+\\+\\).*\n" nil t)
                  (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
                    (overlay-put overlay 'display "")
                    (overlay-put overlay 'evaporate t)))
                ;; Replace @@ lines with a single-line "changes" label.
                ;; Intended display is (blank lines above and below each
                ;; label give hunks breathing room):
                ;;
                ;;   + prior hunk's last line...
                ;;
                ;;   [changes]
                ;;
                ;;    def greet(name):
                ;;   -    print("hi " + name)
                ;;   +    print("hello " + name)
                ;;
                ;; The overlay covers only the @@ text (not its trailing
                ;; newline) and its `display' string contains no newlines.
                ;; Overlay `display'/`before-string' strings that embed
                ;; newlines make `move_it_vertically_backward' pathological,
                ;; hanging redisplay while scrolling (see #719).  The blank
                ;; lines are real newlines (not overlay strings); the one
                ;; below the label is tagged with `agent-shell-diff-spacer'
                ;; so the hunk parsers skip it when locating the body.
                (goto-char (point-min))
                (while (re-search-forward "^@@.*@@.*$" nil t)
                  (let ((beg (match-beginning 0))
                        (end (match-end 0)))
                    ;; Blank line above the label, except the first one
                    ;; (already at the top of the buffer).
                    (unless (= beg (point-min))
                      (save-excursion
                        (goto-char beg)
                        (insert "\n"))
                      (setq beg (1+ beg)
                            end (1+ end)))
                    ;; Blank line below the label.  Tagged so the hunk
                    ;; parsers can skip it: the body no longer sits
                    ;; immediately below the @@ header.
                    (save-excursion
                      (goto-char (min (point-max) (1+ end)))
                      (insert (propertize "\n" 'agent-shell-diff-spacer t)))
                    (let ((overlay (make-overlay beg end)))
                      (overlay-put overlay 'display
                                   (propertize " changes " 'face 'agent-shell-diff-changes-label))
                      (overlay-put overlay 'evaporate t))))))
            (goto-char (point-min))
            (ignore-errors (diff-hunk-next))
            (setq agent-shell-diff--file first-file
                  agent-shell-diff--accept-all-command on-accept
                  agent-shell-diff--reject-all-command on-reject)
            (when on-exit
              (setq agent-shell-diff--on-exit on-exit)
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (when (and agent-shell-diff--on-exit
                                     (buffer-live-p calling-buffer))
                            (with-current-buffer calling-buffer
                              (funcall on-exit)))
                          ;; Give focus back to calling buffer.
                          (when (buffer-live-p calling-buffer)
                            (ignore-errors
                              (when (window-live-p calling-window)
                                (unless (eq (window-buffer calling-window) calling-buffer)
                                  (set-window-buffer calling-window calling-buffer))
                                (select-window calling-window)))))
                        nil t))
            (let ((map (copy-keymap agent-shell-diff-mode-map)))
              (when (and interrupt-key
                         (not (lookup-key map interrupt-key)))
                (define-key map interrupt-key #'agent-shell-diff-reject-all))
              (use-local-map map))
            (setq header-line-format
                  (substitute-command-keys
                   (concat
                    "  "
                    (when title
                      (concat (propertize title 'face 'mode-line-emphasis) " "))
                    "\\[diff-hunk-next] next hunk  "
                    "\\[diff-hunk-prev] previous hunk  "
                    "\\[agent-shell-diff-accept-all] accept  "
                    "\\[agent-shell-diff-reject-all] reject  "
                    "\\[agent-shell-diff-open-file] open file  "
                    "\\[kill-current-buffer] quit"))))
          diff-buffer)
      (pop-to-buffer diff-buffer '((display-buffer-use-some-window
                                    display-buffer-same-window))))))

(defun agent-shell-diff-open-file ()
  "Open the file for the diff at point and jump to the change.

The diff's oldText/newText are often just a fragment of the file (eg.
Codex sends the edited region), so the hunk's line numbers are relative
to that fragment, not the file.  Rather than trust them, this searches
the file for the changed content.  When ACP reports a `locations' line
for the change, it is used to disambiguate duplicate matches.

Uses the hunk at point, or the nearest one below, else above.  When
point sits on a body line, lands on that line's content.  Searches for
the old-side text first (the change has usually not been applied yet),
then the new-side text.

See https://github.com/xenodium/agent-shell/issues/347"
  (declare (modes agent-shell-diff-mode))
  (interactive)
  (let ((target (agent-shell-diff--target-at-point)))
    (unless (map-elt target :file)
      (user-error "No file associated with this diff buffer"))
    (find-file (map-elt target :file))
    (agent-shell-diff--jump-to-anchor
     :old-block (map-elt target :old-block)
     :new-block (map-elt target :new-block)
     :offset (map-elt target :offset)
     :hint-line (map-elt target :hint-line))))

(cl-defun agent-shell-diff--jump-to-anchor (&key old-block new-block offset hint-line)
  "Move point in the current buffer to a change described by anchors.

Searches for OLD-BLOCK (the old-side text of the change), falling back to
NEW-BLOCK (the new-side text) when the change is already applied.  Once a
block matches, moves OFFSET lines into it.

HINT-LINE, when non-nil, is the ACP-reported line of the change; it is
used to pick between duplicate matches.  Leaves point at the top when
neither block is found or both are empty.

A region left active in the buffer is dropped either way: point has
moved out from under it, so what it spans is no longer what anything
selected.  Dropped whatever `transient-mark-mode' is, since what
activated it did not consult the mode either."
  (if-let* ((position (or (agent-shell-diff--search-block old-block hint-line)
                          (agent-shell-diff--search-block new-block hint-line))))
      (progn
        (goto-char position)
        (forward-line offset))
    (goto-char (point-min)))
  ;; Forced: a range reference activates the mark whatever
  ;; `transient-mark-mode' says, so clearing it cannot depend on the mode
  ;; either, or the activation outlives what it was selecting.
  (deactivate-mark t)
  (recenter))

(defun agent-shell-diff--search-block (block hint-line)
  "Return the start position of the best match of BLOCK, or nil.

Collects every occurrence of BLOCK in the current buffer.  With HINT-LINE
non-nil, returns the occurrence whose line is closest to it; otherwise the
first.  Returns nil when BLOCK is nil, empty, or absent."
  (when (and block (not (string-empty-p block)))
    (save-excursion
      (goto-char (point-min))
      (let (positions)
        (while (search-forward block nil t)
          (push (match-beginning 0) positions)
          (goto-char (1+ (match-beginning 0))))
        (setq positions (nreverse positions))
        (if (and hint-line (cdr positions))
            (car (seq-sort-by (lambda (position)
                                (abs (- (line-number-at-pos position) hint-line)))
                              #'< positions))
          (car positions))))))

(defun agent-shell-diff--target-at-point ()
  "Return the file and change anchors for the diff at or nearest point.

Returns an alist:

  ((:file . file-path)
   (:old-block . old-side-text)
   (:new-block . new-side-text)
   (:offset . line-offset)
   (:hint-line . acp-reported-line))

The blocks are the old-side and new-side text of the relevant hunk, to
be searched for in the file.  :OFFSET is how many lines into a matched
block to land on.  :HINT-LINE is the ACP `locations' line, when present,
used to disambiguate duplicate matches.  Uses the hunk at point, or the
nearest below, else above.  When point is on a body line, :OFFSET targets
that line; otherwise it targets the hunk's first change."
  (save-excursion
    (if-let* ((header (agent-shell-diff--hunk-header-at-point)))
        (append (list (cons :file (get-text-property header 'agent-shell-diff-file))
                      (cons :hint-line (get-text-property header 'agent-shell-diff-line)))
                (agent-shell-diff--hunk-anchor
                 header
                 ;; Point on the hunk header itself is not on a particular
                 ;; change, so target the hunk's first change instead.
                 (unless (= header (line-beginning-position))
                   (line-beginning-position))))
      (if-let* ((near (or (save-excursion
                            (when (re-search-forward "^@@" nil t)
                              (line-beginning-position)))
                          (save-excursion
                            (when (re-search-backward "^@@" nil t)
                              (line-beginning-position))))))
          (append (list (cons :file (get-text-property near 'agent-shell-diff-file))
                        (cons :hint-line (get-text-property near 'agent-shell-diff-line)))
                  (agent-shell-diff--hunk-anchor near nil))
        (list (cons :file agent-shell-diff--file)
              (cons :old-block nil)
              (cons :new-block nil)
              (cons :offset 0)
              (cons :hint-line nil))))))

(defun agent-shell-diff--hunk-header-at-point ()
  "Return the position of the hunk header enclosing point, or nil.

Walks up from point's line through diff body lines until it reaches a
hunk header, stopping (with nil) at any non-diff line."
  (save-excursion
    (beginning-of-line)
    (catch 'result
      (while t
        (cond
         ((looking-at "^@@")
          (throw 'result (point)))
         ;; Skip the blank spacer inserted below the "changes" label.
         ((get-text-property (point) 'agent-shell-diff-spacer)
          (unless (zerop (forward-line -1))
            (throw 'result nil)))
         ((memq (char-after) '(?\s ?- ?+ ?\\))
          (unless (zerop (forward-line -1))
            (throw 'result nil)))
         (t
          (throw 'result nil)))))))

(defun agent-shell-diff--hunk-anchor (header-pos target-pos)
  "Return change anchors for the hunk at HEADER-POS.

TARGET-POS, when non-nil, is the position of a body line to land on;
otherwise the hunk's first change is used.  Returns an alist:

  ((:old-block . old-side-text)
   (:new-block . new-side-text)
   (:offset . line-offset))

The blocks are the hunk's old-side and new-side text (context plus
removed, and context plus added, respectively).  :OFFSET is the old-side
line index to move to within a matched block."
  (save-excursion
    (goto-char header-pos)
    (forward-line 1)
    ;; Skip the blank spacer inserted below the "changes" label.
    (when (get-text-property (point) 'agent-shell-diff-spacer)
      (forward-line 1))
    (let ((old-lines nil) (new-lines nil) (seen 0) (offset nil) (first-change nil))
      (while (and (not (eobp))
                  (memq (char-after) '(?\s ?- ?+ ?\\)))
        (let ((char (char-after))
              (text (buffer-substring-no-properties
                     (1+ (line-beginning-position)) (line-end-position)))
              (at-target (and target-pos (= (line-beginning-position) target-pos))))
          (cond
           ;; "\ No newline at end of file" marker; not file content.
           ((eq char ?\\))
           ((eq char ?\s)
            (when at-target (setq offset seen))
            (push text old-lines)
            (push text new-lines)
            (setq seen (1+ seen)))
           ((eq char ?-)
            (when at-target (setq offset seen))
            (unless first-change (setq first-change seen))
            (push text old-lines)
            (setq seen (1+ seen)))
           ((eq char ?+)
            (when at-target (setq offset seen))
            (unless first-change (setq first-change seen))
            (push text new-lines))))
        (forward-line 1))
      (list (cons :old-block (when old-lines
                               (string-join (nreverse old-lines) "\n")))
            (cons :new-block (when new-lines
                               (string-join (nreverse new-lines) "\n")))
            (cons :offset (or offset first-change 0))))))

(defun agent-shell-diff--insert-diffs (diffs buf)
  "Insert DIFFS into buffer BUF, one file section each.

DIFFS is a list of alists with :old, :new, :file and optional :line
keys.  When DIFFS holds more than one entry, each section is preceded by
a header naming the file.  Each section is tagged with
`agent-shell-diff-file' and, when present, `agent-shell-diff-line' text
properties so `agent-shell-diff-open-file' can locate the change."
  (let ((multiple (cdr diffs)))
    (with-current-buffer buf
      (dolist (diff diffs)
        (unless (bobp)
          (insert "\n"))
        (let ((section-start (point))
              (file (map-elt diff :file)))
          (when multiple
            (insert (propertize (concat (or file "changes") "\n")
                                'face 'diff-file-header)))
          (insert (agent-shell-diff--diff-section-string
                   (or (map-elt diff :old) "")
                   (or (map-elt diff :new) "")
                   file))
          (when file
            (put-text-property section-start (point)
                               'agent-shell-diff-file file))
          (when-let* ((line (map-elt diff :line)))
            (put-text-property section-start (point)
                               'agent-shell-diff-line line)))))))

(defun agent-shell-diff--diff-section-string (old new file)
  "Return a cleaned diff between OLD and NEW for FILE.

FILE is only used to derive a temp-file suffix so `diff' picks a
sensible mode; it may be nil.  The leading command line and trailing
\"Diff finished.\" line that `diff-no-select' adds are removed."
  (let* ((extension (and file (file-name-extension file)))
         (suffix (and extension (format ".%s" extension)))
         (old-file (make-temp-file "old" nil suffix))
         (new-file (make-temp-file "new" nil suffix)))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert old))
          (with-temp-file new-file (insert new))
          (with-temp-buffer
            (diff-no-select old-file new-file "-U3" t (current-buffer))
            (let ((inhibit-read-only t))
              ;; Remove command added by diff-no-select
              (goto-char (point-min))
              (delete-region (point) (progn (forward-line 1) (point)))
              ;; Remove "Diff finished." added by diff-no-select
              (delete-region (progn (goto-char (point-max))
                                    (forward-line -1)
                                    (forward-line 0)
                                    (point))
                             (point-max)))
            (buffer-string)))
      (delete-file old-file)
      (delete-file new-file))))

(provide 'agent-shell-diff)

;;; agent-shell-diff.el ends here
