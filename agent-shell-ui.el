;;; agent-shell-ui.el --- Interactive shell UI elements -*- lexical-binding: t; -*-

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
;; A library for creating interactive shell UI elements.
;;
;; Note: This package is in very early stages and likely has
;; rough edges.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'cursor-sensor)
(require 'subr-x)
(require 'text-property-search)
(require 'agent-shell-invariants)

(defvar-local agent-shell-ui--content-store nil
  "A hash table used to save sui content like body.
This avoids duplicating body content in text properties which is more costly.")

(cl-defun agent-shell-ui-make-fragment-model (&key (namespace-id "global") (block-id "1") label-left label-right body)
  "Create a fragment model alist.
NAMESPACE-ID, BLOCK-ID, LABEL-LEFT, LABEL-RIGHT, and BODY are the keys."
  (list (cons :namespace-id namespace-id)
        (cons :block-id block-id)
        (cons :label-left (agent-shell-ui--string-or-nil label-left))
        (cons :label-right (agent-shell-ui--string-or-nil label-right))
        (cons :body (agent-shell-ui--string-or-nil body))))

(defun agent-shell-ui--insert-read-only (text)
  "Insert TEXT as read-only output."
  (add-text-properties 0 (length text)
                       '(read-only t front-sticky (read-only))
                       text)
  (insert text))

(cl-defun agent-shell-ui-update-fragment (model &key append create-new on-post-process navigation expanded no-undo insert-before)
  "Update or add a fragment using MODEL.

When APPEND is non-nil, append to body instead of replacing.
When CREATE-NEW is non-nil, create new block.
When ON-POST-PROCESS is non-nil, call this function after updating.
When NAVIGATION is `never', block won't be TAB navigatable.
When NAVIGATION is `auto', block is navigatable if non-empty body.
When NAVIGATION is `always', block is always TAB navigatable.
When EXPANDED is non-nil, body will be expanded by default.
When NO-UNDO is non-nil, disable undo recording for this operation.
When INSERT-BEFORE is a buffer position, new blocks are inserted
before that position instead of at the end of the buffer.  This
keeps content above the shell prompt when user input is pending.

For existing blocks, the current expansion state is preserved unless overridden."
  (save-mark-and-excursion
    (let* ((inhibit-read-only t)
           (buffer-undo-list (if no-undo t buffer-undo-list))
           (namespace-id (map-elt model :namespace-id))
           (qualified-id (format "%s-%s" namespace-id (map-elt model :block-id)))
           (new-label-left (map-elt model :label-left))
           (new-label-right (map-elt model :label-right))
           (new-body (map-elt model :body))
           (block-start nil)
           (padding-start nil)
           (padding-end nil)
           (match (save-mark-and-excursion
                    (goto-char (point-max))
                    (text-property-search-backward
                     'agent-shell-ui-state nil
                     (lambda (_ state)
                       (equal (map-elt state :qualified-id) qualified-id))
                     t))))
      (when (or new-label-left new-label-right new-body)
        (when match
          (goto-char (prop-match-beginning match)))
        (if (and match (not create-new))
            (let* ((existing-model (agent-shell-ui--read-fragment-at-point))
                   (state (get-text-property (point) 'agent-shell-ui-state))
                   (existing-body (map-elt existing-model :body))
                   (block-end (prop-match-end match)))
              (setq block-start (prop-match-beginning match))
              (save-excursion
                (goto-char block-start)
                (skip-chars-backward "\n")
                (setq padding-start (point)))
              (if (and append new-body
                       existing-body (not (string-empty-p existing-body))
                       (not new-label-left)
                       (not new-label-right))
                  ;; Append in-place: insert only new body text,
                  ;; avoiding the delete-and-reinsert that displaces point.
                  (let* ((body-range (agent-shell-ui--nearest-range-matching-property
                                      :property 'agent-shell-ui-section :value 'body
                                      :from block-start :to block-end))
                         (old-body-start (map-elt body-range :start))
                         (old-body-end (map-elt body-range :end))
                         (body-text new-body))
                    ;; Normalize trailing whitespace only.  Do NOT
                    ;; strip leading newlines here — unlike the initial
                    ;; insert (where \n\n is already placed between
                    ;; label and body), appended chunks carry meaningful
                    ;; leading newlines (list-item separators, paragraph
                    ;; breaks, etc.).
                    (when (string-suffix-p "\n\n" body-text)
                      (setq body-text (concat (string-trim-right body-text) "\n\n")))
                    ;; Cap consecutive newlines at the append boundary
                    ;; to at most two.  An empty agent_message_chunk is
                    ;; substituted with "\n\n" upstream to break
                    ;; paragraphs; if the existing body already ends in
                    ;; one or more "\n", a naive concat produces three
                    ;; or more newlines (an extra blank line).
                    (let* ((trailing-count
                            (save-excursion
                              (goto-char old-body-end)
                              (let ((n 0))
                                (while (and (< (point-min) (point))
                                            (eq (char-before) ?\n))
                                  (cl-incf n)
                                  (forward-char -1))
                                n)))
                           (leading-count
                            (let ((i 0))
                              (while (and (< i (length body-text))
                                          (eq (aref body-text i) ?\n))
                                (cl-incf i))
                              i))
                           (boundary-target (min 2 (max trailing-count leading-count)))
                           (keep-leading (max 0 (- boundary-target trailing-count))))
                      (when (< keep-leading leading-count)
                        (setq body-text (concat (make-string keep-leading ?\n)
                                                (substring body-text leading-count)))))
                    (if (map-elt state :collapsed)
                        ;; Collapsed: insert-and-inherit picks up invisible
                        ;; from existing body via stickiness.
                        (progn
                          (goto-char old-body-end)
                          (insert-and-inherit (agent-shell-ui--indent-text
                                              (string-remove-prefix "  " body-text) "  ")))
                      ;; Expanded: un-hide old trailing whitespace (no longer
                      ;; trailing), insert, re-hide new trailing whitespace.
                      (remove-text-properties old-body-start old-body-end
                                              '(invisible nil))
                      (goto-char old-body-end)
                      (insert-and-inherit (agent-shell-ui--indent-text
                               (string-remove-prefix "  " body-text) "  "))
                      (let ((new-body-end (point)))
                        (save-mark-and-excursion
                          (goto-char new-body-end)
                          (when (re-search-backward "[^ \t\n]" old-body-start t)
                            (forward-char 1)
                            (when (< (point) new-body-end)
                              (add-text-properties (point) new-body-end
                                                   '(invisible t)))))))
                    (let ((new-body-end (point)))
                      ;; Extend block-level properties to cover new text
                      (put-text-property block-start new-body-end
                                         'agent-shell-ui-state
                                         (get-text-property block-start 'agent-shell-ui-state))
                      (put-text-property block-start new-body-end 'read-only t)
                      (put-text-property block-start new-body-end 'front-sticky '(read-only))
                      ;; Update content-store
                      (unless agent-shell-ui--content-store
                        (setq agent-shell-ui--content-store (make-hash-table :test 'equal)))
                      (puthash (concat qualified-id "-body")
                               (concat existing-body new-body)
                               agent-shell-ui--content-store)
                      (setq padding-end new-body-end)))
                ;; Full rebuild: delete and regenerate (label change, first
                ;; body content, or non-append replacement).
                (let* ((final-body (if new-body
                                       (if (and append existing-body)
                                           (concat existing-body new-body)
                                         new-body)
                                     existing-body))
                       (final-model (list (cons :namespace-id namespace-id)
                                          (cons :block-id (map-elt model :block-id))
                                          (cons :label-left (or new-label-left
                                                                (map-elt existing-model :label-left)))
                                          (cons :label-right (or new-label-right
                                                                 (map-elt existing-model :label-right)))
                                          (cons :body final-body))))
                  (delete-region block-start block-end)
                  (goto-char block-start)
                  (agent-shell-ui--insert-fragment final-model qualified-id
                                                   (not (map-elt state :collapsed))
                                                   navigation)
                  (setq padding-end (point)))))

          ;; Not found or create-new - insert new block
          (goto-char (if insert-before
                         (min insert-before (point-max))
                       (point-max)))
          (setq padding-start (point))
          (agent-shell-ui--insert-read-only (agent-shell-ui--required-newlines 2))
          (setq block-start (point))
          (agent-shell-ui--insert-fragment model qualified-id expanded navigation)
          (agent-shell-ui--insert-read-only "\n\n")
          (setq padding-end (point))))
      (when on-post-process
        (funcall on-post-process))
      (when-let ((block-range (agent-shell-ui--block-range :position block-start)))
        (list (cons :block block-range)
              (cons :body (agent-shell-ui--nearest-range-matching-property
                           :property 'agent-shell-ui-section :value 'body
                           :from (map-elt block-range :start)
                           :to (map-elt block-range :end)))
              (cons :label-left (agent-shell-ui--nearest-range-matching-property
                                 :property 'agent-shell-ui-section :value 'label-left
                                 :from (map-elt block-range :start)
                                 :to (map-elt block-range :end)))
              (cons :label-right (agent-shell-ui--nearest-range-matching-property
                                  :property 'agent-shell-ui-section :value 'label-right
                                  :from (map-elt block-range :start)
                                  :to (map-elt block-range :end)))
              (cons :padding (when (and padding-start padding-end)
                               (list (cons :start padding-start)
                                     (cons :end padding-end)))))))))


(defun agent-shell-ui--split-qualified-id (qualified-id)
  "Split QUALIFIED-ID into (NAMESPACE-ID . BLOCK-ID) on the first hyphen.
Namespace-ids are hyphen-free in production (request-count integer),
but block-ids commonly carry hyphens (e.g.  \"toolCallId-plan\",
\"permission-toolCallId\", \"failed-X-id:Y-code:Z\").  Returns nil
when QUALIFIED-ID has no hyphen."
  (when (string-match "^\\([^-]+\\)-\\(.+\\)$" qualified-id)
    (cons (match-string 1 qualified-id)
          (match-string 2 qualified-id))))

(defun agent-shell-ui--read-fragment-at (position qualified-id)
  "Read fragment at POSITION with QUALIFIED-ID."
  (when-let ((fragment (list (cons :block-id qualified-id)))
             (state (get-text-property position 'agent-shell-ui-state))
             (range (agent-shell-ui--block-range :position position)))
    ;; TODO: Get rid of merging block namespace and id.
    (when-let ((split (agent-shell-ui--split-qualified-id qualified-id)))
      (setf (map-elt fragment :namespace-id) (car split))
      (setf (map-elt fragment :block-id) (cdr split)))
    (save-mark-and-excursion
      (save-restriction
        (narrow-to-region (map-elt range :start)
                          (map-elt range :end))
        (goto-char (map-elt range :start))
        (setf (map-elt fragment :collapsed) (map-elt state :collapsed))
        (when-let ((label-left (agent-shell-ui--nearest-range-matching-property
                                :property 'agent-shell-ui-section :value 'label-left)))
          (setf (map-elt fragment :label-left) (buffer-substring (map-elt label-left :start)
                                                                 (map-elt label-left :end))))
        (when-let ((label-right (agent-shell-ui--nearest-range-matching-property
                                 :property 'agent-shell-ui-section :value 'label-right)))
          (setf (map-elt fragment :label-right) (buffer-substring (map-elt label-right :start)
                                                                  (map-elt label-right :end))))
        (when agent-shell-ui--content-store
          (when-let ((body (gethash (concat qualified-id "-body") agent-shell-ui--content-store)))
            (setf (map-elt fragment :body) body)))))
    fragment))

(cl-defun agent-shell-ui-delete-fragment (&key namespace-id block-id no-undo)
  "Delete fragment with NAMESPACE-ID and BLOCK-ID.

When NO-UNDO is non-nil, disable undo recording for this operation."
  (save-mark-and-excursion
    (let* ((inhibit-read-only t)
           (buffer-undo-list (if no-undo t buffer-undo-list))
           (qualified-id (format "%s-%s" namespace-id block-id))
           (match (save-mark-and-excursion
                    (goto-char (point-max))
                    (text-property-search-backward
                     'agent-shell-ui-state nil
                     (lambda (_ state)
                       (equal (map-elt state :qualified-id) qualified-id))
                     t))))
      (when match
        (let ((block-start (prop-match-beginning match))
              (block-end (prop-match-end match)))
          (when agent-shell-ui--content-store
            (remhash qualified-id agent-shell-ui--content-store))
          ;; Remove vertical space that's part of the block.
          (goto-char block-end)
          (skip-chars-forward " \t\n")
          (setq block-end (point))
          (delete-region block-start block-end))))))

(defun agent-shell-ui--read-fragment-at-point ()
  "Read fragment at point, returning model or nil if none found."
  (when-let ((state (get-text-property (point) 'agent-shell-ui-state))
             (range (agent-shell-ui--block-range :position (point))))
    (agent-shell-ui--read-fragment-at (map-elt range :start)
                                      (map-elt state :qualified-id))))

(cl-defun agent-shell-ui--block-range (&key position)
  "Get block range at POSITION if found.  Nil otherwise.

In the form:

  ((start . 1)
   (end . 3))."
  (when-let ((qualified-id (map-elt (get-text-property (or position (point)) 'agent-shell-ui-state) :qualified-id)))
    (agent-shell-ui--nearest-range-matching-property
     :property 'agent-shell-ui-state
     :value qualified-id
     :predicate (lambda (qualified-id property)
                  (equal (map-elt property :qualified-id) qualified-id)))))

(cl-defun agent-shell-ui--nearest-range-matching-property (&key property value (predicate t) from to)
  "Return nearest range where PREDICATE is non-nil for PROPERTY and VALUE."
  (save-mark-and-excursion
    (save-restriction
      (when (and from to)
        (narrow-to-region from to))
      (let ((backward-match (or (text-property-search-backward property value predicate)
                                (progn
                                  (unless (eobp)
                                    (forward-char 1))
                                  (text-property-search-backward property value predicate))))
            (forward-match (text-property-search-forward property value predicate)))
        (when (or backward-match forward-match)
          `((:start . ,(if backward-match
                           (prop-match-beginning backward-match)
                         (prop-match-beginning forward-match)))
            (:end . ,(if forward-match
                         (prop-match-end forward-match)
                       (prop-match-end backward-match)))))))))

(defun agent-shell-ui--insert-fragment (model qualified-id &optional expanded navigation)
  "Insert fragment from MODEL with QUALIFIED-ID text properties.
EXPANDED determines initial state (default nil for collapsed).
NAVIGATION controls navigability:

 `never' (not navigatable)
 `auto' (navigatable if body and indicator present)
 `always' (always navigatable)."
  (let ((block-start (point))
        (label-left (map-elt model :label-left))
        (label-right (map-elt model :label-right))
        (body (map-elt model :body))
        (need-space nil)
        (indicator-start)
        (indicator-end)
        (label-left-start)
        (label-left-end)
        (label-right-start)
        (label-right-end)
        (body-start)
        (body-end)
        (collapsable))

    ;; Insert collapse indicator if body exists
    (when-let ((has-labels (or label-left label-right)))
      (if body
          (progn
            (setq collapsable has-labels)
            (setq indicator-start (point))
            (insert (agent-shell-ui-add-action-to-text
                     (if expanded "▼ " "▶ ")
                     (lambda ()
                       (interactive)
                       (agent-shell-ui-toggle-fragment-at-point))
                     (lambda ()
                       (message "Press RET to toggle"))))
            (setq indicator-end (point))
            (add-text-properties indicator-start indicator-end
                                 `(agent-shell-ui-section indicator
                                                          keymap ,(agent-shell-ui-make-action-keymap
                                                                   (lambda ()
                                                                     (interactive)
                                                                     (agent-shell-ui-toggle-fragment-at-point)))
                                                          read-only t
                                                          front-sticky (read-only))))
        (setq collapsable nil)
        (setq indicator-start (point))
        ;; Reserving the space for expand indicators enables
        ;; aligning columns but also avoids text jumping when
        ;; body arrives later on.
        ;;
        ;; For example:
        ;;
        ;; "   [ completed ] [ read ] Read agent-shell/README.org"
        ;;
        ;; vs
        ;;
        ;; "▼  [ completed ] [ read ] Read agent-shell/README.org"
        (insert "  ") ;; "▶ "
        (setq indicator-end (point))))

    (when label-left
      (setq label-left-start (point))
      (insert (agent-shell-ui-add-action-to-text
               label-left
               (lambda ()
                 (interactive)
                 (agent-shell-ui-toggle-fragment-at-point))
               (lambda ()
                 (message "Press RET to toggle"))))
      (setq label-left-end (point))
      (add-text-properties label-left-start label-left-end
                           `(agent-shell-ui-section label-left
                                                    help-echo ,qualified-id
                                                    read-only t
                                                    front-sticky (read-only)))
      (setq need-space t))

    (when label-right
      (when need-space
        (insert " "))
      (setq label-right-start (point))
      (insert (agent-shell-ui-add-action-to-text
               label-right
               (lambda ()
                 (interactive)
                 (agent-shell-ui-toggle-fragment-at-point))
               (lambda ()
                 (message "Press RET to toggle"))))
      (setq label-right-end (point))
      (add-text-properties label-right-start label-right-end
                           `(agent-shell-ui-section label-right
                                                    help-echo ,qualified-id
                                                    read-only t
                                                    front-sticky (read-only))))

    (when body
      (when (or label-left label-right)
        (insert "\n\n"))
      ;; Drop any leading body newlines as newlines are
      ;; already inserted between labels and body.
      (when (string-prefix-p "\n" body)
        (setq body (string-trim-left body "\n")))
      ;; Never leave more than two trailing newlines.
      (when (string-suffix-p "\n\n" body)
        (setq body (concat (string-trim-right body) "\n\n")))
      (setq body-start (point))
      (let ((clean-body (string-remove-prefix "  " body)))
        (insert (agent-shell-ui--indent-text clean-body "  ")))
      (setq body-end (point))
      (add-text-properties body-start body-end
                           `(agent-shell-ui-section body
                                                    help-echo ,qualified-id
                                                    read-only t
                                                    front-sticky (read-only))))
    ;; Include the newlines before the body in the invisible region
    (when collapsable
      (add-text-properties (or label-right-end label-left-end)
                           body-end
                           `(invisible ,(if expanded nil t))))
    ;; Hide trailing whitespace (don't delete) in body using text properties.
    (when body
      (save-mark-and-excursion
        (goto-char body-end)
        (when (re-search-backward "[^ \t\n]" body-start t)
          (forward-char 1)
          (when (< (point) body-end)
            (add-text-properties (point) body-end
                                 '(invisible t))))))
    (when body
      (unless agent-shell-ui--content-store
        (setq agent-shell-ui--content-store (make-hash-table :test 'equal)))
      (puthash (concat qualified-id "-body") body agent-shell-ui--content-store))
    (put-text-property
     block-start (or body-end label-right-end label-left-end)
     'agent-shell-ui-state (list
                            ;; Note: Avoid storing chunky data in
                            ;; agent-shell-ui-state as it will impact performance.
                            ;; Use agent-shell-ui--content-store for these instances.
                            ;; For example, fragment body.
                            (cons :qualified-id qualified-id)
                            (cons :collapsed (and (or label-left label-right)
                                                    (not expanded)))
                            (cons :navigatable (cond
                                                ((eq navigation 'never) nil)
                                                ((eq navigation 'always) t)
                                                ((eq navigation 'auto)
                                                 (and body indicator-start))
                                                (t
                                                 ;; Default to auto
                                                 (and body indicator-start))))))
    (put-text-property block-start (or body-end label-right-end label-left-end) 'read-only t)
    (put-text-property block-start (or body-end label-right-end label-left-end) 'front-sticky '(read-only))))

(cl-defun agent-shell-ui-update-text (&key namespace-id block-id text append create-new no-undo insert-before)
  "Update or insert a plain text entry identified by NAMESPACE-ID and BLOCK-ID.

TEXT is the string to insert or append.
When APPEND is non-nil, append TEXT to existing entry.
When CREATE-NEW is non-nil, always create a new entry.
When NO-UNDO is non-nil, disable undo recording.
When INSERT-BEFORE is a buffer position, new entries are inserted
before that position instead of at the end of the buffer."
  (save-mark-and-excursion
    (let* ((inhibit-read-only t)
           (buffer-undo-list (if no-undo t buffer-undo-list))
           (qualified-id (format "%s-%s" namespace-id block-id))
           (props `(agent-shell-ui-state ((:qualified-id . ,qualified-id))
                                         read-only t
                                         front-sticky (read-only)))
           (match (save-mark-and-excursion
                    (goto-char (point-max))
                    (text-property-search-backward
                     'agent-shell-ui-state nil
                     (lambda (_ state)
                       (equal (map-elt state :qualified-id) qualified-id))
                     t))))
      (when text
        (cond
         ;; Append to existing entry.
         ((and match (not create-new) append)
          (goto-char (prop-match-end match))
          (insert (apply #'propertize text props))
          (list (cons :block (list (cons :start (prop-match-beginning match))
                                   (cons :end (point))))
                (cons :padding (list (cons :start (prop-match-beginning match))
                                     (cons :end (point))))))
         ;; Replace existing entry.
         ((and match (not create-new))
          (let ((padding-start (save-excursion
                                 (goto-char (prop-match-beginning match))
                                 (skip-chars-backward "\n")
                                 (point))))
            (delete-region (prop-match-beginning match) (prop-match-end match))
            (goto-char (prop-match-beginning match))
            (insert (apply #'propertize text props))
            (list (cons :block (list (cons :start (prop-match-beginning match))
                                     (cons :end (point))))
                  (cons :padding (list (cons :start padding-start)
                                       (cons :end (point)))))))
         ;; New entry.
         (t
          (goto-char (if insert-before
                         (min insert-before (point-max))
                       (point-max)))
          (let ((padding-start (point)))
            (agent-shell-ui--insert-read-only (agent-shell-ui--required-newlines 2))
            (let ((block-start (point)))
              (insert (apply #'propertize text props))
              (list (cons :block (list (cons :start block-start)
                                       (cons :end (point))))
                    (cons :padding (list (cons :start padding-start)
                                         (cons :end (point)))))))))))))

(defun agent-shell-ui--required-newlines (desired)
  "Return string of newlines needed to reach DESIRED before POSITION."
  (let ((context (save-mark-and-excursion
                   (let ((end (point)))
                     (forward-line (- (+ 1 desired)))
                     (buffer-substring (point) end)))))
    (with-temp-buffer
      (insert context)
      ;; When counting visible newlines before point,
      ;; we may encounter invisible text, which may
      ;; look like newlines but gives false negatives.
      ;; In those cases, delete any 'invisible text
      ;; and try counting.
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (while (not (bobp))
          (let* ((end (point))
                 (start (previous-single-property-change end 'invisible nil (point-min))))
            (if (get-text-property (1- end) 'invisible)
                (delete-region start end))
            (goto-char start))))
      (goto-char (point-max))
      (let ((pos (point)))
        (skip-chars-backward "\n")
        (make-string (max 0 (- desired (- pos (point)))) ?\n)))))

(defun agent-shell-ui-toggle-fragment-at-point ()
  "Toggle visibility of fragment body at point."
  (interactive)
  (save-mark-and-excursion
    (when-let* ((inhibit-read-only t)
                (buffer-undo-list t)
                (state (get-text-property (point) 'agent-shell-ui-state))
                (block (agent-shell-ui--block-range :position (point)))
                (body (agent-shell-ui--nearest-range-matching-property
                       :property 'agent-shell-ui-section :value 'body
                       :from (map-elt block :start)
                       :to (map-elt block :end)))
                (indicator (agent-shell-ui--nearest-range-matching-property
                            :property 'agent-shell-ui-section :value 'indicator
                            :from (map-elt block :start)
                            :to (map-elt block :end)))
                ;; Find where labels end (either label-right or label-left)
                (invisible-start (or (map-elt (agent-shell-ui--nearest-range-matching-property
                                               :property 'agent-shell-ui-section :value 'label-right
                                               :from (map-elt block :start)
                                               :to (map-elt block :end))
                                              :end)
                                     (map-elt (agent-shell-ui--nearest-range-matching-property
                                               :property 'agent-shell-ui-section :value 'label-left
                                               :from (map-elt block :start)
                                               :to (map-elt block :end))
                                              :end)))
                ;; Must be saved before deleting region.
                (indicator-properties (text-properties-at (map-elt indicator :start))))
      (let ((new-collapsed-state (not (map-elt state :collapsed))))
        ;; Toggle invisible text property including newlines before body
        (put-text-property invisible-start
                           (map-elt body :end)
                           'invisible new-collapsed-state)
        ;; Update indicator
        (delete-region (map-elt indicator :start)
                       (map-elt indicator :end))
        (goto-char (map-elt indicator :start))
        (insert (if new-collapsed-state "▶ " "▼ "))
        ;; Update state
        (add-text-properties (map-elt indicator :start)
                             (point) indicator-properties)
        (map-put! state :collapsed new-collapsed-state)
        (put-text-property (map-elt block :start)
                           (map-elt block :end) 'agent-shell-ui-state state)
        (when-let* ((qid (map-elt state :qualified-id))
                    (split (agent-shell-ui--split-qualified-id qid)))
          (agent-shell-invariants-on-collapse-toggle
           (car split) (cdr split) new-collapsed-state))))))

(defun agent-shell-ui-collapse-fragment-by-id (namespace-id block-id)
  "Collapse fragment with NAMESPACE-ID and BLOCK-ID."
  (save-mark-and-excursion
    (let ((qualified-id (format "%s-%s" namespace-id block-id)))
      (goto-char (point-max))
      (when (text-property-search-backward
             'agent-shell-ui-state qualified-id
             (lambda (_ state)
               (equal (map-elt state :qualified-id) qualified-id))
             t)
        (agent-shell-ui-toggle-fragment-at-point)))))

(defun agent-shell-ui--string-or-nil (str)
  "Return STR if it is not nil and not empty, otherwise nil."
  (and str (not (string-empty-p str)) str))

(defun agent-shell-ui--indent-text (text &optional indent-string)
  "Indent TEXT visually without affecting copied text.
INDENT-STRING defaults to two spaces.
Uses `line-prefix' display property so indentation is visual only."
  (when text
    (let* ((indent (or indent-string "  "))
           (lines (split-string text "\n")))
      (concat
       (propertize (car lines) 'line-prefix indent 'wrap-prefix indent)
       (mapconcat (lambda (line)
                    (propertize (concat "\n" line) 'line-prefix indent 'wrap-prefix indent))
                  (cdr lines)
                  "")))))

(defun agent-shell-ui-forward-block ()
  "Jump to the next block."
  (interactive)
  (when-let* ((start-point (point))
              (found (save-mark-and-excursion
                       ;; In navigatable block already
                       ;; move past it.
                       (when-let ((state (get-text-property (point) 'agent-shell-ui-state))
                                  (block (agent-shell-ui--block-range :position (point))))
                         (goto-char (map-elt block :end)))
                       (when-let ((next (text-property-search-forward
                                         'agent-shell-ui-state nil
                                         (lambda (_old-val new-val)
                                           (and new-val (map-elt new-val :navigatable)))
                                         t)))
                         (prop-match-beginning next)))))
    (when found
      (deactivate-mark)
      (goto-char found)
      found)))

(defun agent-shell-ui-backward-block ()
  "Jump to the previous block."
  (interactive)
  (when-let* ((start-point (point))
              (found (save-mark-and-excursion
                       ;; In navigatable block already
                       ;; move to beginning.
                       (when-let ((state (get-text-property (point) 'agent-shell-ui-state))
                                  (block (agent-shell-ui--block-range :position (point))))
                         (goto-char (map-elt block :start)))
                       (when-let ((prev (text-property-search-backward
                                         'agent-shell-ui-state nil
                                         (lambda (_old-val new-val)
                                           (and new-val (map-elt new-val :navigatable)))
                                         t)))
                         (prop-match-beginning prev)))))
    (when found
      (deactivate-mark)
      (goto-char found)
      found)))

(defun agent-shell-ui-make-action-keymap (action)
  "Create keymap with ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] action)
    (define-key map (kbd "RET") action)
    (define-key map [remap self-insert-command] 'ignore)
    map))

(defun agent-shell-ui-add-action-to-text (text action &optional on-entered face)
  "Add ACTION lambda to propertized TEXT and return modified text.
ON-ENTERED is a function to call when the cursor enters the text.
FACE when non-nil applies the specified face to the text."
  (add-text-properties 0 (length text)
                       `(keymap ,(agent-shell-ui-make-action-keymap action))
                       text)
  (when on-entered
    (add-text-properties 0 (length text)
                         (list 'cursor-sensor-functions
                               (list (lambda (_window _old-pos sensor-action)
                                       (when (eq sensor-action 'entered)
                                         (funcall on-entered)))))
                         text))
  (when face
    (add-text-properties 0 (length text)
                         `(font-lock-face ,face
                           face ,face)
                         text))
  (add-text-properties 0 (length text)
                       '(pointer hand
                         rear-nonsticky t)
                       text)
  text)

(defvar-local agent-shell-ui--isearch-opened-fragments nil
  "List of fragment qualified-ids that were opened during isearch.")

(defun agent-shell-ui--isearch-filter-predicate (beg end)
  "Custom isearch filter that expands collapsed fragments when matches are found.
BEG and END define the match region."
  ;; Check if the match contains invisible text
  (let ((pos beg)
        (found-invisible nil))
    (while (and (< pos end) (not found-invisible))
      (when (get-text-property pos 'invisible)
        (setq found-invisible t))
      (setq pos (1+ pos)))

    ;; If we found invisible text, expand the fragment
    (when found-invisible
      (save-excursion
        (goto-char beg)
        (when-let* ((state (get-text-property (point) 'agent-shell-ui-state))
                    (qualified-id (map-elt state :qualified-id))
                    ((map-elt state :collapsed)))
          ;; Track which fragments we've opened
          (unless (member qualified-id agent-shell-ui--isearch-opened-fragments)
            (push qualified-id agent-shell-ui--isearch-opened-fragments))
          ;; Expand the fragment
          (agent-shell-ui-toggle-fragment-at-point))))

    ;; Always return t to include the match
    t))

(defun agent-shell-ui--isearch-cleanup ()
  "Clean up isearch state when search ends."
  (setq agent-shell-ui--isearch-opened-fragments nil))

(defvar agent-shell-ui-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `agent-shell-ui-mode'.")

;;;###autoload
(define-minor-mode agent-shell-ui-mode
  "Minor mode for SUI block navigation."
  :lighter " SUI"
  :keymap agent-shell-ui-mode-map
  (if agent-shell-ui-mode
      (progn
        (cursor-sensor-mode 1)
        ;; Enable searching in invisible text and auto-expansion
        (setq-local search-invisible 'open-all)
        ;; Use custom filter predicate to expand fragments during search
        (setq-local isearch-filter-predicate #'agent-shell-ui--isearch-filter-predicate)
        ;; Clean up when search ends
        (add-hook 'isearch-mode-end-hook #'agent-shell-ui--isearch-cleanup nil 'local))
    (cursor-sensor-mode -1)
    (kill-local-variable 'search-invisible)
    (kill-local-variable 'isearch-filter-predicate)
    (remove-hook 'isearch-mode-end-hook #'agent-shell-ui--isearch-cleanup 'local)))

(provide 'agent-shell-ui)

;;; agent-shell-ui.el ends here
