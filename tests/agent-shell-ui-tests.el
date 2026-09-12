;;; agent-shell-ui-tests.el --- Tests for agent-shell-ui -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-ui)

;;; Code:

(ert-deftest agent-shell-ui-body-invisible-p-handles-whitespace-only-body ()
  ;; Regression for PR #597 (pi-acp): the markdown renderer strips
  ;; an empty `\\`\\`\\`console' fence down to a body of only
  ;; newlines.  On the next `agent-shell-ui--replace-body',
  ;; `--body-invisible-p' must still report the body as hidden when
  ;; its chars carry `invisible t' — otherwise new chars come in
  ;; visible and the fragment "expands" on every subsequent update
  ;; while still showing the `▶' collapsed indicator.
  (with-temp-buffer
    (insert "\n\n")
    (add-text-properties (point-min) (point-max) '(invisible t))
    (should (agent-shell-ui--body-invisible-p (point-min) (point-max))))
  (with-temp-buffer
    (insert "\n\n")
    (should-not (agent-shell-ui--body-invisible-p (point-min) (point-max)))))

(ert-deftest agent-shell-ui-indent-text-preserves-caller-text-properties ()
  ;; A pre-rendered body (eg. a diff tagged `agent-shell-markdown-frozen')
  ;; passes through `--indent-text' on its way into the fragment buffer.
  ;; Every char of the indented result — including the inter-line `\\n's
  ;; — must keep the caller's text properties, otherwise the markdown
  ;; renderer's contiguous frozen-range collapses per-line and the
  ;; header / blockquote passes match across the now-bare line breaks.
  ;; See PR #597.
  (let* ((input (propertize "line one\nline two\nline three"
                            'agent-shell-markdown-frozen t))
         (out (agent-shell-ui--indent-text input "  ")))
    (dotimes (i (length out))
      (should (eq t (get-text-property i 'agent-shell-markdown-frozen out)))
      (should (equal "  " (get-text-property i 'line-prefix out)))
      (should (equal "  " (get-text-property i 'wrap-prefix out))))))


(defun agent-shell-ui-tests--make-buffer-with-fragments (fragments)
  "Create a temp buffer with FRAGMENTS inserted.

FRAGMENTS is a list of alists, each with keys :namespace-id,
:block-id, :label-left, :body, and optionally :expanded.

Example:

  (agent-shell-ui-tests--make-buffer-with-fragments
   \\='(((:namespace-id . \"ns\") (:block-id . \"1\")
      (:label-left . \"First\") (:body . \"body one\")
      (:expanded . t))
     ((:namespace-id . \"ns\") (:block-id . \"2\")
      (:label-left . \"Second\") (:body . \"body two\"))))

Returns the buffer.  Caller must kill it."
  (let ((buf (generate-new-buffer " *test-ui-fragments*")))
    (with-current-buffer buf
      (agent-shell-ui-mode 1)
      (dolist (frag fragments)
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id (map-elt frag :namespace-id)
          :block-id (map-elt frag :block-id)
          :label-left (map-elt frag :label-left)
          :label-right (map-elt frag :label-right)
          :body (map-elt frag :body))
         :expanded (map-elt frag :expanded)
         :navigation 'always)))
    buf))

(defun agent-shell-ui-tests--fragment-collapsed-p (namespace-id block-id)
  "Return non-nil when fragment NAMESPACE-ID/BLOCK-ID is collapsed."
  (let ((qualified-id (format "%s-%s" namespace-id block-id)))
    (save-mark-and-excursion
      (goto-char (point-min))
      (when-let* ((match (text-property-search-forward
                         'agent-shell-ui-state nil
                         (lambda (_ state)
                           (equal (map-elt state :qualified-id) qualified-id))
                         t)))
        (map-elt (get-text-property (prop-match-beginning match)
                                    'agent-shell-ui-state)
                 :collapsed)))))

;;; majority-collapsed-p

(ert-deftest agent-shell-ui-majority-collapsed-all-collapsed-test ()
  "All collapsed fragments yields non-nil."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "A") (:body . "body a"))
                ((:namespace-id . "ns") (:block-id . "2")
                 (:label-left . "B") (:body . "body b"))
                ((:namespace-id . "ns") (:block-id . "3")
                 (:label-left . "C") (:body . "body c"))))))
    (unwind-protect
        (with-current-buffer buf
          (should (agent-shell-ui--majority-collapsed-p)))
      (kill-buffer buf))))

(ert-deftest agent-shell-ui-majority-collapsed-all-expanded-test ()
  "All expanded fragments yields nil."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "A") (:body . "body a") (:expanded . t))
                ((:namespace-id . "ns") (:block-id . "2")
                 (:label-left . "B") (:body . "body b") (:expanded . t))
                ((:namespace-id . "ns") (:block-id . "3")
                 (:label-left . "C") (:body . "body c") (:expanded . t))))))
    (unwind-protect
        (with-current-buffer buf
          (should-not (agent-shell-ui--majority-collapsed-p)))
      (kill-buffer buf))))

(ert-deftest agent-shell-ui-majority-collapsed-mixed-test ()
  "Three collapsed, two expanded yields non-nil."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "A") (:body . "body a"))
                ((:namespace-id . "ns") (:block-id . "2")
                 (:label-left . "B") (:body . "body b") (:expanded . t))
                ((:namespace-id . "ns") (:block-id . "3")
                 (:label-left . "C") (:body . "body c"))
                ((:namespace-id . "ns") (:block-id . "4")
                 (:label-left . "D") (:body . "body d") (:expanded . t))
                ((:namespace-id . "ns") (:block-id . "5")
                 (:label-left . "E") (:body . "body e"))))))
    (unwind-protect
        (with-current-buffer buf
          (should (agent-shell-ui--majority-collapsed-p)))
      (kill-buffer buf))))

;;; toggle-all-fragments

(ert-deftest agent-shell-ui-toggle-all-collapses-expanded-test ()
  "Toggling when all expanded collapses everything."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "A") (:body . "body a") (:expanded . t))
                ((:namespace-id . "ns") (:block-id . "2")
                 (:label-left . "B") (:body . "body b") (:expanded . t))))))
    (unwind-protect
        (with-current-buffer buf
          (agent-shell-ui-toggle-all-fragments)
          (should (agent-shell-ui-tests--fragment-collapsed-p "ns" "1"))
          (should (agent-shell-ui-tests--fragment-collapsed-p "ns" "2"))
          (should (eq agent-shell-ui--fold-toggle-state 'collapsed)))
      (kill-buffer buf))))

(ert-deftest agent-shell-ui-toggle-all-expands-collapsed-test ()
  "Toggling when all collapsed expands everything."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "A") (:body . "body a"))
                ((:namespace-id . "ns") (:block-id . "2")
                 (:label-left . "B") (:body . "body b"))))))
    (unwind-protect
        (with-current-buffer buf
          (agent-shell-ui-toggle-all-fragments)
          (should-not (agent-shell-ui-tests--fragment-collapsed-p "ns" "1"))
          (should-not (agent-shell-ui-tests--fragment-collapsed-p "ns" "2"))
          (should (eq agent-shell-ui--fold-toggle-state 'expanded)))
      (kill-buffer buf))))

(ert-deftest agent-shell-ui-toggle-all-round-trip-test ()
  "Toggling twice returns to original state."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "A") (:body . "body a") (:expanded . t))
                ((:namespace-id . "ns") (:block-id . "2")
                 (:label-left . "B") (:body . "body b") (:expanded . t))))))
    (unwind-protect
        (with-current-buffer buf
          ;; First toggle: collapse all
          (agent-shell-ui-toggle-all-fragments)
          (should (agent-shell-ui-tests--fragment-collapsed-p "ns" "1"))
          ;; Second toggle: expand all
          (agent-shell-ui-toggle-all-fragments)
          (should-not (agent-shell-ui-tests--fragment-collapsed-p "ns" "1"))
          (should-not (agent-shell-ui-tests--fragment-collapsed-p "ns" "2")))
      (kill-buffer buf))))

;;; enclosing-fragment-position

(ert-deftest agent-shell-ui-enclosing-position-on-fragment-test ()
  "When point is on a fragment, return point."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "A") (:body . "body a") (:expanded . t))))))
    (unwind-protect
        (with-current-buffer buf
          ;; Move to a position that has agent-shell-ui-state
          (goto-char (point-min))
          (text-property-search-forward 'agent-shell-ui-state nil
                                        (lambda (_ s) (and s t)) t)
          (goto-char (prop-match-beginning
                      (save-mark-and-excursion
                        (text-property-search-backward
                         'agent-shell-ui-state nil
                         (lambda (_ s) (and s t)) t))))
          (should (equal (agent-shell-ui--enclosing-fragment-position)
                         (point))))
      (kill-buffer buf))))

(ert-deftest agent-shell-ui-enclosing-position-nil-in-empty-buffer-test ()
  "Empty buffer returns nil."
  (let ((buf (generate-new-buffer " *test-ui-empty*")))
    (unwind-protect
        (with-current-buffer buf
          (agent-shell-ui-mode 1)
          (should-not (agent-shell-ui--enclosing-fragment-position)))
      (kill-buffer buf))))

;;; toggle-fragment

(ert-deftest agent-shell-ui-toggle-fragment-on-fragment-test ()
  "Toggle on a fragment toggles it."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "A") (:body . "body a") (:expanded . t))))))
    (unwind-protect
        (with-current-buffer buf
          ;; Position on the fragment
          (goto-char (point-min))
          (text-property-search-forward 'agent-shell-ui-state nil
                                        (lambda (_ s) (and s t)) t)
          (goto-char (prop-match-beginning
                      (save-mark-and-excursion
                        (text-property-search-backward
                         'agent-shell-ui-state nil
                         (lambda (_ s) (and s t)) t))))
          ;; Fragment starts expanded, toggle should collapse it
          (agent-shell-ui-toggle-fragment)
          (should (agent-shell-ui-tests--fragment-collapsed-p "ns" "1")))
      (kill-buffer buf))))

(ert-deftest agent-shell-ui-toggle-survives-surgical-replace-test ()
  "Toggle target stays consistent after `--surgical-replace-body'.

Surgical replace mints a fresh state plist on the new body chars
but `:qualified-id` is stable.  Toggle resolves the target via
`:qualified-id` so it still hits the right fragment."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "Tool") (:body . "initial")
                 (:expanded . t))))))
    (unwind-protect
        (with-current-buffer buf
          (agent-shell-ui-update-fragment
           (agent-shell-ui-make-fragment-model
            :namespace-id "ns" :block-id "1"
            :body "replaced body content")
           :append nil :navigation 'always)
          (goto-char (point-min))
          (text-property-search-forward 'agent-shell-ui-state nil
                                        (lambda (_ s) (and s t)) t)
          (goto-char (prop-match-beginning
                      (save-mark-and-excursion
                        (text-property-search-backward
                         'agent-shell-ui-state nil
                         (lambda (_ s) (and s t)) t))))
          (agent-shell-ui-toggle-fragment)
          (should (agent-shell-ui-tests--fragment-collapsed-p "ns" "1")))
      (kill-buffer buf))))

(defun agent-shell-ui-tests--visible-body-p ()
  "Return non-nil if any body-section char in the buffer is visible.
A collapsed fragment must keep every body char `invisible'."
  (save-mark-and-excursion
    (goto-char (point-min))
    (catch 'visible
      (while (< (point) (point-max))
        (when (and (eq (get-text-property (point) 'agent-shell-ui-section) 'body)
                   (not (get-text-property (point) 'invisible)))
          (throw 'visible t))
        (goto-char (or (next-single-property-change (point) 'agent-shell-ui-section)
                       (point-max))))
      nil)))

(ert-deftest agent-shell-ui-body-stays-collapsed-after-label-length-change-test ()
  "A collapsed body stays hidden when a label update changes label length.

A combined label+body update replaces the label first, which can change
its length and shift the body below it.  Deriving the body range before
that replacement leaves it stale, so `--replace-body' corrupts the body
boundary and leaks the collapsed content into view (e.g. a diff spilling
out of a collapsed edit tool call).  The label-right sits right above the
body, so growing it shifts the body the most.  The body must stay
invisible across the update."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "Edit") (:label-right . "short")
                 (:body . "first body\nsecond line"))))))
    (unwind-protect
        (with-current-buffer buf
          ;; Sanity: the body starts collapsed (hidden).
          (should-not (agent-shell-ui-tests--visible-body-p))
          ;; Grow the adjacent label-right and replace the body at once.
          (agent-shell-ui-update-fragment
           (agent-shell-ui-make-fragment-model
            :namespace-id "ns" :block-id "1"
            :label-left "Edit" :label-right "a much longer right label than before"
            :body "second body content\nanother line")
           :append nil :navigation 'always)
          (should-not (agent-shell-ui-tests--visible-body-p)))
      (kill-buffer buf))))

;;; delete-fragment

(ert-deftest agent-shell-ui-delete-fragment-preserves-next-indicator-test ()
  "Deleting a fragment keeps the following fragment's leading indicator.

A collapsed labels-only fragment reserves a two-space indicator
placeholder for column alignment.  Deleting the fragment right above it
must not consume that placeholder.  Regression: a permission dialog
deleted on tool-call completion swallowed the next tool call's indent
because `agent-shell-ui-delete-fragment' skipped trailing whitespace
straight into the next block's leading spaces."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "top")
                 (:label-left . "Top"))
                ((:namespace-id . "ns") (:block-id . "next")
                 (:label-left . "Next"))))))
    (unwind-protect
        (with-current-buffer buf
          (agent-shell-ui-delete-fragment :namespace-id "ns" :block-id "top")
          (let ((start (agent-shell-ui-tests--fragment-start "ns-next")))
            (should start)
            (should (equal "  " (buffer-substring-no-properties start (+ start 2))))))
      (kill-buffer buf))))

;;; groups

(defun agent-shell-ui-tests--group-child-ids (group-qualified-id)
  "Return the ordered child qualified-ids of GROUP-QUALIFIED-ID."
  (mapcar (lambda (c) (map-elt c :qualified-id))
          (agent-shell-ui--group-children :group-qualified-id group-qualified-id)))

(ert-deftest agent-shell-ui-group-auto-creates-header-and-nests-children-test ()
  "A child with a `:group-id' auto-creates the header and nests under it."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (dolist (id '("t1" "t2"))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id "ns" :block-id id :group-id "grp" :group-label "Tools"
        :label-left "run" :label-right id)
       :navigation 'always))
    (should (agent-shell-ui--group-header-range "ns-grp"))
    (should (equal '("ns-t1" "ns-t2")
                   (agent-shell-ui-tests--group-child-ids "ns-grp")))))

(ert-deftest agent-shell-ui-group-reports-created-header-range-test ()
  "Creating a group header reports its range; a later child reports none.
The header is inserted on its own, outside any child's block/padding, so
`agent-shell-ui-update-fragment' must hand its extent back to the caller
via `:group-header' (callers mark output over that span so navigation
does not stop mid-header).  The span covers the header and its padding,
and only the header-creating call reports it."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (let* ((first (agent-shell-ui-update-fragment
                   (agent-shell-ui-make-fragment-model
                    :namespace-id "ns" :block-id "t1" :group-id "grp"
                    :group-label "Tools" :label-left "run" :label-right "t1")
                   :navigation 'always))
           (second (agent-shell-ui-update-fragment
                    (agent-shell-ui-make-fragment-model
                     :namespace-id "ns" :block-id "t2" :group-id "grp"
                     :group-label "Tools" :label-left "run" :label-right "t2")
                    :navigation 'always))
           (header (agent-shell-ui--group-header-range "ns-grp"))
           (gh-start (map-nested-elt first '(:group-header :start)))
           (gh-end (map-nested-elt first '(:group-header :end))))
      ;; First child (which materialized the header) reports its range.
      (should gh-start)
      (should gh-end)
      ;; Span encloses the header block itself.
      (should (<= gh-start (map-elt header :start)))
      (should (>= gh-end (map-elt header :end)))
      ;; A second child into the same group creates no header, reports none.
      (should-not (map-elt second :group-header)))))

(ert-deftest agent-shell-ui-group-child-padding-abuts-following-block-test ()
  "A grouped child's padding reaches the following top-level block's padding.
The group's trailing separator (the header's `\\n\\n') is pushed below the
child, belonging to neither the header block nor the child.  The child
must fold it into its padding so the reported ranges tile with no gap;
otherwise that separator is left outside every block's range (and a
caller stamping ranges leaves it unmarked, stranding navigation there)."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (let* ((child (agent-shell-ui-update-fragment
                    (agent-shell-ui-make-fragment-model
                     :namespace-id "0" :block-id "th" :label-left "Thinking"
                     :body "pondering" :group-id "grp" :group-label "Activity")
                    :create-new t :expanded t))
           (top (agent-shell-ui-update-fragment
                 (agent-shell-ui-make-fragment-model
                  :namespace-id "0" :block-id "msg" :body "Answer")
                 :create-new t)))
      (should (= (map-nested-elt child '(:padding :end))
                 (map-nested-elt top '(:padding :start)))))))

(ert-deftest agent-shell-ui-group-collapse-hides-children-and-restores-state-test ()
  "Collapsing a group hides every child; expanding restores per-child folds."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    ;; m1 stays collapsed (default), m2 expanded.
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "m1" :group-id "grp" :group-label "T"
      :label-left "run" :label-right "a" :body "aa")
     :navigation 'always)
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "m2" :group-id "grp" :group-label "T"
      :label-left "run" :label-right "b" :body "bb")
     :expanded t :navigation 'always)
    (cl-flet ((child-start (n) (map-elt (nth n (agent-shell-ui--group-children
                                                 :group-qualified-id "ns-grp"))
                                         :start))
              (body-start (n) (let ((c (nth n (agent-shell-ui--group-children
                                               :group-qualified-id "ns-grp"))))
                                (map-elt (agent-shell-ui--nearest-range-matching-property
                                          :property 'agent-shell-ui-section :value 'body
                                          :from (map-elt c :start) :to (map-elt c :end))
                                         :start))))
      ;; Collapse: both child header lines hidden.
      (let ((inhibit-read-only t)) (agent-shell-ui--set-group-collapsed "ns-grp" t))
      (should (get-text-property (child-start 0) 'invisible))
      (should (get-text-property (child-start 1) 'invisible))
      ;; Expand: headers visible; m1 body stays hidden, m2 body visible.
      (let ((inhibit-read-only t)) (agent-shell-ui--set-group-collapsed "ns-grp" nil))
      (should-not (get-text-property (child-start 0) 'invisible))
      (should-not (get-text-property (child-start 1) 'invisible))
      (should (get-text-property (body-start 0) 'invisible))
      (should-not (get-text-property (body-start 1) 'invisible)))))

(ert-deftest agent-shell-ui-set-group-collapsed-by-id-test ()
  "Setting a group's fold state by id is idempotent and leaves leaves alone."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "m1" :group-id "grp" :group-label "T"
      :label-left "run" :label-right "a" :body "aa")
     :navigation 'always)
    ;; A leaf fragment, to guard that only group headers are targeted.
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "leaf" :label-left "msg" :body "bb")
     :expanded t :navigation 'always)
    (cl-flet ((child-hidden-p ()
                (get-text-property
                 (map-elt (car (agent-shell-ui--group-children
                                :group-qualified-id "ns-grp"))
                          :start)
                 'invisible)))
      (should-not (child-hidden-p))
      (agent-shell-ui-set-group-collapsed-by-id
       :namespace-id "ns" :block-id "grp" :collapsed t)
      (should (child-hidden-p))
      ;; Repeat calls are a no-op rather than a toggle.
      (agent-shell-ui-set-group-collapsed-by-id
       :namespace-id "ns" :block-id "grp" :collapsed t)
      (should (child-hidden-p))
      (agent-shell-ui-set-group-collapsed-by-id
       :namespace-id "ns" :block-id "grp" :collapsed nil)
      (should-not (child-hidden-p))
      ;; Unknown ids and non-group fragments are left untouched.
      (agent-shell-ui-set-group-collapsed-by-id
       :namespace-id "ns" :block-id "missing" :collapsed t)
      (agent-shell-ui-set-group-collapsed-by-id
       :namespace-id "ns" :block-id "leaf" :collapsed t)
      (should-not (child-hidden-p)))))

(ert-deftest agent-shell-ui-group-child-streams-body-stays-nested-test ()
  "A labels-only child that later gains a body stays nested and indented."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "m1" :group-id "grp" :group-label "T"
      :label-left "run" :label-right "a")
     :navigation 'always)
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "m1" :group-id "grp" :group-label "T"
      :label-left "run" :label-right "a" :body "streamed body")
     :navigation 'always)
    (should (equal '("ns-m1") (agent-shell-ui-tests--group-child-ids "ns-grp")))
    (let* ((child (car (agent-shell-ui--group-children
                             :group-qualified-id "ns-grp")))
           (body (agent-shell-ui--nearest-range-matching-property
                  :property 'agent-shell-ui-section :value 'body
                  :from (map-elt child :start) :to (map-elt child :end))))
      ;; group indent (2) + body indent (2) = 4.
      (should (equal "    " (get-text-property (map-elt body :start) 'line-prefix))))))

(ert-deftest agent-shell-ui-group-update-existing-child-keeps-group-test ()
  "Updating an existing child never spawns a new group header.
Regression: a caller whose group-id advanced (a message streamed between
a tool call and its completion) must not create an empty group; the
child stays in its original group."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    ;; Child created in group g1.
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "m1" :group-id "g1" :group-label "T"
      :label-left "… run" :label-right "a")
     :navigation 'always)
    ;; Same child updated, but the caller now passes a *different* group.
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "m1" :group-id "g2" :group-label "T"
      :label-left "✓ run" :label-right "a" :body "output")
     :navigation 'always)
    ;; No g2 header; the child is still the sole child of g1, indented.
    (should-not (agent-shell-ui--group-header-range "ns-g2"))
    (should (agent-shell-ui--group-header-range "ns-g1"))
    (let ((kids (agent-shell-ui--group-children :group-qualified-id "ns-g1")))
      (should (equal '("ns-m1") (mapcar (lambda (c) (map-elt c :qualified-id)) kids)))
      ;; Body regenerated on update keeps the group+body indent (4).
      (let ((body (agent-shell-ui--nearest-range-matching-property
                   :property 'agent-shell-ui-section :value 'body
                   :from (map-elt (car kids) :start) :to (map-elt (car kids) :end))))
        (should (equal "    " (get-text-property (map-elt body :start) 'line-prefix)))))))

(ert-deftest agent-shell-ui-group-child-added-while-collapsed-stays-hidden-test ()
  "A child added to a folded group is hidden, not popped into view."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "m1" :group-id "grp" :group-label "T"
      :label-left "run" :label-right "a")
     :navigation 'always)
    (let ((inhibit-read-only t)) (agent-shell-ui--set-group-collapsed "ns-grp" t))
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "m2" :group-id "grp" :group-label "T"
      :label-left "run" :label-right "b")
     :navigation 'always)
    (let ((kids (agent-shell-ui--group-children :group-qualified-id "ns-grp")))
      (should (equal '("ns-m1" "ns-m2")
                     (mapcar (lambda (c) (map-elt c :qualified-id)) kids)))
      (dolist (c kids)
        (should (get-text-property (map-elt c :start) 'invisible))))))

(ert-deftest agent-shell-ui-group-collapsed-child-update-stays-hidden-test ()
  "Updating a child in a collapsed group keeps it hidden (no leak).
Regression: a child's in-place edit restored its own visibility while the
separators stayed hidden, collapsing children onto the folded header line."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    ;; Group created collapsed; two labels-only children.
    (dolist (m '("m1" "m2"))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id "ns" :block-id m :group-id "grp" :group-label "T"
        :group-expanded nil :label-left "… run" :label-right m)
       :navigation 'always))
    ;; Update m1 with a body (as a completion would).
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "m1" :group-id "grp" :group-label "T"
      :group-expanded nil :label-left "✓ run" :label-right "m1" :body "output")
     :navigation 'always)
    ;; Every child, including the just-updated one, stays hidden.
    (dolist (c (agent-shell-ui--group-children :group-qualified-id "ns-grp"))
      (should (get-text-property (map-elt c :start) 'invisible))
      ;; A position strictly inside the block is hidden too (not just the
      ;; leading char), so child content can't leak onto the header line.
      (should (get-text-property (1+ (map-elt c :start)) 'invisible)))))

(ert-deftest agent-shell-ui-group-children-end-matches-enumeration-test ()
  "`--group-children-end' matches enumeration, and stops at a non-child.
It replaces `--group-children' where only the last child's end is
needed, so the two must resolve the same position."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (dolist (m '("m1" "m2" "m3"))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id "ns" :block-id m :group-id "grp" :group-label "T"
        :label-left "run" :label-right m :body "output")
       :navigation 'always))
    ;; A block after the group, so the run has something to stop at.
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "after" :label-left "After")
     :navigation 'always)
    (let ((header (agent-shell-ui--group-header-range "ns-grp"))
          (children (agent-shell-ui--group-children :group-qualified-id "ns-grp")))
      (should (equal '("ns-m1" "ns-m2" "ns-m3")
                     (mapcar (lambda (c) (map-elt c :qualified-id)) children)))
      (should (equal (map-elt (car (last children)) :end)
                     (agent-shell-ui--group-children-end "ns-grp"
                                                        (map-elt header :end))))
      ;; Empty group: nil, so callers fall back to the header's end.
      (should-not (agent-shell-ui--group-children-end "ns-grp" (point-max))))))

(ert-deftest agent-shell-ui-block-cache-survives-buffer-erase-test ()
  "Rebuilding after an erase renders as if nothing had been cached.
The viewport erases and rebuilds itself, which collapses every cached
marker to `point-min' while the same fragment ids come back.  Entries are
verified against the buffer on use rather than invalidated on change, so
this is the case that has to fail those checks and fall back to
searching."
  (cl-flet ((populate ()
              (dolist (m '("a" "b" "c"))
                (agent-shell-ui-update-fragment
                 (agent-shell-ui-make-fragment-model
                  :namespace-id "ns" :block-id m :label-left "run" :label-right m
                  :body "output\n")
                 :navigation 'always)
                (dotimes (_ 3)
                  (agent-shell-ui-update-fragment
                   (agent-shell-ui-make-fragment-model
                    :namespace-id "ns" :block-id m :body "more\n")
                   :append t)))))
    (let ((reference (with-temp-buffer
                       (agent-shell-ui-mode 1)
                       (populate)
                       (buffer-substring-no-properties (point-min) (point-max)))))
      (with-temp-buffer
        (agent-shell-ui-mode 1)
        (populate)
        (should agent-shell-ui--block-cache)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (populate)
        (should (equal reference
                       (buffer-substring-no-properties (point-min)
                                                       (point-max))))))))

(ert-deftest agent-shell-ui-group-streamed-body-stays-hidden-test ()
  "Chunks streamed into a folded group stay hidden, every char of them.
An append into an already hidden body hides what it writes, so the
group's fold is not re-applied afterwards.  This is what makes that safe:
the first chunk lands in an empty body, later ones extend a hidden one,
and a label update comes through the middle."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "m" :group-id "grp" :group-label "T"
      :group-expanded nil :label-left "thinking" :body "")
     :navigation 'always)
    (dotimes (i 12)
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id "ns" :block-id "m" :body (format "chunk %d\n" i))
       :append t)
      (when (= i 5)
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id "ns" :block-id "m" :label-left "still thinking"))))
    (let ((region (agent-shell-ui--group-child-region
                   :group-qualified-id "ns-grp")))
      (should region)
      (should (> (- (map-elt region :end) (map-elt region :start)) 12))
      (dolist (position (number-sequence (map-elt region :start)
                                         (1- (map-elt region :end))))
        (should (get-text-property position 'invisible))))))

(ert-deftest agent-shell-ui-group-header-range-tracks-relabel-test ()
  "A cached group header range follows the header's own edits.
`agent-shell-ui--group-header-range' answers from
`agent-shell-ui--block-cache', whose end is a marker.  A relabel that
changes the header's width has to leave that end where a fresh search
would put it."
  (cl-flet ((searched ()
              (save-mark-and-excursion
                (goto-char (point-min))
                (when-let* ((match (text-property-search-forward
                                    'agent-shell-ui-state nil
                                    (lambda (_ state)
                                      (and (equal (map-elt state :qualified-id)
                                                  "ns-grp")
                                           (eq (map-elt state :kind) 'group)))
                                    t)))
                  (agent-shell-ui--block-range
                   :position (prop-match-beginning match))))))
    (with-temp-buffer
      (agent-shell-ui-mode 1)
      (dolist (m '("m1" "m2"))
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id "ns" :block-id m :group-id "grp" :group-label "T"
          :label-left "run" :label-right m :body "output\n")
         :navigation 'always))
      (should (equal (searched) (agent-shell-ui--group-header-range "ns-grp")))
      ;; Relabel the header to something much wider, then much narrower.
      (dolist (label '("A considerably longer group label" "T"))
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id "ns" :block-id "grp" :label-left label))
        (should (equal (searched)
                       (agent-shell-ui--group-header-range "ns-grp")))))))

(ert-deftest agent-shell-ui-group-navigation-skips-collapsed-children-test ()
  "Forward navigation steps into visible children but skips folded ones."
  (cl-flet ((walk ()
              (goto-char (point-min))
              (let (visited)
                (while (agent-shell-ui-forward-block)
                  (push (map-elt (get-text-property (point) 'agent-shell-ui-state)
                                 :qualified-id)
                        visited))
                (nreverse visited))))
    (with-temp-buffer
      (agent-shell-ui-mode 1)
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id "d" :block-id "before" :label-left "Before")
       :navigation 'always)
      (dolist (m '("m1" "m2"))
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id "d" :block-id m :group-id "g" :group-label "G"
          :label-left "run" :label-right m :body "b")
         :navigation 'always))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id "d" :block-id "after" :label-left "After")
       :navigation 'always)
      ;; Expanded: header and both children are visited.
      (should (equal '("d-before" "d-g" "d-m1" "d-m2" "d-after") (walk)))
      ;; Collapsed: children are skipped.
      (let ((inhibit-read-only t)) (agent-shell-ui--set-group-collapsed "d-g" t))
      (should (equal '("d-before" "d-g" "d-after") (walk))))))

(ert-deftest agent-shell-ui-group-delete-child-keeps-header-test ()
  "Deleting a child leaves the header and the remaining children intact."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (dolist (id '("m1" "m2"))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id "ns" :block-id id :group-id "grp" :group-label "T"
        :label-left "run" :label-right id)
       :navigation 'always))
    (agent-shell-ui-delete-fragment :namespace-id "ns" :block-id "m1")
    (should (agent-shell-ui--group-header-range "ns-grp"))
    (should (equal '("ns-m2") (agent-shell-ui-tests--group-child-ids "ns-grp")))))

(ert-deftest agent-shell-ui-group-children-stops-when-block-range-stalls-test ()
  "Child enumeration stops when a block range fails to advance.
The child walk moves to each block's `:end', with nothing requiring that
end to move forward.  A range ending at or behind the walk position, or a
nil range, used to re-examine the same child forever and cons on every
pass until Emacs stopped responding."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (dolist (id '("t1" "t2"))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id "ns" :block-id id :group-id "grp" :group-label "Tools"
        :label-left "run" :label-right id)
       :navigation 'always))
    (should (equal '("ns-t1" "ns-t2")
                   (agent-shell-ui-tests--group-child-ids "ns-grp")))
    (let ((block-range (symbol-function 'agent-shell-ui--block-range)))
      ;; Children report a stalled range; the header keeps its real one so the
      ;; walk still starts.
      (cl-letf (((symbol-function 'agent-shell-ui--block-range)
                 (lambda (&rest args)
                   (if (eq 'group (map-elt (get-text-property
                                            (plist-get args :position)
                                            'agent-shell-ui-state)
                                           :kind))
                       (apply block-range args)
                     '((:start . 1) (:end . 1))))))
        (should-not (agent-shell-ui-tests--group-child-ids "ns-grp")))
      (cl-letf (((symbol-function 'agent-shell-ui--block-range)
                 (lambda (&rest args)
                   (when (eq 'group (map-elt (get-text-property
                                              (plist-get args :position)
                                              'agent-shell-ui-state)
                                             :kind))
                     (apply block-range args)))))
        (should-not (agent-shell-ui-tests--group-child-ids "ns-grp"))))))

;;; backward-block

(defun agent-shell-ui-tests--fragment-start (qualified-id)
  "Return the start position of fragment QUALIFIED-ID, or nil."
  (save-mark-and-excursion
    (goto-char (point-min))
    (when-let* ((match (text-property-search-forward
                        'agent-shell-ui-state nil
                        (lambda (_ state)
                          (equal (map-elt state :qualified-id) qualified-id))
                        t)))
      (prop-match-beginning match))))

(ert-deftest agent-shell-ui-backward-block-from-inside-goes-to-own-start-test ()
  "`agent-shell-ui-backward-block' from inside a block goes to its own start.

From the block's start it then jumps to the previous block."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "First") (:body . "body one") (:expanded . t))
                ((:namespace-id . "ns") (:block-id . "2")
                 (:label-left . "Second") (:body . "body two") (:expanded . t))))))
    (unwind-protect
        (with-current-buffer buf
          (let ((first-start (agent-shell-ui-tests--fragment-start "ns-1"))
                (second-start (agent-shell-ui-tests--fragment-start "ns-2")))
            ;; Strictly inside the second block -> its own start.
            (goto-char (+ second-start 3))
            (should (equal (agent-shell-ui-backward-block) second-start))
            ;; At the second block's start -> the previous block.
            (goto-char second-start)
            (should (equal (agent-shell-ui-backward-block) first-start))))
      (kill-buffer buf))))

(ert-deftest agent-shell-ui-backward-block-skips-non-navigatable-block-test ()
  "`agent-shell-ui-backward-block' skips non-navigatable blocks.

From inside a non-navigatable block it lands on the previous
navigatable block, not on the non-navigatable block's own start."
  (let ((buf (generate-new-buffer " *test-ui-fragments*")))
    (unwind-protect
        (with-current-buffer buf
          (agent-shell-ui-mode 1)
          (agent-shell-ui-update-fragment
           (agent-shell-ui-make-fragment-model
            :namespace-id "ns" :block-id "1"
            :label-left "First" :body "body one")
           :expanded t :navigation 'always)
          (agent-shell-ui-update-fragment
           (agent-shell-ui-make-fragment-model
            :namespace-id "ns" :block-id "2"
            :label-left "Second" :body "body two")
           :expanded t :navigation 'never)
          (let ((first-start (agent-shell-ui-tests--fragment-start "ns-1"))
                (non-nav-start (agent-shell-ui-tests--fragment-start "ns-2")))
            (goto-char (+ non-nav-start 3))
            (should (equal (agent-shell-ui-backward-block) first-start))))
      (kill-buffer buf))))

(ert-deftest agent-shell-ui-left-label-recolor-without-text-change-test ()
  "A left label that changes only its face is still redrawn.
Regression: the rewrite guard compared text alone, so a status style
carrying its state in the face (e.g. a box whose color separates pending
from completed) kept the color it was first rendered with."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (dolist (face '(agent-shell-pending agent-shell-success))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id "ns" :block-id "1"
        :label-left (propertize " edit " 'font-lock-face face)
        :label-right "file.el")
       :navigation 'always))
    (should (equal 'agent-shell-success
                   (get-text-property
                    (map-elt (agent-shell-ui--nearest-range-matching-property
                              :property 'agent-shell-ui-section :value 'label-left
                              :from (point-min) :to (point-max))
                             :start)
                    'font-lock-face)))))

(ert-deftest agent-shell-ui-right-label-face-change-skips-rewrite-test ()
  "A right label whose text is unchanged is left alone.
Markdown renders over right labels in place, so their buffer faces stop
matching the text handed in.  Comparing faces there would rewrite the
title on every streamed chunk, discarding the rendering each time."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (dolist (face '(agent-shell-section-heading agent-shell-success))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id "ns" :block-id "1"
        :label-left " edit "
        :label-right (propertize "file.el" 'font-lock-face face))
       :navigation 'always))
    (should (equal 'agent-shell-section-heading
                   (get-text-property
                    (map-elt (agent-shell-ui--nearest-range-matching-property
                              :property 'agent-shell-ui-section :value 'label-right
                              :from (point-min) :to (point-max))
                             :start)
                    'font-lock-face)))))

(ert-deftest agent-shell-ui-group-child-without-labels-test ()
  "A group child with neither label renders instead of erroring.

A tool call carrying only a `toolCallId' has no status, kind, title or
description, so both its labels come back nil, and tool calls are always
group children."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id "1" :block-id "t1"
        :body "some tool output\n"
        :group-id "activity-1" :group-label "Activity")
       :no-undo t)
      (should (string-match-p "some tool output"
                              (buffer-substring-no-properties (point-min) (point-max)))))))

;;; actions

(ert-deftest agent-shell-ui-make-foldable-text-shares-one-map-test ()
  "Fragment chrome gets the shared map, never a keymap of its own.

Sharing one map is what makes rebinding it reach every fragment already
on screen."
  (let ((text (agent-shell-ui-make-foldable-text
               :text "▼ "
               :hint "toggle")))
    (should (eq agent-shell-ui-fragment-map
                (get-text-property 0 'keymap text)))
    (should (eq 'hand (get-text-property 0 'pointer text)))
    (should (get-text-property 0 'cursor-sensor-functions text))))

(ert-deftest agent-shell-ui-fragment-body-carries-no-map-test ()
  "A fragment's body reads rather than folds.

That guarantee now comes from where the map is applied, not from a
check inside a command: the chrome carries it and the body does not,
so RET in the body is left alone."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "A") (:body . "body a") (:expanded . t))))))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (should (text-property-search-forward
                   'agent-shell-ui-section 'body
                   (lambda (want got) (eq want got))))
          (should-not (get-text-property (point) 'keymap)))
      (kill-buffer buf))))

(ert-deftest agent-shell-ui-toggle-from-rendered-label-test ()
  "Folding from a rendered label works through the map on that label.

Guards the real render path, not a hand-built string: the label must
come out tagged `label-left' and carrying the fragment map, or RET
never reaches `agent-shell-ui-toggle-fragment' at all."
  (let ((buf (agent-shell-ui-tests--make-buffer-with-fragments
              '(((:namespace-id . "ns") (:block-id . "1")
                 (:label-left . "A") (:body . "body a") (:expanded . t))))))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (should (text-property-search-forward
                   'agent-shell-ui-section 'label-left
                   (lambda (want got) (eq want got))))
          (goto-char (1- (point)))
          (should (eq agent-shell-ui-fragment-map
                      (get-text-property (point) 'keymap)))
          (should-not (agent-shell-ui-tests--fragment-collapsed-p "ns" "1"))
          (agent-shell-ui-toggle-fragment)
          (should (agent-shell-ui-tests--fragment-collapsed-p "ns" "1")))
      (kill-buffer buf))))

(ert-deftest agent-shell-ui-action-hint-follows-rebound-key-test ()
  "The cursor hint names whichever key the keymap actually binds.

Issue #759: the hint used to hardcode RET, so rebinding the map left
it lying.  Mouse bindings are skipped so the hint stays pressable."
  (let ((agent-shell-ui-fragment-map (copy-keymap agent-shell-ui-fragment-map))
        (echoed nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
      (agent-shell-ui--echo-action-hint "toggle")
      (should (equal "Press RET to toggle" echoed))
      (define-key agent-shell-ui-fragment-map (kbd "TAB") #'agent-shell-ui-toggle-fragment)
      (define-key agent-shell-ui-fragment-map (kbd "RET") nil t)
      (agent-shell-ui--echo-action-hint "toggle")
      (should (equal "Press TAB to toggle" echoed)))))

(ert-deftest agent-shell-ui-action-hint-names-the-supplied-keymap-test ()
  "The hint describes the command and keymap it was handed.

Chrome bound to its own shared map needs the same \"Press KEY to VERB\"
hint pointing at its own binding, rather than at whichever key folds a
fragment."
  (let ((map (make-sparse-keymap))
        (echoed nil))
    (define-key map (kbd "C-c C-e") #'ignore)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
      (agent-shell-ui--echo-action-hint "act on this" #'ignore map)
      (should (equal "Press C-c C-e to act on this" echoed)))))

(ert-deftest agent-shell-ui-body-keeps-its-own-help-echo-test ()
  "A fragment leaves the help its body carried alone.

Adding a nil `help-echo' across the body does not merely fail to tag it,
it erases whatever help the body's own content came with, which
`display-local-help' would otherwise read out.  With debugging on the
tag does cover the body, since that mode exists to surface
qualified-ids."
  (cl-flet ((body-help (debugging)
              (with-temp-buffer
                (agent-shell-ui-mode 1)
                (let ((inhibit-read-only t)
                      (agent-shell-ui-debug-enabled debugging))
                  (agent-shell-ui-update-fragment
                   (agent-shell-ui-make-fragment-model
                    :namespace-id "ns" :block-id "1" :label-left "run"
                    :body (concat (propertize "described" 'help-echo "the content's own help")
                                  " plain\n"))
                   :expanded t :navigation 'always))
                (goto-char (point-min))
                (should (search-forward "described" nil t))
                (get-text-property (match-beginning 0) 'help-echo))))
    (should (equal (body-help nil) "the content's own help"))
    (should (equal (body-help t) "ns-1"))))

(ert-deftest agent-shell-ui-keys-reach-fragment-chrome-only-test ()
  "The fold keys sit on the chrome, leaving the rest of the buffer alone.

Scoping them to the text is what keeps RET submitting the prompt and
typing inserting everywhere else.  On the chrome, typing resolves to
`ignore', since that text is read-only and `self-insert-command' would
otherwise signal."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (insert (agent-shell-ui-make-foldable-text
             :text "label"
             :hint "toggle"))
    (insert "plain")
    (goto-char (point-min))
    (should (eq #'agent-shell-ui-toggle-fragment (key-binding (kbd "RET"))))
    (should (eq #'ignore (key-binding [?x])))
    ;; Off the chrome nothing is shadowed.  Here RET is `newline'; in a
    ;; shell buffer it submits the prompt.
    (goto-char (point-max))
    (should (eq #'newline (key-binding (kbd "RET"))))
    (should (eq #'self-insert-command (key-binding [?x])))))

(ert-deftest agent-shell-ui-replace-both-labels-keeps-block-extent-test ()
  "Replacing both labels at once leaves the block and its neighbour intact.

`agent-shell-ui--replace-label' is handed the block extent its caller
already resolved, as a marker, so the second replacement sees the width
the first one left behind."
  (cl-flet ((searched (qualified-id)
              (save-mark-and-excursion
                (goto-char (point-min))
                (when-let* ((match (text-property-search-forward
                                    'agent-shell-ui-state nil
                                    (lambda (_ state)
                                      (equal (map-elt state :qualified-id)
                                             qualified-id))
                                    t)))
                  (agent-shell-ui--block-range
                   :position (prop-match-beginning match))))))
    (with-temp-buffer
      (agent-shell-ui-mode 1)
      (dolist (block-id '("1" "2"))
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id "ns" :block-id block-id
          :label-left "run" :label-right block-id :body "output\n")
         :expanded t :navigation 'always))
      (let* ((second (searched "ns-2"))
             (untouched (buffer-substring-no-properties (map-elt second :start)
                                                        (map-elt second :end))))
        ;; Both labels change at once, first much wider then much narrower.
        (dolist (labels '(("a considerably longer status" "a considerably longer title")
                          ("ok" "1")))
          (let ((range (map-elt (agent-shell-ui-update-fragment
                                 (agent-shell-ui-make-fragment-model
                                  :namespace-id "ns" :block-id "1"
                                  :label-left (nth 0 labels)
                                  :label-right (nth 1 labels))
                                 :navigation 'always)
                                :block))
                (block (searched "ns-1")))
            ;; The extent the update reports is the one a fresh search finds.
            (should (equal block range))
            (let ((text (buffer-substring-no-properties (map-elt block :start)
                                                        (map-elt block :end))))
              (should (string-match-p (regexp-quote (nth 0 labels)) text))
              (should (string-match-p (regexp-quote (nth 1 labels)) text))
              (should (string-match-p "output" text)))
            ;; The block below keeps its own chars.
            (let ((second (searched "ns-2")))
              (should (equal untouched
                             (buffer-substring-no-properties
                              (map-elt second :start)
                              (map-elt second :end)))))))))))

;;; provide

(provide 'agent-shell-ui-tests)

;;; agent-shell-ui-tests.el ends here
