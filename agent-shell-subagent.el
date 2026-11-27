;;; agent-shell-subagent.el --- ACP sub-agent sessions -*- lexical-binding: t; -*-

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
;; Sub-agent sessions, as described by ACP's subagents spec:
;;
;;   https://agentclientprotocol.com/protocol/v1/subagents
;;
;; When the client advertises `clientCapabilities.subagents', agents
;; announce each child session with a `subagent_update' sent on its
;; parent's session, then deliver the child's updates and requests
;; under the child's own `sessionId', on the same connection.
;;
;; The main shell owns a registry of every sub-agent in its session
;; tree.  Child traffic is kept in memory, and rendered into a regular
;; (read-only) agent-shell buffer only once the user opens it, by
;; replaying what was kept.  From then on, child traffic renders live.
;;
;; In the parent, each sub-agent is shown as a single card with its
;; status, task and result.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(eval-when-compile
  (require 'cl-lib))

(declare-function agent-shell--active-requests-p "agent-shell")
(declare-function agent-shell--cancel-idle-timer-for "agent-shell")
(declare-function agent-shell--content-block-to-markdown "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell--dispatch-notification "agent-shell")
(declare-function agent-shell--emit-event "agent-shell")
(declare-function agent-shell--flush-stream-render "agent-shell")
(declare-function agent-shell--make-button "agent-shell")
(declare-function agent-shell--make-status-kind-label "agent-shell")
(declare-function agent-shell--on-request "agent-shell")
(declare-function agent-shell--resolve-path "agent-shell")
(declare-function agent-shell--delete-fragment "agent-shell")
(declare-function agent-shell--make-permission-button "agent-shell")
(declare-function agent-shell--permission-block-id "agent-shell")
(declare-function agent-shell--refresh-permission-dialog "agent-shell")
(declare-function agent-shell--save-tool-call "agent-shell")
(declare-function agent-shell--send-permission-response "agent-shell")
(declare-function agent-shell--tool-call-update-details "agent-shell")
(declare-function agent-shell-diff-kill-buffer "agent-shell-diff")
(declare-function agent-shell-elicitation--render "agent-shell-elicitation")
(declare-function agent-shell-elicitation--respond "agent-shell-elicitation")
(declare-function agent-shell-elicitation--retire "agent-shell-elicitation")
(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--update-header-and-mode-line "agent-shell")

(defvar agent-shell--state)

(defvar agent-shell-subagent-sessions-enabled nil
  "When non-nil, advertise ACP sub-agent session support.

Agents then report each sub-agent as its own session, which
`agent-shell-open-subagent' opens.  When nil (the default), agents
keep reporting sub-agent activity inline in the main session.

Takes effect on new shells.")

(defvar-local agent-shell-subagent-count 0
  "Number of sub-agent sessions in the main shell's session tree.

Kept current in the main shell buffer (nested sub-agents included), for
use in mode lines, headers, and the like.  Always 0 elsewhere.

See also `agent-shell-subagent-running-count'.")

(defvar-local agent-shell-subagent-running-count 0
  "Number of sub-agent sessions currently working or awaiting input.

Kept current in the main shell buffer.  See `agent-shell-subagent-count'.")

(defvar-local agent-shell-subagent--message-anchors nil
  "Alist of message ids to markers, where their removed blocks were.

A message shown on its own, then taken over by a sub-agent's card, goes
back to its marker if the card lets go of it.  That keeps it with the
turn it arrived in.  See `agent-shell-subagent--render-message'.")

(defun agent-shell-subagent--add-client-capability (request)
  "Return initialize REQUEST advertising sub-agent session support.

REQUEST is returned unchanged unless
`agent-shell-subagent-sessions-enabled'.  For example:

  ((:method . \"initialize\")
   (:params (clientCapabilities (fs ...))))
  => ((:method . \"initialize\")
      (:params (clientCapabilities (fs ...) (subagents))))

where `(subagents)' is sent as \"subagents\": {}."
  (if (not agent-shell-subagent-sessions-enabled)
      request
    (agent-shell-subagent--alist-put
     request :params
     (agent-shell-subagent--alist-put
      (map-elt request :params) 'clientCapabilities
      (append (map-nested-elt request '(:params clientCapabilities))
              (list (cons 'subagents nil)))))))

;;; Registry

(cl-defun agent-shell-subagent--make-record (&key session-id parent-session-id)
  "Return a new sub-agent record for SESSION-ID owned by PARENT-SESSION-ID.

Records live in the main shell's state under `:subagents', keyed by
session id.  For example:

  (agent-shell-subagent--make-record :session-id \"c1\"
                                     :parent-session-id \"p1\")
  => ((:session-id . \"c1\") (:parent-session-id . \"p1\")
      (:title) (:description) (:state) ...)"
  (list (cons :session-id session-id)
        (cons :parent-session-id parent-session-id)
        (cons :title nil)
        (cons :description nil)
        (cons :state nil)
        (cons :stop-reason nil)
        (cons :error nil)
        (cons :historical nil)
        (cons :disconnected nil)
        (cons :can-cancel nil)
        (cons :cwd nil)
        (cons :task nil)
        (cons :result nil)
        (cons :tool-calls nil)
        (cons :notifications nil)
        (cons :buffer nil)
        (cons :created-at (current-time))))

(defun agent-shell-subagent--ensure-state (state)
  "Add the keys sub-agents need to STATE when a shell predates them.

Without this, `map-put!' fails on a mid-session package update."
  (dolist (key '(:subagents :subagent-messages))
    (unless (assq key state)
      (nconc state (list (cons key nil)))))
  state)

(defun agent-shell-subagent--alist-put (alist key value)
  "Return a copy of ALIST with KEY bound to VALUE.

Unlike `map-insert', any existing binding is replaced, rather than
shadowed by a new one.

  (agent-shell-subagent--alist-put \\='((a . 1) (b . 2)) \\='a 3)
  => ((a . 3) (b . 2))"
  (if (assoc key alist)
      (mapcar (lambda (pair)
                (if (equal (car pair) key)
                    (cons key value)
                  pair))
              alist)
    (append alist (list (cons key value)))))

(defun agent-shell-subagent--loading-p (state)
  "Return non-nil while main shell STATE's `session/load' is in flight.

As per spec, sub-agent state, capabilities, and configuration arriving
before the load's response are history: they can be shown, but don't
confirm a sub-agent is working, nor permit cancelling it."
  (seq-find (lambda (request)
              (equal (map-elt request :method) "session/load"))
            (map-elt state :active-requests)))

(defun agent-shell-subagent--restoring-p (state)
  "Return non-nil while main shell STATE's load or resume is in flight.

As per spec, nothing may be sent for a sub-agent until the restore
succeeds, even when (as with `session/resume') what it reports is
current.  A failed restore discards it all (see
`agent-shell-subagent--discard-restore')."
  (seq-find (lambda (request)
              (member (map-elt request :method) '("session/load" "session/resume")))
            (map-elt state :active-requests)))

(defun agent-shell-subagent--root-state (state)
  "Return the main shell's state for STATE.

STATE is returned as is unless it belongs to a sub-agent buffer, in
which case its main shell's state is returned instead."
  (if-let* ((root-buffer (map-elt state :subagent-root-buffer))
            ((buffer-live-p root-buffer)))
      (buffer-local-value 'agent-shell--state root-buffer)
    state))

(defun agent-shell-subagent--record (state session-id)
  "Return the sub-agent record for SESSION-ID in STATE's session tree."
  (when session-id
    (map-nested-elt (agent-shell-subagent--root-state state)
                    (list :subagents session-id))))

(defun agent-shell-subagent--records (state)
  "Return all sub-agent records in STATE's session tree, oldest first."
  (map-values (map-elt (agent-shell-subagent--root-state state) :subagents)))

(defun agent-shell-subagent-p (&optional buffer)
  "Return non-nil if BUFFER (or the current one) shows a sub-agent session."
  (when-let* ((buffer (or buffer (current-buffer)))
              ((buffer-live-p buffer))
              (state (buffer-local-value 'agent-shell--state buffer)))
    (and (map-elt state :subagent-root-buffer) t)))

(defun agent-shell-subagent--apply-update (record update &optional historical)
  "Patch RECORD in place with `subagent_update' UPDATE.

As per spec, fields absent from UPDATE are left unchanged, while a
present null (parsed as nil) clears them.  For example:

  record: ((:title . \"Old\") (:state . \"running\") ...)
  update: ((sessionId . \"c1\") (title . \"New\")
           (state (state . \"idle\") (stopReason . \"end_turn\")))
  => ((:title . \"New\") (:state . \"idle\") (:stop-reason . \"end_turn\") ...)

When HISTORICAL, UPDATE is replayed history: its state is kept for
display only (see `agent-shell-subagent--working-p'), and its
capabilities permit nothing."
  (dolist (field '((title . :title)
                   (description . :description)
                   (cwd . :cwd)))
    (when (assq (car field) update)
      (map-put! record (cdr field) (map-elt update (car field)))))
  ;; Note: `capabilities' is replaced as a whole.  As per spec, an
  ;; advertised `cancel' is {}, and a null one means no support.  The
  ;; ACP decoder parses both as nil for now, so the key's presence is
  ;; taken as support, unless the value is a null marker.
  (when (assq 'capabilities update)
    (map-put! record :can-cancel
              (and (not historical)
                   (when-let* ((cancel (assq 'cancel (map-elt update 'capabilities))))
                     (not (memq (cdr cancel) '(:null :json-null))))
                   t)))
  (when (assq 'state update)
    (map-put! record :state (map-nested-elt update '(state state)))
    (map-put! record :stop-reason (map-nested-elt update '(state stopReason)))
    (map-put! record :error (map-nested-elt update '(state error message)))
    (map-put! record :historical (and historical t))
    ;; Fresh state, so the connection is live (again).
    (map-put! record :disconnected nil))
  record)

(defun agent-shell-subagent--ancestor-p (state session-id parent-session-id)
  "Return non-nil if SESSION-ID is PARENT-SESSION-ID or one of its ancestors.

STATE is any state in the session tree."
  (let ((ancestor-id parent-session-id)
        (found nil))
    (while (and ancestor-id (not found))
      (if (equal ancestor-id session-id)
          (setq found t)
        (setq ancestor-id (map-elt (agent-shell-subagent--record state ancestor-id)
                                   :parent-session-id))))
    found))

(cl-defun agent-shell-subagent--register (&key state parent-session-id update)
  "Record `subagent_update' UPDATE sent on PARENT-SESSION-ID into STATE.

STATE is the main shell's state.  The first update for a session
announces it, later ones patch it.  An announcement that would make a
session its own ancestor is ignored, as the tree couldn't be walked."
  (when-let* ((session-id (map-elt update 'sessionId))
              ((or (agent-shell-subagent--record state session-id)
                   (not (agent-shell-subagent--ancestor-p
                         state session-id parent-session-id)))))
    (unless (agent-shell-subagent--record state session-id)
      (map-put! state :subagents
                (append (map-elt state :subagents)
                        (list (cons session-id
                                    (agent-shell-subagent--make-record
                                     :session-id session-id
                                     :parent-session-id parent-session-id))))))
    (agent-shell-subagent--watch-connection state)
    (agent-shell-subagent--apply-update
     (agent-shell-subagent--record state session-id)
     update
     (agent-shell-subagent--loading-p state))))

(defun agent-shell-subagent--watch-connection (state)
  "Have main shell STATE's sub-agents learn when its agent process ends.

Done once per process.  See `agent-shell-subagent--on-disconnect'."
  (when-let* ((process (map-nested-elt state '(:client :process)))
              ((processp process))
              ((not (process-get process 'agent-shell-subagent-watched))))
    (process-put process 'agent-shell-subagent-watched t)
    (add-function :after (process-sentinel process)
                  (lambda (process _event)
                    (unless (process-live-p process)
                      (agent-shell-subagent--on-disconnect state))))))

(defun agent-shell-subagent--on-disconnect (state)
  "Mark main shell STATE's sub-agents as disconnected.

As per spec, once the connection ends, a sub-agent's last working state
is no longer confirmed, and nothing can be sent for it, so it can't be
cancelled either.  Outcomes already reported (done, failed...) stay.
Its pending requests can't be answered any more, so they're withdrawn."
  (when (buffer-live-p (map-elt state :buffer))
    (agent-shell-subagent--retire-requests state)
    (let ((records (agent-shell-subagent--records state)))
      (dolist (record records)
        (map-put! record :disconnected t)
        (map-put! record :can-cancel nil))
      (agent-shell-subagent--sync-counts state)
      (dolist (record records)
        (agent-shell-subagent--refresh-card state record)
        (agent-shell-subagent--refresh-buffer state record)
        (agent-shell-subagent--emit-update :state state :record record)))))

(defun agent-shell-subagent--forget-all (state)
  "Forget every sub-agent in main shell STATE, killing their buffers."
  (agent-shell-subagent--ensure-state state)
  (let ((buffers (seq-filter #'buffer-live-p
                             (mapcar (lambda (record)
                                       (map-elt record :buffer))
                                     (agent-shell-subagent--records state)))))
    (map-put! state :subagents nil)
    (map-put! state :subagent-messages nil)
    (agent-shell-subagent--sync-counts state)
    (mapc #'kill-buffer buffers)))

(defun agent-shell-subagent--discard-restore (state)
  "Discard what main shell STATE learned of sub-agents in a failed restore.

As per spec, a failed `session/load' or `session/resume' leaves nothing
of the sub-agent state it reported.  Restoring happens as the shell
starts, so everything known came from that request.

Requests the sub-agents raised meanwhile are answered as cancelled, and
their cards (with Open buttons) removed, so nothing left on screen acts
on a sub-agent that's gone."
  (agent-shell-subagent--ensure-state state)
  (let* ((records (agent-shell-subagent--records state))
         (session-ids (mapcar (lambda (record)
                                (map-elt record :session-id))
                              records))
         ;; Described while still known, for subscribers.
         (descriptions (mapcar (lambda (record)
                                 (agent-shell-subagent--describe state record))
                               records)))
    (agent-shell-subagent--forget-all state)
    (when (buffer-live-p (map-elt state :buffer))
      (with-current-buffer (map-elt state :buffer)
        (dolist (session-id session-ids)
          (agent-shell--delete-fragment
           :state state
           :namespace-id "subagent"
           :block-id (agent-shell-subagent--card-block-id session-id)))))
    ;; Forgotten first, so settled forms no longer name (or link to)
    ;; their sub-agent.  A restore failing because the process ended
    ;; can't be answered, so its requests are withdrawn instead.
    (if (agent-shell-subagent--connected-p state)
        (dolist (session-id session-ids)
          (agent-shell-subagent--cancel-requests
           :state state :session-id session-id :reminder-scope 'request))
      (agent-shell-subagent--retire-requests state))
    ;; Forms answered earlier still link to their sub-agent.
    (map-do (lambda (id elicitation)
              (when (member (map-elt elicitation :subagent-session-id) session-ids)
                (agent-shell-elicitation--render :state state :id id)))
            (map-elt state :elicitations))
    (dolist (description descriptions)
      (agent-shell-subagent--emit-update
       :state state :description description :removed t))))

(defun agent-shell-subagent--connected-p (state)
  "Return non-nil unless main shell STATE's agent process is known to have ended."
  (let ((process (map-nested-elt state '(:client :process))))
    (or (not (processp process))
        (process-live-p process))))

(defun agent-shell-subagent--on-restore-success (state)
  "Tell main shell STATE's subscribers its restore succeeded.

Sub-agents reported during a `session/resume' couldn't be cancelled
until now (see `agent-shell-subagent--can-cancel-p')."
  (dolist (record (agent-shell-subagent--records state))
    (agent-shell-subagent--refresh-buffer state record)
    (agent-shell-subagent--emit-update :state state :record record)))

(defun agent-shell-subagent--content-text (content)
  "Return markdown for CONTENT, a content block or a list of them.

  (agent-shell-subagent--content-text
   \\='(((type . \"text\") (text . \"Look\")) ((type . \"text\") (text . \" here\"))))
  => \"Look here\""
  (if (and (consp content) (map-elt content 'type))
      (agent-shell--content-block-to-markdown content)
    (mapconcat #'agent-shell--content-block-to-markdown content "")))

(cl-defun agent-shell-subagent--record-message (&key state session-id update)
  "Record inter-session message UPDATE sent on SESSION-ID into STATE.

STATE is the main shell's state.  Handles both `session_message'
upserts and `session_message_chunk' appends.  Messages are keyed by
the pair of SESSION-ID and `messageId', since message ids are only
unique within a session's transcript.

The newest message from a known sub-agent's parent is its task, and
the newest from a known sub-agent to its parent its result (see
`agent-shell-subagent--summarize').  Any other message (between
siblings, say) is only kept as a message.

Return the records whose task or result changed."
  (let* ((key (cons session-id (map-elt update 'messageId)))
         (message (or (map-nested-elt state (list :subagent-messages key))
                      (list (cons :sender nil)
                            (cons :recipient nil)
                            (cons :sender-message-id nil)
                            (cons :recipient-message-id nil)
                            (cons :text "")))))
    (if (equal (map-elt update 'sessionUpdate) "session_message_chunk")
        (map-put! message :text (concat (map-elt message :text)
                                        (agent-shell-subagent--content-text
                                         (map-elt update 'content))))
      ;; As per spec, an omitted field is unchanged, and a null one cleared.
      (dolist (field '((senderSessionId . :sender)
                       (recipientSessionId . :recipient)
                       (senderMessageId . :sender-message-id)
                       (recipientMessageId . :recipient-message-id)))
        (when (assq (car field) update)
          (map-put! message (cdr field) (map-elt update (car field)))))
      (when (assq 'content update)
        (map-put! message :text (agent-shell-subagent--content-text
                                 (map-elt update 'content)))))
    (unless (map-nested-elt state (list :subagent-messages key))
      (map-put! state :subagent-messages
                (cons (cons key message) (map-elt state :subagent-messages))))
    ;; Every sub-agent, as a patch (a link, say) may change which
    ;; message is newest for a sub-agent it doesn't name.
    (let ((messages (agent-shell-subagent--merged-messages state)))
      (seq-filter (lambda (record)
                    (agent-shell-subagent--summarize record messages))
                  (agent-shell-subagent--records state)))))

(defun agent-shell-subagent--summarize (record messages)
  "Bring sub-agent RECORD's task and result up to date with MESSAGES.

MESSAGES are as returned by `agent-shell-subagent--merged-messages'.
The newest from RECORD's parent to RECORD is its task, and the newest
back its result.  Return non-nil if either changed."
  (let* ((session-id (map-elt record :session-id))
         (parent-session-id (map-elt record :parent-session-id))
         (task (map-elt (seq-find (lambda (message)
                                    (and (equal (map-elt message :sender) parent-session-id)
                                         (equal (map-elt message :recipient) session-id)))
                                  messages)
                        :text))
         (result (map-elt (seq-find (lambda (message)
                                      (and (equal (map-elt message :sender) session-id)
                                           (equal (map-elt message :recipient) parent-session-id)))
                                    messages)
                          :text))
         (changed (not (and (equal task (map-elt record :task))
                            (equal result (map-elt record :result))))))
    (map-put! record :task task)
    (map-put! record :result result)
    changed))

(defun agent-shell-subagent--linked-key (message)
  "Return the key of MESSAGE's other view, if MESSAGE names it.

Keyed as in `agent-shell-subagent--record-message'."
  (cond ((and (map-elt message :sender) (map-elt message :sender-message-id))
         (cons (map-elt message :sender) (map-elt message :sender-message-id)))
        ((and (map-elt message :recipient) (map-elt message :recipient-message-id))
         (cons (map-elt message :recipient) (map-elt message :recipient-message-id)))))

(defun agent-shell-subagent--merged-messages (state)
  "Return STATE's messages newest first, each pair of linked views as one.

A message's outgoing view (in its sender's transcript) and incoming
view (in its recipient's) are the same message.  As per spec, they're
linked only by an explicit `senderMessageId' or `recipientMessageId',
never by equal ids.  Either view may lack what the other reports, so
the merged message takes each participant from whichever view names
it, and its text from a view that has some (the sender's, if both do).
It's as old as its older view, so a late copy doesn't pass for a newer
message.

Each is an alist with :sender, :recipient and :text.  For example,
with A sent before B, and A's incoming view (A') reported last:

  views (newest first): A', B, A  => B, A (merged with A')"
  ;; Hash tables rather than alists, as this runs on every chunk recorded
  ;; and a scan per view makes long transcripts slow to stream or replay.
  (let ((views (map-elt state :subagent-messages))
        ;; Key => (POSITION . VIEW).
        (index (make-hash-table :test #'equal))
        (peers (make-hash-table :test #'equal))
        (merged nil))
    (seq-do-indexed (lambda (view position)
                      (puthash (car view) (cons position view) index))
                    views)
    (dolist (view views)
      (when-let* ((peer-key (agent-shell-subagent--linked-key (cdr view)))
                  ((gethash peer-key index)))
        (puthash (car view) peer-key peers)
        (puthash peer-key (car view) peers)))
    (dolist (view views)
      (let ((peer (when-let* ((peer-key (gethash (car view) peers)))
                    (gethash peer-key index))))
        ;; A pair is merged once, from its older view.
        (unless (and peer
                     (< (car (gethash (car view) index)) (car peer)))
          (let* ((pair (if peer (list view (cdr peer)) (list view)))
                 (sender (seq-some (lambda (view)
                                     (map-elt (cdr view) :sender))
                                   pair))
                 (with-text (seq-remove (lambda (view)
                                          (string-empty-p (map-elt (cdr view) :text)))
                                        pair)))
            (push (list (cons :sender sender)
                        (cons :recipient (seq-some (lambda (view)
                                                     (map-elt (cdr view) :recipient))
                                                   pair))
                        (cons :text (map-elt (cdr (or (seq-find (lambda (view)
                                                                  (equal (car (car view)) sender))
                                                                with-text)
                                                      (car with-text)
                                                      view))
                                             :text)))
                  merged)))))
    (nreverse merged)))

(defun agent-shell-subagent--peer-key (views key)
  "Return the key of the view linked to the one at KEY in VIEWS, if any.

VIEWS are as kept in STATE's :subagent-messages.  The link may be named
by either view (see `agent-shell-subagent--linked-key')."
  (let ((linked (agent-shell-subagent--linked-key (map-elt views key))))
    (if (and linked (assoc linked views))
        linked
      (car (seq-find (lambda (view)
                       (equal (agent-shell-subagent--linked-key (cdr view)) key))
                     views)))))

(defun agent-shell-subagent--message (state session-id message-id)
  "Return the recorded message MESSAGE-ID sent on SESSION-ID in STATE's tree.

A participant this view omits is taken from its linked view, if any,
as both are the same message."
  (let* ((views (map-elt (agent-shell-subagent--root-state state) :subagent-messages))
         (key (cons session-id message-id))
         (message (map-elt views key))
         (peer (when message
                 (map-elt views (agent-shell-subagent--peer-key views key)))))
    (if peer
        (append (list (cons :sender (or (map-elt message :sender)
                                        (map-elt peer :sender)))
                      (cons :recipient (or (map-elt message :recipient)
                                           (map-elt peer :recipient))))
                message)
      message)))

(defun agent-shell-subagent--render-view (state key)
  "Render again the message view at KEY in its session's buffer, if open.

STATE is any state in the session tree.  KEY is as in
`agent-shell-subagent--record-message'.  For a view whose linked view
changed: it may now take a participant from it, and so belong in a
sub-agent's card, or no longer, and so belong on its own."
  (when-let* ((message (agent-shell-subagent--message state (car key) (cdr key)))
              (buffer (agent-shell-subagent--session-buffer state (car key))))
    (with-current-buffer buffer
      (agent-shell-subagent--render-message
       :state agent-shell--state
       :message-id (cdr key)
       :message message))))

(defun agent-shell-subagent--working-p (record)
  "Return non-nil if RECORD's sub-agent is working or awaiting input.

A state replayed from history, or reported before the connection
ended, doesn't count, as it isn't current."
  (and (not (map-elt record :historical))
       (not (map-elt record :disconnected))
       (member (map-elt record :state) '("running" "requires_action"))))

(defun agent-shell-subagent--can-cancel-p (state record)
  "Return non-nil if RECORD's sub-agent can be cancelled right now.

That takes an advertised `cancel' capability, a live connection, and
no restore in flight (see `agent-shell-subagent--restoring-p').  STATE
is any state in the session tree."
  (and (map-elt record :can-cancel)
       (not (map-elt record :disconnected))
       (not (agent-shell-subagent--restoring-p
             (agent-shell-subagent--root-state state)))))

(defun agent-shell-subagent--sync-counts (state)
  "Sync count variables, header, and subscribers with main shell STATE."
  (when-let* ((buffer (map-elt state :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq-local agent-shell-subagent-count
                  (length (map-elt state :subagents)))
      (setq-local agent-shell-subagent-running-count
                  (seq-count #'agent-shell-subagent--working-p
                             (agent-shell-subagent--records state)))
      (when (derived-mode-p 'agent-shell-mode)
        (agent-shell--update-header-and-mode-line))
      (force-mode-line-update))))

(defun agent-shell-subagent--pending-request-count (state session-id)
  "Return how many of sub-agent SESSION-ID's requests await an answer.

STATE is any state in the session tree.  Counts the permission requests
and forms the main shell holds for SESSION-ID."
  (let ((root-state (agent-shell-subagent--root-state state)))
    (+ (seq-count (lambda (pair)
                    (and (equal (map-elt (cdr pair) :subagent-session-id) session-id)
                         (map-elt (cdr pair) :permission-request-id)))
                  (map-elt root-state :tool-calls))
       (seq-count (lambda (pair)
                    (and (equal (map-elt (cdr pair) :subagent-session-id) session-id)
                         (eq (map-elt (cdr pair) :status) 'pending)))
                  (map-elt root-state :elicitations)))))

(defun agent-shell-subagent--describe (state record)
  "Return sub-agent RECORD of STATE's session tree, as seen by callers.

See `agent-shell-subagents' for its fields."
  (let ((root-state (agent-shell-subagent--root-state state)))
    (list (cons :session-id (map-elt record :session-id))
          (cons :parent-session-id (map-elt record :parent-session-id))
          (cons :title (agent-shell-subagent--title record))
          (cons :description (map-elt record :description))
          (cons :status (agent-shell-subagent--status-text record))
          (cons :state (map-elt record :state))
          (cons :stop-reason (map-elt record :stop-reason))
          (cons :error (map-elt record :error))
          (cons :can-cancel (agent-shell-subagent--can-cancel-p root-state record))
          (cons :depth (agent-shell-subagent--depth root-state record))
          (cons :pending-requests (agent-shell-subagent--pending-request-count
                                   root-state (map-elt record :session-id)))
          (cons :cwd (map-elt record :cwd))
          (cons :task (map-elt record :task))
          (cons :result (map-elt record :result))
          (cons :buffer (when (buffer-live-p (map-elt record :buffer))
                          (map-elt record :buffer)))
          (cons :shell-buffer (map-elt root-state :buffer)))))

(cl-defun agent-shell-subagent--emit-update (&key state record meta description removed)
  "Emit a `subagent-update' event for RECORD from main shell STATE.

META is the update's `_meta', if any.  DESCRIPTION, if given, describes
the sub-agent instead of RECORD, and REMOVED is non-nil if the
sub-agent is gone (see `agent-shell-subagent--discard-restore')."
  (when-let* ((root-state (agent-shell-subagent--root-state state))
              (buffer (map-elt root-state :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (agent-shell--emit-event
       :event 'subagent-update
       :data (list (cons :subagent (or description
                                       (agent-shell-subagent--describe root-state record)))
                   (cons :count agent-shell-subagent-count)
                   (cons :running-count agent-shell-subagent-running-count)
                   (cons :removed (and removed t))
                   (cons :meta meta))))))

(defun agent-shell-subagent--tree-records (state)
  "Return STATE's sub-agent records, each parent before its children.

Siblings stay oldest first.  Records whose parent is unknown come
last.  For example, with c2 nested in c1:

  c1, c3, c2 (as registered)  => c1, c2, c3"
  (let* ((records (agent-shell-subagent--records state))
         (root-session-id (map-nested-elt (agent-shell-subagent--root-state state)
                                          '(:session :id)))
         (top-level (seq-filter (lambda (record)
                                  (and root-session-id
                                       (equal (map-elt record :parent-session-id)
                                              root-session-id)))
                                records)))
    (nreverse (seq-reduce (lambda (ordered record)
                            (agent-shell-subagent--visit-record records record ordered))
                          (append top-level records)
                          nil))))

(defun agent-shell-subagent--visit-record (records record ordered)
  "Return ORDERED with RECORD, then its descendants in RECORDS, pushed on.

ORDERED is newest first.  A record already in it is skipped, so each
is pushed once.  For example, with c2 nested in c1:

  RECORD c1, ORDERED (c0)  => (c2 c1 c0)"
  (if (memq record ordered)
      ordered
    (seq-reduce (lambda (ordered child)
                  (if (equal (map-elt child :parent-session-id)
                             (map-elt record :session-id))
                      (agent-shell-subagent--visit-record records child ordered)
                    ordered))
                records
                (cons record ordered))))

;;; Routing

(cl-defun agent-shell-subagent--route-notification (&key state acp-notification)
  "Route ACP-NOTIFICATION, received by main shell STATE, to its session.

Sub-agent bookkeeping (announcements, state, messages) is recorded
first, whichever session it arrives on.

Return non-nil if ACP-NOTIFICATION belongs to a sub-agent session, in
which case it is kept in memory (and rendered in the sub-agent's buffer
if open) and must not be rendered in the main shell."
  (when (equal (map-elt acp-notification 'method) "session/update")
    (agent-shell-subagent--ensure-state state)
    (let ((session-id (map-nested-elt acp-notification '(params sessionId)))
          (update (map-nested-elt acp-notification '(params update))))
      (pcase (map-elt update 'sessionUpdate)
        ("subagent_update"
         (when-let* ((record (agent-shell-subagent--register
                              :state state
                              :parent-session-id session-id
                              :update update)))
           (agent-shell-subagent--sync-counts state)
           (agent-shell-subagent--refresh-buffer state record)
           ;; Sub-agents without a `cwd' of their own inherit this one.
           (when (assq 'cwd update)
             (dolist (descendant (agent-shell-subagent--descendants state record))
               (agent-shell-subagent--refresh-buffer state descendant)))
           ;; As per spec, a sub-agent reported as cancelled has its
           ;; requests treated as cancelled too.  Any other stop leaves
           ;; them answerable: unrelated work may still need them.
           (when (and (assq 'state update)
                      (equal (map-elt record :state) "idle")
                      (equal (map-elt record :stop-reason) "cancelled")
                      (not (map-elt record :historical)))
             (agent-shell-subagent--cancel-requests
              :state state :session-id (map-elt record :session-id)
              :reminder-scope 'request))
           (agent-shell-subagent--emit-update
            :state state :record record :meta (map-elt update '_meta))))
        ((or "session_message" "session_message_chunk")
         (let* ((key (cons session-id (map-elt update 'messageId)))
                ;; Chunks only add text, which a linked view doesn't
                ;; show.  A patch may clear or redirect the link, so
                ;; the view linked before it counts too.
                (peer-keys (when (equal (map-elt update 'sessionUpdate) "session_message")
                             (list (agent-shell-subagent--peer-key
                                    (map-elt state :subagent-messages) key)))))
           (dolist (record (agent-shell-subagent--record-message
                            :state state :session-id session-id :update update))
             ;; Even when the parent renders the message, as it may no
             ;; longer name this sub-agent.
             (agent-shell-subagent--refresh-card state record)
             (agent-shell-subagent--emit-update :state state :record record))
           (when peer-keys
             (push (agent-shell-subagent--peer-key
                    (map-elt state :subagent-messages) key)
                   peer-keys))
           (dolist (peer-key (seq-uniq (delq nil peer-keys)))
             (unless (equal peer-key key)
               (agent-shell-subagent--render-view state peer-key))))))
      (when-let* ((record (agent-shell-subagent--record state session-id)))
        (agent-shell-subagent--remember-tool-call record update)
        (agent-shell-subagent--update-permission
         :state state :session-id session-id :update update)
        (agent-shell-subagent--settle-tool-call
         :state state :session-id session-id :update update)
        ;; As per spec, configuration replayed by `session/load' is
        ;; history, never the sub-agent's current one, so it's dropped.
        ;; The agent reports the current one after the load.
        (unless (and (equal (map-elt update 'sessionUpdate) "config_option_update")
                     (agent-shell-subagent--loading-p state))
          (map-put! record :notifications
                    (cons acp-notification (map-elt record :notifications)))
          (when-let* ((buffer (map-elt record :buffer))
                      ((buffer-live-p buffer)))
            (with-current-buffer buffer
              (agent-shell--dispatch-notification
               :state agent-shell--state
               :acp-notification acp-notification))))
        t))))

(defun agent-shell-subagent--descendants (state record)
  "Return the records nested under RECORD in STATE's session tree."
  (seq-filter (lambda (other)
                (and (not (eq other record))
                     (agent-shell-subagent--ancestor-p
                      state (map-elt record :session-id)
                      (map-elt other :parent-session-id))))
              (agent-shell-subagent--records state)))

(defun agent-shell-subagent--tool-call-id (session-id tool-call-id)
  "Return sub-agent SESSION-ID's TOOL-CALL-ID as keyed in the main shell.

Tool call ids are opaque and only unique within a session, so the key
is the pair of both, which can't match any of the main shell's own.

  (agent-shell-subagent--tool-call-id \"c1\" \"t1\") => (\"c1\" . \"t1\")"
  (cons session-id tool-call-id))

(defconst agent-shell-subagent--permission-namespace-id "subagent-permission"
  "Fragment namespace of sub-agent permission dialogs in the main shell.

Fixed, since a sub-agent's request can outlive the main shell's turn.")

(defun agent-shell-subagent--remember-tool-call (record update)
  "Keep sub-agent RECORD's tool call details from UPDATE, if it has any.

A sub-agent may request permission naming only a tool call it reported
earlier, so its details are kept here (whether or not its buffer was
ever opened) until the call completes or fails."
  (when-let* (((member (map-elt update 'sessionUpdate) '("tool_call" "tool_call_update")))
              (tool-call-id (map-elt update 'toolCallId)))
    (if (member (map-elt update 'status) '("completed" "failed"))
        (map-put! record :tool-calls
                  (seq-remove (lambda (pair)
                                (equal (car pair) tool-call-id))
                              (map-elt record :tool-calls)))
      (map-put! record :tool-calls
                (agent-shell-subagent--alist-put
                 (map-elt record :tool-calls) tool-call-id
                 (map-merge 'alist
                            (map-nested-elt record (list :tool-calls tool-call-id))
                            (seq-filter (lambda (pair)
                                          (and (cdr pair)
                                               (not (eq (car pair) 'sessionUpdate))))
                                        update)))))))

(cl-defun agent-shell-subagent--update-permission (&key state session-id update)
  "Bring main shell STATE's permission dialog up to date with UPDATE.

UPDATE is sub-agent SESSION-ID's.  A sub-agent may report what a tool
call does (its command, diffs...) only after asking permission for it,
so the dialog is redrawn to show it."
  (when-let* (((member (map-elt update 'sessionUpdate) '("tool_call" "tool_call_update")))
              ((not (member (map-elt update 'status) '("completed" "failed"))))
              (tool-call-id (agent-shell-subagent--tool-call-id
                             session-id (map-elt update 'toolCallId)))
              ((map-nested-elt state (list :tool-calls tool-call-id :permission-request-id)))
              (details (agent-shell--tool-call-update-details update)))
    (agent-shell--save-tool-call state tool-call-id details)
    (with-current-buffer (map-elt state :buffer)
      (agent-shell--refresh-permission-dialog state tool-call-id))))

(defun agent-shell-subagent--complete-tool-call (record tool-call tool-call-id)
  "Return TOOL-CALL, from sub-agent RECORD's request, keyed as TOOL-CALL-ID.

Details the sub-agent reported earlier fill in whatever TOOL-CALL
leaves out (its title, command, diffs...).  For example:

  remembered: ((title . \"Run build\") (rawInput (command . \"make\")))
  tool-call:  ((toolCallId . \"t1\"))
  => ((title . \"Run build\") (rawInput (command . \"make\"))
      (toolCallId . (\"c1\" . \"t1\")))"
  (agent-shell-subagent--alist-put
   (map-merge 'alist
              (map-nested-elt record (list :tool-calls (map-elt tool-call 'toolCallId)))
              (seq-filter #'cdr tool-call))
   'toolCallId tool-call-id))

(defun agent-shell-subagent--request-session-id (state acp-request)
  "Return the sub-agent session id ACP-REQUEST came from, if any.

STATE is any state in the session tree."
  (when-let* ((session-id (map-nested-elt acp-request '(params sessionId)))
              ((agent-shell-subagent--record state session-id)))
    session-id))

(cl-defun agent-shell-subagent--route-request (&key state acp-request)
  "Route ACP-REQUEST, received by main shell STATE, if from a sub-agent.

Sub-agent requests (permissions and forms especially) are shown and
answered in the main shell, so they are never missed, and opening or
killing a sub-agent's buffer doesn't affect them.  Each is tagged with
its sub-agent's `:subagent-session-id', which links it to the sub-agent's
conversation.

Return non-nil if ACP-REQUEST came from a sub-agent and was handled."
  (when-let* ((session-id (agent-shell-subagent--request-session-id state acp-request))
              (record (agent-shell-subagent--record state session-id)))
    (with-current-buffer (map-elt state :buffer)
      (if (equal (map-elt acp-request 'method) "session/request_permission")
          (let* ((tool-call (map-nested-elt acp-request '(params toolCall)))
                 (tool-call-id (agent-shell-subagent--tool-call-id
                                session-id (map-elt tool-call 'toolCallId))))
            ;; Pending until the sub-agent's tool call settles, so the main
            ;; shell's end of turn doesn't drop it.
            (agent-shell--save-tool-call
             state tool-call-id
             (list (cons :subagent-session-id session-id)
                   (cons :status "pending")
                   (cons :permission-namespace-id
                         agent-shell-subagent--permission-namespace-id)))
            (agent-shell--on-request
             :state state
             :acp-request (agent-shell-subagent--alist-put
                           acp-request 'params
                           (agent-shell-subagent--alist-put
                            (map-elt acp-request 'params) 'toolCall
                            (agent-shell-subagent--complete-tool-call
                             record tool-call tool-call-id)))))
        ;; Forms tag themselves (see `agent-shell-elicitation--on-create-request').
        (agent-shell--on-request :state state :acp-request acp-request)))
    t))

(cl-defun agent-shell-subagent--settle-tool-call (&key state session-id update)
  "Settle main shell STATE's permission for SESSION-ID's tool call UPDATE.

SESSION-ID is a sub-agent's.

Once the sub-agent's tool call completes or fails, requests raised for
it no longer apply.  Its permission request is dismissed (answered as
cancelled if still pending, as per spec) and its entry dropped.  A form
raised for it, if still pending, is answered as cancelled too."
  (when-let* (((equal (map-elt update 'sessionUpdate) "tool_call_update"))
              ((member (map-elt update 'status) '("completed" "failed")))
              (tool-call-id (agent-shell-subagent--tool-call-id
                             session-id (map-elt update 'toolCallId))))
    (when (map-nested-elt state (list :tool-calls tool-call-id))
      (agent-shell-subagent--dismiss-tool-call
       :state state :tool-call-id tool-call-id :reminder-scope 'request))
    ;; Copied, since responding rewrites them.
    (map-do (lambda (id elicitation)
              (when (equal (map-elt elicitation :tool-call-id) tool-call-id)
                (agent-shell-elicitation--respond
                 :state state :id id :action 'cancel :reminder-scope 'request)))
            (copy-sequence (map-elt state :elicitations)))))

(cl-defun agent-shell-subagent--dismiss-tool-call (&key state tool-call-id unanswered
                                                         reminder-scope)
  "Dismiss TOOL-CALL-ID's permission in main shell STATE and drop its entry.

A request still pending is answered as cancelled first, so the agent
gets exactly one answer, unless UNANSWERED (when nothing can answer it).
Either way, its dialog, diff viewer, and reminder go too.  An answer
passes REMINDER-SCOPE on (see `agent-shell--send-permission-response'),
and a withdrawal only takes the request's own reminder.  Return non-nil
if it was pending."
  (let ((request-id (map-nested-elt state (list :tool-calls tool-call-id
                                                :permission-request-id))))
    (if (and request-id (not unanswered))
        (agent-shell--send-permission-response
         :client (map-elt state :client)
         :request-id request-id
         :cancelled t
         :state state
         :tool-call-id tool-call-id
         :reminder-scope reminder-scope)
      (when-let* ((diff-buffer (map-nested-elt state (list :tool-calls tool-call-id
                                                           :diff-buffer))))
        (agent-shell-diff-kill-buffer diff-buffer))
      (with-current-buffer (map-elt state :buffer)
        (agent-shell--cancel-idle-timer-for request-id)
        (agent-shell--delete-fragment
         :state state
         :namespace-id agent-shell-subagent--permission-namespace-id
         :block-id (agent-shell--permission-block-id tool-call-id))))
    (map-put! state :tool-calls
              (seq-remove (lambda (pair)
                            (equal (car pair) tool-call-id))
                          (map-elt state :tool-calls)))
    (and request-id t)))

(cl-defun agent-shell-subagent--cancel-requests (&key state session-id reminder-scope)
  "Answer sub-agent SESSION-ID's pending requests as cancelled.

STATE is any state in the session tree.  The requests are held by the
main shell (see `agent-shell-subagent--route-request').  Its entries
for SESSION-ID are dropped too, since nothing is waiting on them.
REMINDER-SCOPE is `request' unless the user asked for this (see
`agent-shell--send-permission-response').

Return non-nil if any request was pending."
  (let ((root-state (agent-shell-subagent--root-state state))
        (cancelled nil))
    ;; Copied, since responding rewrites them.
    (map-do (lambda (tool-call-id tool-call)
              (when (and (equal (map-elt tool-call :subagent-session-id) session-id)
                         (agent-shell-subagent--dismiss-tool-call
                          :state root-state :tool-call-id tool-call-id
                          :reminder-scope reminder-scope))
                (setq cancelled t)))
            (copy-sequence (map-elt root-state :tool-calls)))
    (map-do (lambda (id elicitation)
              (when (and (equal (map-elt elicitation :subagent-session-id) session-id)
                         (agent-shell-elicitation--respond
                          :state root-state :id id :action 'cancel
                          :reminder-scope reminder-scope))
                (setq cancelled t)))
            (copy-sequence (map-elt root-state :elicitations)))
    cancelled))

(defun agent-shell-subagent--retire-requests (state)
  "Withdraw every sub-agent request main shell STATE holds, unanswered.

For when nothing can answer them any more, as once the connection ends."
  (map-do (lambda (tool-call-id tool-call)
            (when (map-elt tool-call :subagent-session-id)
              (agent-shell-subagent--dismiss-tool-call
               :state state :tool-call-id tool-call-id :unanswered t)))
          (copy-sequence (map-elt state :tool-calls)))
  (map-do (lambda (id elicitation)
            (when (map-elt elicitation :subagent-session-id)
              (agent-shell-elicitation--retire :state state :id id)))
          (copy-sequence (map-elt state :elicitations))))

(defun agent-shell-subagent--session-buffer (state session-id)
  "Return the live buffer of session SESSION-ID in STATE's tree, if open."
  (let* ((root-state (agent-shell-subagent--root-state state))
         (buffer (if (equal session-id (map-nested-elt root-state '(:session :id)))
                     (map-elt root-state :buffer)
                   (map-elt (agent-shell-subagent--record root-state session-id)
                            :buffer))))
    (when (buffer-live-p buffer)
      buffer)))

(defun agent-shell-subagent--refresh-card (state record)
  "Refresh sub-agent RECORD's card in its parent's buffer, if open.

STATE is any state in the session tree."
  (when-let* ((parent-buffer (agent-shell-subagent--session-buffer
                              state (map-elt record :parent-session-id))))
    (with-current-buffer parent-buffer
      (agent-shell-subagent--render-card
       :state agent-shell--state
       :session-id (map-elt record :session-id)))))

(defun agent-shell-subagent--refresh-buffer (state record)
  "Bring sub-agent RECORD's open buffer up to date with RECORD.

STATE is any state in the session tree."
  (when-let* ((buffer (map-elt record :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq default-directory
            (file-name-as-directory (agent-shell-subagent--cwd state record)))
      (agent-shell-subagent--render-header agent-shell--state record)
      (agent-shell--update-header-and-mode-line))))

;;; Rendering

(defun agent-shell-subagent--title (record)
  "Return RECORD's display title.

Falls back to its description's first line, then to its session id."
  (or (when-let* ((title (map-elt record :title))
                  ((not (string-empty-p (string-trim title)))))
        (string-trim title))
      (when-let* ((description (map-elt record :description))
                  ((not (string-empty-p (string-trim description)))))
        (car (split-string (string-trim description) "\n")))
      (format "Sub-agent %s" (map-elt record :session-id))))

(defun agent-shell-subagent--status-text (record)
  "Return RECORD's work state as text.

  ((:state . \"idle\") (:stop-reason . \"end_turn\"))  => \"done\"
  ((:state . \"idle\") (:stop-reason . \"error\"))     => \"failed\"
  ((:state . \"requires_action\"))                   => \"needs input\"

Activity replayed from history isn't current, so reads \"unconfirmed\".
Once the connection ends, it reads \"disconnected\"."
  (pcase (map-elt record :state)
    ('nil (if (map-elt record :disconnected) "disconnected" "started"))
    ((and (or "running" "requires_action")
          (guard (map-elt record :disconnected)))
     "disconnected")
    ((and (or "running" "requires_action")
          (guard (map-elt record :historical)))
     "unconfirmed")
    ("running" "running")
    ("requires_action" "needs input")
    ("unknown" "unknown")
    ("idle" (pcase (map-elt record :stop-reason)
              ((or 'nil "end_turn") "done")
              ("cancelled" "cancelled")
              ("error" "failed")
              (reason (replace-regexp-in-string "_" " " reason))))
    (state (replace-regexp-in-string "_" " " (string-remove-prefix "_" state)))))

(defun agent-shell-subagent--tool-call-status (record)
  "Return the tool call status whose look best fits RECORD's work state."
  (pcase (and (not (map-elt record :historical))
              (not (and (map-elt record :disconnected)
                        (not (equal (map-elt record :state) "idle"))))
              (map-elt record :state))
    ('nil "pending")
    ("running" "in_progress")
    ("requires_action" "pending")
    ("idle" (if (member (map-elt record :stop-reason) '("error" "cancelled"))
                "failed"
              "completed"))
    (_ "pending")))

(defun agent-shell-subagent--card-block-id (session-id)
  "Return the card block id for sub-agent SESSION-ID."
  (concat "card-" session-id))

(defun agent-shell-subagent--open-button (session-id root-buffer &optional keymap)
  "Return a button opening sub-agent SESSION-ID of ROOT-BUFFER.

KEYMAP, if any, is the button's parent keymap."
  (agent-shell--make-button
   :text "Open"
   :help "Open sub-agent conversation"
   :kind 'subagent
   :keymap keymap
   :action (lambda ()
             (interactive)
             (agent-shell-subagent--open
              :root-buffer root-buffer :session-id session-id))))

(defun agent-shell-subagent--card-body (record root-buffer)
  "Return the card body for sub-agent RECORD of ROOT-BUFFER."
  (string-join
   (delq nil
         (list (concat (agent-shell-subagent--open-button
                        (map-elt record :session-id) root-buffer)
                       " "
                       (propertize (agent-shell-subagent--status-text record)
                                   'font-lock-face 'agent-shell-secondary))
               (when-let* ((error-message (map-elt record :error)))
                 (propertize error-message 'font-lock-face 'agent-shell-error))
               (map-elt record :description)
               (when-let* ((task (map-elt record :task))
                           ((not (string-empty-p task))))
                 (concat (propertize "Task" 'font-lock-face 'agent-shell-section-heading)
                         "\n\n" task))
               (when-let* ((result (map-elt record :result))
                           ((not (string-empty-p result))))
                 (concat (propertize "Result" 'font-lock-face 'agent-shell-section-heading)
                         "\n\n" result))))
   "\n\n"))

(cl-defun agent-shell-subagent--permission-open-command (&key state tool-call)
  "Return a command opening TOOL-CALL's sub-agent, or nil if it has none.

STATE is the main shell's, which shows TOOL-CALL's permission dialog."
  (when-let* ((session-id (map-elt tool-call :subagent-session-id))
              (root-buffer (map-elt state :buffer)))
    (lambda ()
      (interactive)
      (agent-shell-subagent--open :root-buffer root-buffer :session-id session-id))))

(cl-defun agent-shell-subagent--permission-text (&key state tool-call keymap)
  "Return the line naming TOOL-CALL's sub-agent in its permission dialog.

Return nil if TOOL-CALL isn't a sub-agent's.  STATE is the main shell's.
KEYMAP is the dialog's.  For example:

  From sub-agent: Test investigator [ Open (o) ]"
  (when-let* ((open (agent-shell-subagent--permission-open-command
                     :state state :tool-call tool-call))
              (record (agent-shell-subagent--record
                       state (map-elt tool-call :subagent-session-id))))
    (concat (propertize "From sub-agent: " 'font-lock-face 'agent-shell-secondary)
            (agent-shell-subagent--title record)
            " "
            (agent-shell--make-permission-button
             :text "Open (o)"
             :help "Open the sub-agent's conversation"
             :action open
             :keymap keymap))))

(cl-defun agent-shell-subagent--source-text (&key state session-id)
  "Return the line naming sub-agent SESSION-ID as a request's source.

Return nil if SESSION-ID is nil or unknown.  STATE is any state in the
session tree.  For example:

  From sub-agent: Test investigator [ Open ]"
  (when-let* ((record (agent-shell-subagent--record state session-id))
              (root-buffer (map-elt (agent-shell-subagent--root-state state) :buffer)))
    (concat (propertize "From sub-agent: " 'font-lock-face 'agent-shell-secondary)
            (agent-shell-subagent--title record)
            " "
            (agent-shell-subagent--open-button
             session-id root-buffer
             ;; Same key as on the form's controls.
             (agent-shell-subagent--open-key-map session-id root-buffer)))))

(defun agent-shell-subagent--open-key-map (session-id root-buffer)
  "Return a keymap where \\`o' opens sub-agent SESSION-ID of ROOT-BUFFER."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o")
                (lambda ()
                  (interactive)
                  (agent-shell-subagent--open
                   :root-buffer root-buffer :session-id session-id)))
    map))

(defun agent-shell-subagent--session-reference-text (state session-id)
  "Return text naming the session SESSION-ID a tool call refers to.

STATE is any state in the session tree.  A known sub-agent's title
opens its conversation.  For example:

  \"Sub-agent: Test investigator\""
  (if-let* ((record (agent-shell-subagent--record state session-id))
            (root-buffer (map-elt (agent-shell-subagent--root-state state) :buffer)))
      (concat "Sub-agent: "
              (agent-shell--make-button
               :text (agent-shell-subagent--title record)
               :help "Open sub-agent conversation"
               :kind 'subagent
               :boxed nil
               :properties (list 'font-lock-face 'agent-shell-link)
               :action (lambda ()
                         (interactive)
                         (agent-shell-subagent--open
                          :root-buffer root-buffer :session-id session-id))))
    (format "Session: %s" session-id)))

(cl-defun agent-shell-subagent--render-card (&key state session-id)
  "Render or refresh sub-agent SESSION-ID's card in STATE's buffer."
  (when-let* ((record (agent-shell-subagent--record state session-id)))
    (agent-shell--update-fragment
     :state state
     ;; Fixed namespace, so updates arriving in later turns refresh the
     ;; card wherever it first landed.
     :namespace-id "subagent"
     :block-id (agent-shell-subagent--card-block-id session-id)
     :label-left (agent-shell--make-status-kind-label
                  :status (agent-shell-subagent--tool-call-status record)
                  :kind "subagent")
     :label-right (agent-shell-subagent--title record)
     :body (agent-shell-subagent--card-body
            record (map-elt (agent-shell-subagent--root-state state) :buffer))
     :above-last-prompt (not (agent-shell--active-requests-p state)))))

(cl-defun agent-shell-subagent--render-message (&key state message-id message)
  "Render inter-session MESSAGE with MESSAGE-ID in STATE's buffer.

A message between STATE's session and one of its own sub-agents (its
task or result) shows in that sub-agent's card instead.  Until both
ends are known (from either view of it), it shows on its own, and it
moves to the card once they are."
  (let* ((own-session-id (map-nested-elt state '(:session :id)))
         (child (seq-find (lambda (record)
                            (and (equal (map-elt record :parent-session-id) own-session-id)
                                 (member (list (map-elt message :sender)
                                               (map-elt message :recipient))
                                         (list (list own-session-id (map-elt record :session-id))
                                               (list (map-elt record :session-id) own-session-id)))))
                          (agent-shell-subagent--records state))))
    (with-current-buffer (map-elt state :buffer)
      (if child
          (progn
            (when-let* ((position (agent-shell--delete-fragment
                                   :state state
                                   :namespace-id "subagent-message"
                                   :block-id message-id)))
              (setq agent-shell-subagent--message-anchors
                    (map-insert agent-shell-subagent--message-anchors
                                message-id (copy-marker position))))
            (agent-shell-subagent--render-card
             :state state :session-id (map-elt child :session-id)))
        (let ((anchor (map-elt agent-shell-subagent--message-anchors message-id)))
          (setq agent-shell-subagent--message-anchors
                (map-delete agent-shell-subagent--message-anchors message-id))
          (agent-shell-subagent--render-standalone-message
           :state state :message-id message-id :message message :position anchor)
          (when anchor
            (set-marker anchor nil)))))))

(cl-defun agent-shell-subagent--render-standalone-message (&key state message-id message position)
  "Render inter-session MESSAGE with MESSAGE-ID on its own in STATE's buffer.

A new block goes at POSITION if given, or else at the end."
  (let ((own-session-id (map-nested-elt state '(:session :id)))
        ;; The main session has no parent, so a missing participant
        ;; never matches it.
        (parent-session-id (map-elt state :subagent-parent-session-id)))
    (agent-shell--update-fragment
     :state state
     :namespace-id "subagent-message"
     :block-id message-id
     :position position
     :label-left (propertize
                  (cond ((and parent-session-id
                              (equal (map-elt message :recipient) own-session-id)
                              (equal (map-elt message :sender) parent-session-id))
                         "Task from parent")
                        ((equal (map-elt message :recipient) own-session-id)
                         "Message received")
                        ((and parent-session-id
                              (equal (map-elt message :sender) own-session-id)
                              (equal (map-elt message :recipient) parent-session-id))
                         "Reported to parent")
                        ((equal (map-elt message :sender) own-session-id)
                         "Message sent")
                        (t
                         "Session message"))
                  'font-lock-face 'agent-shell-section-heading)
     :body (map-elt message :text)
     :expanded t
     :above-last-prompt (not (agent-shell--active-requests-p state)))))

(cl-defun agent-shell-subagent--on-notification (&key state acp-notification)
  "Render sub-agent related ACP-NOTIFICATION in STATE's buffer.

Bookkeeping already happened in `agent-shell-subagent--route-notification',
so this only renders: the card of an announced sub-agent, or an
inter-session message."
  (let ((update (map-nested-elt acp-notification '(params update))))
    (pcase (map-elt update 'sessionUpdate)
      ("subagent_update"
       (agent-shell-subagent--render-card
        :state state :session-id (map-elt update 'sessionId)))
      ((or "session_message" "session_message_chunk")
       (when-let* ((message (agent-shell-subagent--message
                             state
                             (map-nested-elt acp-notification '(params sessionId))
                             (map-elt update 'messageId))))
         (agent-shell-subagent--render-message
          :state state
          :message-id (map-elt update 'messageId)
          :message message))))))

;;; Buffers

(defun agent-shell-subagent--cwd (state record)
  "Return the effective working directory of sub-agent RECORD in STATE's tree.

As per spec, a sub-agent without its own `cwd' inherits its parent's."
  (let ((cwd (map-elt record :cwd))
        (parent record))
    (while (and (not cwd)
                (setq parent (agent-shell-subagent--record
                              state (map-elt parent :parent-session-id))))
      (setq cwd (map-elt parent :cwd)))
    ;; The agent's path may need mapping onto the local file system,
    ;; the way the main shell's own paths are.
    (with-current-buffer (map-elt (agent-shell-subagent--root-state state) :buffer)
      (if-let* ((cwd)
                (local (agent-shell--resolve-path cwd))
                ((file-directory-p local)))
          local
        default-directory))))

(defun agent-shell-subagent--buffer-name (root-buffer record)
  "Return a buffer name for sub-agent RECORD of ROOT-BUFFER.

  => \"Claude Agent @ project > Test investigator\""
  (format "%s > %s" (buffer-name root-buffer) (agent-shell-subagent--title record)))

(defun agent-shell-subagent--parent-buffer-label (state)
  "Return a label for the parent of sub-agent STATE."
  (if-let* ((parent (agent-shell-subagent--record
                     state (map-elt state :subagent-parent-session-id))))
      (agent-shell-subagent--title parent)
    (buffer-name (map-elt state :subagent-root-buffer))))

(defun agent-shell-subagent--render-header (state record)
  "Render sub-agent RECORD's header fragment in its buffer, using STATE."
  (agent-shell--update-fragment
   :state state
   :namespace-id "subagent"
   :block-id "header"
   :label-left (propertize "Sub-agent" 'font-lock-face 'agent-shell-section-heading)
   :label-right (agent-shell-subagent--title record)
   :body (string-join
          (delq nil
                (list (map-elt record :description)
                      (concat "Parent: "
                              (agent-shell--make-button
                               :text (agent-shell-subagent--parent-buffer-label state)
                               :help "Switch to parent conversation"
                               :kind 'subagent
                               :action #'agent-shell-subagent-parent)
                              (when-let* ((key (where-is-internal
                                                #'agent-shell-subagent-parent nil t)))
                                (propertize (format " (%s)" (key-description key))
                                            'font-lock-face 'agent-shell-secondary)))
                      (propertize "Read-only: the agent drives this conversation."
                                  'font-lock-face 'agent-shell-secondary)))
          "\n\n")
   :expanded t
   :navigation 'never
   :above-last-prompt t))

(cl-defun agent-shell-subagent--ensure-buffer (&key state session-id)
  "Return sub-agent SESSION-ID's buffer, creating it if needed.

STATE is any state in the session tree.  A newly created buffer shares
the main shell's ACP client and gets everything kept for the sub-agent
so far replayed into it.  It is not displayed."
  (let* ((root-state (agent-shell-subagent--root-state state))
         (record (or (agent-shell-subagent--record root-state session-id)
                     (error "Unknown sub-agent session: %s" session-id))))
    (if (buffer-live-p (map-elt record :buffer))
        (map-elt record :buffer)
      (let ((buffer (agent-shell--start
                     :config (map-elt root-state :agent-config)
                     :no-focus t
                     :new-session t
                     :session-strategy 'new
                     :cwd (agent-shell-subagent--cwd root-state record)
                     :subagent (list (cons :root-buffer (map-elt root-state :buffer))
                                     (cons :client (map-elt root-state :client))
                                     (cons :session-id session-id)
                                     (cons :parent-session-id
                                           (map-elt record :parent-session-id))
                                     (cons :buffer-name
                                           (agent-shell-subagent--buffer-name
                                            (map-elt root-state :buffer) record))))))
        (map-put! record :buffer buffer)
        (with-current-buffer buffer
          (agent-shell-subagent--render-header agent-shell--state record)
          (dolist (notification (reverse (map-elt record :notifications)))
            (agent-shell--dispatch-notification
             :state agent-shell--state
             :acp-notification notification))
          ;; Show the replay in full right away, rather than leave its
          ;; last streamed text to the render timer.
          (agent-shell--flush-stream-render agent-shell--state))
        (agent-shell-subagent--emit-update :state root-state :record record)
        buffer))))

(defun agent-shell-subagent--adopt (state subagent)
  "Turn freshly made STATE into sub-agent SUBAGENT's state.

SUBAGENT is an alist with :root-buffer, :client, :session-id, and
:parent-session-id.  The sub-agent reuses the main shell's
initialized client and never bootstraps one of its own."
  (map-put! state :client (map-elt subagent :client))
  (map-put! state :initialized t)
  (map-put! state :authenticated t)
  (map-put! state :init-finished t)
  (map-put! state :subagent-root-buffer (map-elt subagent :root-buffer))
  (map-put! state :subagent-parent-session-id (map-elt subagent :parent-session-id))
  (map-put! (map-elt state :session) :id (map-elt subagent :session-id))
  state)

(defun agent-shell-subagent--clean-up (state)
  "Release sub-agent resources held by STATE.

Killing a sub-agent's buffer keeps its record (and kept traffic), so it
can be opened again.  Its pending requests live in the main shell, so
they're unaffected.  Shutting down the main shell kills all of its
sub-agent buffers and forgets them, since they share its client."
  (if-let* ((root-buffer (map-elt state :subagent-root-buffer)))
      (when-let* (((buffer-live-p root-buffer))
                  (record (agent-shell-subagent--record
                           state (map-nested-elt state '(:session :id))))
                  ((eq (map-elt record :buffer) (map-elt state :buffer))))
        (map-put! record :buffer nil)
        (agent-shell-subagent--emit-update :state state :record record))
    (agent-shell-subagent--forget-all state)))

(defun agent-shell-subagent--cancellable-p (state)
  "Return non-nil unless STATE is a sub-agent that cannot be cancelled.

See `agent-shell-subagent--can-cancel-p'."
  (or (not (map-elt state :subagent-root-buffer))
      (when-let* ((record (agent-shell-subagent--record
                           state (map-nested-elt state '(:session :id)))))
        (agent-shell-subagent--can-cancel-p state record))))

(defun agent-shell-subagent--indicator (state)
  "Return a header indicator for STATE's sub-agents, or nil if none.

  main shell with 3 sub-agents, 1 working => \"3 sub-agents (1 running)\"
  sub-agent buffer                         => \"sub-agent: done\""
  (if (map-elt state :subagent-root-buffer)
      (when-let* ((record (agent-shell-subagent--record
                           state (map-nested-elt state '(:session :id)))))
        (propertize (concat "sub-agent: " (agent-shell-subagent--status-text record))
                    'face 'agent-shell-secondary))
    (when-let* ((records (agent-shell-subagent--records state)))
      (propertize
       (concat (format "%d sub-agent%s" (length records)
                       (if (length= records 1) "" "s"))
               (when-let* ((running (seq-count #'agent-shell-subagent--working-p records))
                           ((> running 0)))
                 (format " (%d running)" running)))
       'face 'agent-shell-secondary))))

;;; Commands

(cl-defun agent-shell-subagent--open (&key root-buffer session-id)
  "Open sub-agent SESSION-ID of ROOT-BUFFER and display it."
  (unless (buffer-live-p root-buffer)
    (user-error "Main shell is gone"))
  (unless (agent-shell-subagent--record
           (buffer-local-value 'agent-shell--state root-buffer) session-id)
    (user-error "This sub-agent is no longer available"))
  (agent-shell--display-buffer
   (agent-shell-subagent--ensure-buffer
    :state (buffer-local-value 'agent-shell--state root-buffer)
    :session-id session-id)))

(defun agent-shell-subagent--depth (state record)
  "Return RECORD's nesting depth in STATE's tree (1 for direct children)."
  (let ((depth 1)
        (parent record))
    (while (setq parent (agent-shell-subagent--record
                         state (map-elt parent :parent-session-id)))
      (setq depth (1+ depth)))
    depth))

(defun agent-shell-subagent--candidates (state)
  "Return completion candidates for STATE's sub-agents.

An alist of display string to session id, for example:

  ((\"Test investigator  [running]\" . \"c1\")
   (\"  Log reader  [done]\" . \"c2\"))"
  (let ((candidates nil))
    (dolist (record (agent-shell-subagent--tree-records state))
      (let ((label (format "%s%s  [%s]"
                           (make-string (* 2 (1- (agent-shell-subagent--depth state record))) ?\s)
                           (agent-shell-subagent--title record)
                           (agent-shell-subagent--status-text record))))
        (when (assoc label candidates)
          (setq label (format "%s  <%s>" label (map-elt record :session-id))))
        (push (cons label (map-elt record :session-id)) candidates)))
    (nreverse candidates)))

(defun agent-shell-subagent--card-session-id-at-point ()
  "Return the session id of the sub-agent card at point, if any."
  (when-let* ((ui-state (get-text-property (point) 'agent-shell-ui-state))
              (qualified-id (map-elt ui-state :qualified-id))
              (prefix (concat "subagent-" (agent-shell-subagent--card-block-id "")))
              ((string-prefix-p prefix qualified-id)))
    (string-remove-prefix prefix qualified-id)))

(defun agent-shell-open-subagent (&optional session-id)
  "Open one of the current shell's sub-agent conversations.

With point on a sub-agent card, open that sub-agent.  Otherwise, pick
one from all sub-agents in the main session, nested ones included.

From Lisp, SESSION-ID opens that sub-agent instead (see
`agent-shell-subagents').  To open one without displaying it, use
`agent-shell-subagent-buffer'."
  (interactive)
  (let* ((shell-buffer (agent-shell--shell-buffer :no-create t))
         (state (agent-shell-subagent--root-state
                 (buffer-local-value 'agent-shell--state shell-buffer)))
         (session-id (or session-id
                         (with-current-buffer shell-buffer
                           (agent-shell-subagent--card-session-id-at-point))
                         (let ((candidates (agent-shell-subagent--candidates state)))
                           (unless candidates
                             (user-error "No sub-agents in this session"))
                           (map-elt candidates
                                    (completing-read "Sub-agent: " candidates nil t))))))
    (agent-shell-subagent--open :root-buffer (map-elt state :buffer)
                                :session-id session-id)))

(defun agent-shell-subagent--shell-state (shell-buffer)
  "Return SHELL-BUFFER's main shell state.

SHELL-BUFFER is a main shell or one of its sub-agent buffers."
  (unless (and (buffer-live-p shell-buffer)
               (with-current-buffer shell-buffer
                 (derived-mode-p 'agent-shell-mode)))
    (error "Not a shell: %s" shell-buffer))
  (agent-shell-subagent--root-state
   (buffer-local-value 'agent-shell--state shell-buffer)))

(cl-defun agent-shell-subagents (&key shell-buffer)
  "Return the sub-agents in SHELL-BUFFER's session tree.

SHELL-BUFFER defaults to the current buffer, and may be a main shell or
any of its sub-agent buffers.  Sub-agents are listed parent first, so
nested ones follow the sub-agent that started them.

Each sub-agent is an alist with:

  :session-id         Its session id.
  :parent-session-id  Session id of the session that started it.
  :title              Display title.
  :description        Description, if any.
  :status             Status text: \"started\", \"running\", \"needs
                      input\", \"done\", \"cancelled\", \"failed\"...
  :state              Its ACP state: \"running\", \"idle\"...
  :stop-reason        Its ACP stop reason, once idle.
  :error              Error message, if it failed.
  :can-cancel         Non-nil if it can currently be cancelled.
  :depth              Nesting depth, 1 for the main session's own.
  :pending-requests   Number of its requests awaiting an answer (in
                      the main shell).
  :cwd                Its working directory, if it has its own.
  :task               The task its parent gave it, if known.
  :result             What it reported back, if known.
  :buffer             Its buffer, or nil if not opened.
  :shell-buffer       The main shell.

For example:

  (agent-shell-subagents :shell-buffer (car (agent-shell-buffers)))
  => (((:session-id . \"c1\") (:title . \"Test investigator\")
       (:status . \"running\") (:depth . 1) (:buffer) ...)
      ((:session-id . \"c2\") (:parent-session-id . \"c1\")
       (:title . \"Log reader\") (:depth . 2) ...))

The main shell's `subagent-update' event (see `agent-shell-subscribe-to')
fires when a sub-agent is announced or changes, or its buffer is opened
or killed.  :pending-requests changes come with the main shell's
`permission-request', `permission-response', `elicitation-request',
and `elicitation-response' events instead."
  (let ((state (agent-shell-subagent--shell-state (or shell-buffer (current-buffer)))))
    (mapcar (lambda (record)
              (agent-shell-subagent--describe state record))
            (agent-shell-subagent--tree-records state))))

(cl-defun agent-shell-subagent-buffer (&key shell-buffer session-id)
  "Return sub-agent SESSION-ID's buffer, opening it if needed.

SHELL-BUFFER defaults to the current buffer, and may be a main shell or
any of its sub-agent buffers.  A newly opened buffer isn't displayed,
so display it as you see fit.  For example:

  (pop-to-buffer (agent-shell-subagent-buffer :shell-buffer shell
                                              :session-id \"c1\"))"
  (agent-shell-subagent--ensure-buffer
   :state (agent-shell-subagent--shell-state (or shell-buffer (current-buffer)))
   :session-id session-id))

(defun agent-shell-subagent--ensure-not-subagent (&optional buffer)
  "Signal a `user-error' if BUFFER (or the current one) is a sub-agent's.

As per spec, a client sends nothing on a sub-agent's session but
answers and (when allowed) cancellation, so session commands (prompting,
changing modes or options, restarting, forking...) don't apply."
  (when (agent-shell-subagent-p buffer)
    (user-error "Sub-agents are driven by their parent, so this isn't available")))

(defun agent-shell-subagent--reload (buffer)
  "Reload sub-agent BUFFER from what its main shell kept for it.

Return the new buffer, displayed in BUFFER's place."
  (let* ((state (buffer-local-value 'agent-shell--state buffer))
         (root-buffer (map-elt state :subagent-root-buffer))
         (session-id (map-nested-elt state '(:session :id)))
         (windows (get-buffer-window-list buffer nil t)))
    (unless (buffer-live-p root-buffer)
      (user-error "Main shell is gone"))
    (kill-buffer buffer)
    (let ((new-buffer (agent-shell-subagent--ensure-buffer
                       :state (buffer-local-value 'agent-shell--state root-buffer)
                       :session-id session-id)))
      (if windows
          (dolist (window windows)
            (set-window-buffer window new-buffer))
        (agent-shell--display-buffer new-buffer))
      new-buffer)))

(defun agent-shell-subagent-parent (&optional main)
  "Switch from a sub-agent conversation to its parent.

With prefix argument MAIN, switch to the main shell instead (relevant
for nested sub-agents)."
  (interactive "P")
  (let ((state (agent-shell--state)))
    (unless (map-elt state :subagent-root-buffer)
      (user-error "Not a sub-agent conversation"))
    (unless (buffer-live-p (map-elt state :subagent-root-buffer))
      (user-error "Main shell is gone"))
    (if-let* (((not main))
              ((agent-shell-subagent--record
                state (map-elt state :subagent-parent-session-id))))
        (agent-shell-subagent--open :root-buffer (map-elt state :subagent-root-buffer)
                                    :session-id (map-elt state :subagent-parent-session-id))
      (agent-shell--display-buffer (map-elt state :subagent-root-buffer)))))

(provide 'agent-shell-subagent)

;;; agent-shell-subagent.el ends here
