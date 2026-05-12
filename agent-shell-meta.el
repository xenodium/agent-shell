;;; agent-shell-meta.el --- Meta helpers for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Alvaro Ramirez and contributors

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
;; Meta helpers for agent-shell tool call handling.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)

(defun agent-shell--meta-lookup (meta key)
  "Lookup KEY in META, handling symbol or string keys.

For example:

  (agent-shell--meta-lookup \\='((stdout . \"hello\")) \\='stdout)
    => \"hello\"

  (agent-shell--meta-lookup \\='((\"stdout\" . \"hello\")) \\='stdout)
    => \"hello\""
  (let ((value (map-elt meta key)))
    (when (and (null value) (symbolp key))
      (setq value (map-elt meta (symbol-name key))))
    value))

(defun agent-shell--meta-find-tool-response (meta)
  "Find a toolResponse value nested inside any namespace in META.
Agents may place toolResponse under an agent-specific key (e.g.
_meta.agentName.toolResponse).  Walk the top-level entries of META
looking for one that contains a toolResponse.

For example:

  (agent-shell--meta-find-tool-response
   \\='((claudeCode . ((toolResponse . ((stdout . \"hi\")))))))
    => ((stdout . \"hi\"))"
  (or (agent-shell--meta-lookup meta 'toolResponse)
      (when-let ((match (seq-find (lambda (entry)
                                    (and (consp entry) (consp (cdr entry))
                                         (agent-shell--meta-lookup (cdr entry) 'toolResponse)))
                                  (when (listp meta) meta))))
        (agent-shell--meta-lookup (cdr match) 'toolResponse))))

(defun agent-shell--tool-call-meta-response-text (update)
  "Return tool response text from UPDATE meta, if present.
Looks for a toolResponse entry inside any agent-specific _meta
namespace and extracts text from it.  Handles three common shapes:

An alist with a `stdout' string:

  \\='((toolCallId . \"id\")
    (_meta . ((claudeCode . ((toolResponse . ((stdout . \"output\"))))))))
    => \"output\"

An alist with a `content' string:

  \\='((_meta . ((agent . ((toolResponse . ((content . \"text\"))))))))
    => \"text\"

A vector of text items:

  \\='((_meta . ((toolResponse . [((type . \"text\") (text . \"one\"))
                                ((type . \"text\") (text . \"two\"))]))))
    => \"one\\n\\ntwo\""
  (when-let* ((meta (or (map-elt update '_meta)
                        (map-elt update 'meta)))
              (response (agent-shell--meta-find-tool-response meta)))
    (cond
     ((and (listp response)
           (not (vectorp response))
           (let ((stdout (agent-shell--meta-lookup response 'stdout)))
             (and (stringp stdout) (not (string-empty-p stdout)))))
      (agent-shell--meta-lookup response 'stdout))
     ((and (listp response)
           (not (vectorp response))
           (stringp (agent-shell--meta-lookup response 'content)))
      (agent-shell--meta-lookup response 'content))
     ((vectorp response)
      (let* ((items (append response nil))
             (parts (delq nil
                          (mapcar (lambda (item)
                                    (let ((text (agent-shell--meta-lookup item 'text)))
                                      (when (and (stringp text)
                                                 (not (string-empty-p text)))
                                        text)))
                                  items))))
        (when parts
          (mapconcat #'identity parts "\n\n")))))))

(defun agent-shell--tool-call-terminal-output-data (update)
  "Return terminal output data string from UPDATE meta, if present.
Extracts the data field from _meta.terminal_output, used by agents
like codex-acp for incremental streaming.

For example:

  (agent-shell--tool-call-terminal-output-data
   \\='((_meta . ((terminal_output . ((data . \"hello\")))))))
    => \"hello\""
  (when-let* ((meta (or (map-elt update '_meta)
                        (map-elt update 'meta)))
              (terminal (or (agent-shell--meta-lookup meta 'terminal_output)
                            (agent-shell--meta-lookup meta 'terminal-output))))
    (let ((data (agent-shell--meta-lookup terminal 'data)))
      (when (stringp data)
        data))))

(provide 'agent-shell-meta)

;;; agent-shell-meta.el ends here
