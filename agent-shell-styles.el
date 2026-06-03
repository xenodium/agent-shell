;;; agent-shell-styles.el --- Alternative status/kind label styles. -*- lexical-binding: t; -*-

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
;; Alternative functions for `agent-shell-status-kind-label-function'.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'map)
(require 'seq)
(require 'svg nil :noerror)

(declare-function agent-shell--add-text-properties "agent-shell")
(declare-function agent-shell--svg-fill-color "agent-shell")

(defun agent-shell--short-kind-label (kind)
  "Return a short label for tool call KIND string."
  (pcase kind
    ("search" "find")
    ("execute" "run")
    (_ kind)))

(defun agent-shell--status-config (status)
  "Return alist with :label, :icon, and :face for STATUS string.

  (agent-shell--status-config \"completed\")
  ;; => ((:label . \"done\") (:icon . \"✓\") (:face . success))"
  (pcase status
    ("pending" '((:label . "wait") (:icon . "◇") (:face . font-lock-comment-face)))
    ("in_progress" '((:label . "busy") (:icon . "◆") (:face . warning)))
    ("completed" '((:label . "done") (:icon . "✓") (:face . success)))
    ("failed" '((:label . "error") (:icon . "✗") (:face . error)))
    (_ '((:label . "unknown") (:icon . "?") (:face . warning)))))

(defun agent-shell--default-status-kind-label (status kind)
  "Default rendering for STATUS and KIND labels.
STATUS is a string like \"completed\" or nil.
KIND is a string like \"read\" or nil.
Returns a propertized string or nil."
  (let* ((status-config (agent-shell--status-config status))
         (label-format (if (display-graphic-p) " %s " "[%s]"))
         (status-text (when status
                        (let ((label (map-elt status-config :label))
                              (face (map-elt status-config :face)))
                          (agent-shell--add-text-properties
                           (propertize (format label-format label)
                                       'font-lock-face 'default)
                           'font-lock-face (list face '(:inverse-video t))))))
         (kind-text (when kind
                      (let ((box-color (face-foreground
                                        (map-elt status-config :face) nil t)))
                        (agent-shell--add-text-properties
                         (propertize (format label-format
                                            (agent-shell--short-kind-label kind))
                                     'font-lock-face 'default)
                         'font-lock-face `((:box (:color ,box-color))))))))
    (concat status-text kind-text)))

(defun agent-shell--background-tint-status-kind-label (status kind)
  "Render STATUS and KIND as tinted background labels.

Derives background by blending the face foreground (30%) with the
default background (70%), so it adapts to any theme.

  (agent-shell--background-tint-status-kind-label \"completed\" \"read\")
  ;; => #(\" done \" ...) #(\" read \" ...)

STATUS is a string like \"completed\" or nil.
KIND is a string like \"read\" or nil.
Returns a propertized string or nil."
  (let* ((status-config (agent-shell--status-config status))
         (fg (face-foreground (map-elt status-config :face) nil t))
         (bg-base (face-background 'default nil t))
         (bg (when (and fg bg-base)
               (apply #'format "#%02x%02x%02x"
                      (seq-mapn (lambda (f b)
                                  (/ (+ (* f 3) (* b 7)) 10))
                                (color-values fg)
                                (color-values bg-base)))))
         (label-format (if (display-graphic-p) " %s " "[%s]"))
         (status-text (when status
                        (propertize (format label-format
                                           (map-elt status-config :label))
                                    'font-lock-face
                                    `(:background ,bg :foreground ,fg
                                      :weight bold))))
         (kind-text (when kind
                      (propertize (format label-format
                                         (agent-shell--short-kind-label kind))
                                  'font-lock-face
                                  `(:background ,bg :foreground ,fg
                                    :slant italic)))))
    (concat status-text kind-text)))

(defun agent-shell--unicode-icons-status-kind-label (status kind)
  "Render STATUS as a unicode icon and KIND as typed text.

  (agent-shell--unicode-icons-status-kind-label \"completed\" \"read\")
  ;; => \"✓ read\"

  (agent-shell--unicode-icons-status-kind-label \"completed\" nil)
  ;; => \"✓\"

STATUS is a string like \"completed\" or nil.
KIND is a string like \"read\" or nil.
Returns a propertized string or nil."
  (let ((status-config (agent-shell--status-config status))
        (status-text nil)
        (kind-text nil))
    (when status
      (setq status-text (propertize (map-elt status-config :icon)
                                    'font-lock-face
                                    (map-elt status-config :face))))
    (when kind
      (setq kind-text (propertize (agent-shell--short-kind-label kind)
                                  'font-lock-face 'font-lock-type-face)))
    (if (and status-text kind-text)
        (concat status-text " " kind-text)
      (or status-text kind-text))))

(defun agent-shell--plain-colored-status-kind-label (status kind)
  "Render STATUS and KIND as plain colored text with no decoration.

  (agent-shell--plain-colored-status-kind-label \"completed\" \"read\")
  ;; => #(\" done \" ...) #(\" read \" ...)

STATUS is a string like \"completed\" or nil.
KIND is a string like \"read\" or nil.
Returns a propertized string or nil."
  (let* ((status-config (agent-shell--status-config status))
         (face (map-elt status-config :face))
         (label-format (if (display-graphic-p) " %s " "[%s]"))
         (status-text (when status
                        (propertize (format label-format
                                           (map-elt status-config :label))
                                    'font-lock-face face)))
         (kind-text (when kind
                      (propertize (format label-format
                                         (agent-shell--short-kind-label kind))
                                  'font-lock-face face))))
    (concat status-text kind-text)))

(defun agent-shell--svg-default-background-color ()
  "Return the default face background as an `#rrggbb' hex string for SVG."
  (let* ((name (face-background 'default nil 'default))
         (rgb (and (stringp name) (color-name-to-rgb name))))
    (if rgb
        (apply #'color-rgb-to-hex (append rgb '(2)))
      "#000000")))

(defun agent-shell--status-svg-icon (status face)
  "Return an SVG image spec drawing a glyph for STATUS.

The canvas is filled with FACE foreground (matching what
`(:inverse-video t)' produces on surrounding text), and the glyph is
drawn in the default background color.

STATUS is one of \"pending\", \"in_progress\", \"completed\", \"failed\";
any other value renders as a question mark."
  (let* ((height (frame-char-height))
         (icon-size (* height 0.85))
         (h-padding (* height 0.32))
         (width (+ icon-size (* 2 h-padding)))
         (bg (agent-shell--svg-fill-color face))
         (fg (agent-shell--svg-default-background-color))
         (cx (/ width 2.0))
         (cy (/ height 2.0))
         (svg (svg-create width height)))
    (svg-rectangle svg 0 0 width height :fill bg)
    (pcase status
      ((or "pending" "in_progress")
       (let ((r (* icon-size 0.08))
             (spacing (* icon-size 0.26))
             (y (+ cy (* icon-size 0.18))))
         (svg-circle svg (- cx spacing) y r :fill fg)
         (svg-circle svg cx y r :fill fg)
         (svg-circle svg (+ cx spacing) y r :fill fg)))
      ("completed"
       (svg-polyline svg
                     `((,(- cx (* icon-size 0.32)) . ,(+ cy (* icon-size 0.02)))
                       (,(- cx (* icon-size 0.08)) . ,(+ cy (* icon-size 0.24)))
                       (,(+ cx (* icon-size 0.32)) . ,(- cy (* icon-size 0.20))))
                     :stroke fg :stroke-width 1.4 :fill "none"
                     :stroke-linecap "round" :stroke-linejoin "round"))
      ("failed"
       (let ((d (* icon-size 0.22)))
         (svg-line svg (- cx d) (- cy d) (+ cx d) (+ cy d)
                   :stroke fg :stroke-width 1.4 :stroke-linecap "round")
         (svg-line svg (- cx d) (+ cy d) (+ cx d) (- cy d)
                   :stroke fg :stroke-width 1.4 :stroke-linecap "round")))
      (_
       (svg-text svg "?"
                 :x cx :y (* height 0.75)
                 :text-anchor "middle"
                 :font-size (* icon-size 0.85)
                 :fill fg)))
    (svg-image svg :ascent 'center)))

(defun agent-shell--svg-icon-status-kind-label (status kind)
  "Render STATUS as an SVG icon and KIND as boxed text.

The status icon is wrapped in ` %s ' with the same `(:inverse-video t)'
face treatment as `agent-shell--default-status-kind-label', so the
surrounding spaces share the face-colored background.  In a terminal
or when SVG is unavailable, falls back to the unicode glyph from
`agent-shell--status-config'.

  (agent-shell--svg-icon-status-kind-label \"completed\" \"read\")
  ;; => #(\" X \" ...) #(\" read \" ...)

STATUS is a string like \"completed\" or nil.
KIND is a string like \"read\" or nil.
Returns a propertized string or nil."
  (let* ((status-config (agent-shell--status-config status))
         (face (map-elt status-config :face))
         (use-svg (and (display-graphic-p) (image-type-available-p 'svg)))
         (status-text
          (when status
            (if use-svg
                (propertize (format "%s"
                                    (propertize
                                     "x"
                                     'display (agent-shell--status-svg-icon
                                               status face)))
                            'font-lock-face (list face '(:inverse-video t)))
              (propertize (format "[%s]" (map-elt status-config :icon))
                          'font-lock-face face))))
         (kind-text
          (when kind
            (agent-shell--add-text-properties
             (propertize (format (if (display-graphic-p) " %s " "[%s]")
                                 (agent-shell--short-kind-label kind))
                         'font-lock-face 'default)
             'font-lock-face
             `((:box (:color ,(face-foreground face nil t))))))))
    (concat status-text kind-text)))

(provide 'agent-shell-styles)

;;; agent-shell-styles.el ends here
