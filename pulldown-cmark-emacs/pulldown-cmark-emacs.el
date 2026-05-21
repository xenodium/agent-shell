;;; pulldown-cmark-emacs.el -*- lexical-binding: t -*-
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defvar pulldown-cmark-emacs-development-mode nil
  "If non-nil, use `rs-module` to hot-reload the library on every initialization.
Set this to `t` in your development configuration before loading the package.")

(defvar pulldown-cmark-emacs--module-loaded nil
  "Non-nil after the native module has been loaded once.")

(defun pulldown-cmark-emacs--so-path ()
  "Return the absolute path to pulldown-cmark-emacs.so, sibling of this file."
  (expand-file-name
   (if pulldown-cmark-emacs-development-mode
       "target/debug/libpulldown_cmark_emacs.so" ; Usually debug during hot-reloads
     "target/release/libpulldown_cmark_emacs.so") ; Production
   (file-name-directory
    (or load-file-name
        (and (boundp 'byte-compile-current-file)
             byte-compile-current-file)
        default-directory))))

(defun pulldown-cmark-emacs-load-or-reload-module ()
  "Load or dynamically reload the pulldown-cmark-emacs native module."
  (interactive)
  (let ((so-path (pulldown-cmark-emacs--so-path)))
    (if pulldown-cmark-emacs-development-mode
        (progn
          (require 'rs-module)
          (rs-module/load so-path)
          (setq pulldown-cmark-emacs--module-loaded t)
          (message "pulldown-cmark-emacs: Hot-swapped module from %s" so-path))
      (unless pulldown-cmark-emacs--module-loaded
        (if (file-exists-p so-path)
            (progn
              (module-load so-path)
              (setq pulldown-cmark-emacs--module-loaded t))
          (error "Native module not found at %s. Did you run 'cargo build --release'?" so-path))))))

(unless noninteractive
  (pulldown-cmark-emacs-load-or-reload-module))

(provide 'pulldown-cmark-emacs)
;;; pulldown-cmark-emacs.el ends here
