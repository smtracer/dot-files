;;; init.el --- Emacs user configuration.
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'bootstrap)
;; -----------------
(require 'init-editing)
(require 'init-minibuffer)
(require 'init-misc)
(require 'init-org)
(require 'init-programming)
(require 'init-theme)

;; Load extended local initialization files.
(let ((local-config-dir "~/.config/emacs/"))
  (dolist (local-config-file (directory-files-recursively local-config-dir "\\.el$"))
    (when (file-regular-p local-config-file)
      (load-file local-config-file))))

(provide 'init)
;;; init.el ends here
