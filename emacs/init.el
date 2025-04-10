;;; -*- flymake-mode: nil -*-
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

(provide 'init)
;;; init.el ends here
