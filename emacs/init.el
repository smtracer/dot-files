;;; init.el --- User configuration entrypoint.
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Ordered start sequence
(require 'bootstrap-packaging)
(require 'init-gc)
(require 'init-theme)
(require 'init-user-overlay)

;; ...
(require 'init-emacs-misc)
(require 'init-org)
(require 'init-text-editing)
(require 'init-windows-and-navigation)
(require 'init-minibuffer)

;; Programming
(require 'init-compile)
(require 'init-lang-commons)
(require 'init-lang-bash)
(require 'init-lang-emacs-lisp)
(require 'init-lang-java)
(require 'init-lang-json)
(require 'init-lang-python)
(require 'init-lang-ruby)
(require 'init-lang-rust)
(require 'init-lang-toml)
(require 'init-lang-typescript)
(require 'init-lang-yaml)

(provide 'init)
