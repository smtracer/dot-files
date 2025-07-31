;;; init-programming.el ---
;;; Commentary:
;;; Code:

(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-fill-column-indicator-mode )
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode )

(use-package flymake
  :straight (:type built-in)
  :bind
  (:map user-overlay-map
        ("e n" . flymake-goto-next-error)
        ("e p" . flymake-goto-prev-error)))

(add-to-list 'load-path (expand-file-name "lisp/programming" user-emacs-directory))
(require 'init-builtin-programming)
(require 'init-lsp-mode)
;; Language setup
(require 'init-lang-bash)
(require 'init-lang-c)
(require 'init-lang-cpp)
(require 'init-lang-elisp)
(require 'init-lang-json)
(require 'init-lang-python)
(require 'init-lang-ruby)
(require 'init-lang-rust)
(require 'init-lang-toml)
(require 'init-lang-typescript)
(require 'init-lang-yaml)

(provide 'init-programming)
;;; init-programming.el ends here
