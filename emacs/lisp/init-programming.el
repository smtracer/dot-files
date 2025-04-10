;;; init-programming.el ---
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/programming" user-emacs-directory))
(require 'init-builtin-programming)
(require 'init-hl-todo)
(require 'init-lsp-mode)
(require 'init-magit)
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
