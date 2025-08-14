;;; init-lang-ruby.el ---
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-ts-mode))

;; FIXME: Assumes that a ruby language server is installed
(use-package lsp-mode
  :hook (ruby-ts-mode . lsp-deferred))

(provide 'init-lang-ruby)
;;; init-lang-ruby.el ends here
