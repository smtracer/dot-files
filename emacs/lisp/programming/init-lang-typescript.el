;;; init-lang-typescript.el ---
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

;; FIXME: Assumes that a typescript language server is installed
(use-package lsp-mode
  :hook (typescript-ts-mode . lsp-deferred))

(provide 'init-lang-typescript)
;;; init-lang-typescript.el ends here
