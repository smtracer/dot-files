;;; init-lang-python.el ---
;;; Commentary:
;;; Code:

;; TODO: Configure 'lsp-mode' for python
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

;; FIXME: Assumes that a python language server is installed
(use-package lsp-mode
  :hook (python-ts-mode . lsp-deferred))

(provide 'init-lang-python)
;;; init-lang-python.el ends here
