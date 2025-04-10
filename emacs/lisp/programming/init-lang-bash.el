;;; init-lang-bash.el ---
;;; Commentary:
;;; Code:

;; HACK: Assumes that a bash ts grammar is installed
;; ('treesit-install-language-grammer')
(add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("bashrc" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("bash_profile" . bash-ts-mode))

;; FIXME: Assumes that a bash language server is installed
(use-package lsp-mode
  :hook (bash-ts-mode . lsp-deferred))

(provide 'init-lang-bash)
;;; init-lang-bash.el ends here
