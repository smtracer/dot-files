;;; init-lang-rust.el ---
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; Disable 'rust-ts-mode's diagnostics in favor of 'lsp-mode'
(setq-default rust-ts-flymake-command nil)

;; FIXME: Assumes that a rust language server is installed
(use-package lsp-mode
  :hook (rust-ts-mode . lsp-deferred))

(use-package reformatter
  ;; NOTE:
  :hook ((rust-ts-mode . rust-reformatter-on-save-mode))
  :init
  (reformatter-define rust-reformatter
    :program "rustfmt"
    :args '("--quiet" "--emit" "stdout")))

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here
