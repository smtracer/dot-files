;;; init-lsp-mode.el ---
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-j ;")
  :config
  (setq lsp-modeline-code-actions-enable t
        lsp-progress-prefix " "
        lsp-modeline-code-action-fallback-icon ""
        ;; Suggested by 'lsp-doctor'
        read-process-output-max (* 1024 1024)
        lsp-lens-auto-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-signature-render-documentation nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 0
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-diagnostics t))

(provide 'init-lsp-mode)
;;; init-lsp-mode.el ends here
