;;; init-whitespace-cleanup-mode.el ---
;;; Commentary:
;;; Code:

(use-package whitespace-cleanup-mode
  :hook ((prog-mode . whitespace-cleanup-mode)
         (org-mode . whitespace-cleanup-mode)))

(provide 'init-whitespace-cleanup-mode)
;;; init-whitespace-cleanup-mode.el ends here
