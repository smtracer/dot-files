;;; init-org-modern.el ---
;;; Commentary:
;;; Code:

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-mode))
  :config
  (setq org-modern-fold-stars '(("" . ""))))

(provide 'init-org-modern)
;;; init-org-modern.el ends here
