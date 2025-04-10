;;; init-company.el ---
;;; Commentary:
;;; Code:

(use-package company
  :hook (emacs-startup . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1))

(provide 'init-company)
;;; init-company.el ends here
