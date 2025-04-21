;;; init-magit.el ---
;;; Commentary:
;;; Code:

(use-package magit
  :commands magit
  ;; :init
  ;; Save open project files without a prompt when opening magit.
  :config
  (setq magit-save-repository-buffers 'dontask))

(provide 'init-magit)
;;; init-magit.el ends here
