;;; init-dirvish.el ---
;;; Commentary:
;;; Code:

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  (dirvish-side-follow-mode)
  :bind
  (:map user-overlay-prefix-map
        ("c s" . dirvish-side)))

(provide 'init-dirvish)
;;; init-dirvish.el ends here
