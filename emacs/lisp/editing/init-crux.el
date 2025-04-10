;;; init-crux.el ---
;;; Commentary:
;;; Code:

(use-package crux
  :bind
  (:map global-map
        ("C-o" . crux-smart-open-line)
        ("C-q" . crux-smart-open-line-above)))

(provide 'init-crux)
;;; init-crux.el ends here
