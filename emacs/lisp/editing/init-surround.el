;;; init-surround.el ---
;;; Commentary:
;;; Code:

(use-package surround
  :bind
  (:map user-overlay-prefix-map
        ("TAB" . surround-kill-inner)
        ("C-a" . surround-kill-outer)
        ("C-w" . surround-mark-inner)
        ("C-r" . surround-change)))

(provide 'init-surround)
;;; init-surround.el ends here
