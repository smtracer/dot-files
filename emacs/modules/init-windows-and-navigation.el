;;; init-windows-and-navigation.el ---
;;; Commentary:
;;; Code:

(use-package ace-window
  :config
  (setq aw-background nil
        aw-ignore-current t
        aw-scope 'frame
        aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;))
  :bind
  (:map global-map
        ("C-x o" . ace-window)))

(provide 'init-windows-and-navigation)
;;; init-windows-and-navigation.el ends here.
