;;; init-theme.el --- Load theme elements.
;;; Commentary:
;;; Code:

(setq inhibit-splash-screen t)
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
(menu-bar-mode -1)

;; TODO: Make a good rose pine port instead
(use-package nordic-vein-theme
  :straight
  (:type git :repo "https://github.com/smtracer/nordic-vein")
  :init
  (load-theme 'nordic-vein t))

;; Aesthetic & functional mode line
(use-package powerline
  :init
  (powerline-center-theme))

(provide 'init-theme)
;;; init-theme.el ends here
