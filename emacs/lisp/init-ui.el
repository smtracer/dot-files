;;; init-ui.el ---
;;; Commentary:
;;; Code:

(menu-bar-mode -1)
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(use-package doom-modeline
  :hook ((emacs-startup-hook . doom-modeline-mode)))

(use-package nerd-icons
  :config
  (setq nerd-icons-color-icons nil))

(use-package nordic-vein-theme
  :straight (:type git :repo "https://github.com/smtracer/nordic-vein-theme")
  :init
  (load-theme 'nordic-vein t))

(provide 'init-ui)
;;; init-ui.el ends here.
