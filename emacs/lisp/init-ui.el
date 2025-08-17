;;; init-ui.el ---
;;; Commentary:
;;; Code:

(menu-bar-mode -1)
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(use-package centaur-tabs
  :hook
  ((dired-mode . centaur-tabs-local-mode)
   (xref--xref-buffer-mode . centaur-tabs-local-mode)
   (compilation-mode . centaur-tabs-local-mode))
  :config
  (setq centaur-tabs-set-icons t
        centaur-tabs-label-fixed-length 15
        centaur-tabs-plain-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-mode t)
  :bind
  (:map global-map
        ("C-c ;" . centaur-tabs-backward-tab)
        ("C-c '" . centaur-tabs-forward-tab)))

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
