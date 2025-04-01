;;; init-theme.el ---
;;; Commentary:
;;; Code:

(menu-bar-mode -1)
;; Set the vertical border between windows to a continuous line instead of the
;; default dashed line.
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(use-package nordic-vein-theme
  :straight
  (:type git :repo "https://github.com/smtracer/nordic-vein")
  :init
  (load-theme 'nordic-vein t))

;; (use-package doom-themes
;;   :init
;;   (load-theme 'doom-nord t)

;;   (setq doom-nord-region-highlight 'frost
;;         doom-nord-brighter-comments t)
;;   )

(use-package doom-modeline
  :init
  (doom-modeline-mode))

(provide 'init-theme)
;;; init-theme.el ends here
