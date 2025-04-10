;;; init-builtin-theme.el ---
;;; Commentary:
;;; Code:

(use-package emacs
  :hook ((after-init . (lambda() (menu-bar-mode -1)))))
;; Set the vertical border between windows to a continuous line instead of the
;; default dashed line.
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(provide 'init-builtin-theme)
;;; init-builtin-theme.el ends here
