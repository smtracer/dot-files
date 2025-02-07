;;; init-theme.el --- Load theme elements.
;;; Commentary:
;;; Code:

(menu-bar-mode -1)
;; Continuous line border between split windows
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(use-package nordic-vein-theme
  :straight
  ;; FIXME: This repo does not exist! Workaround by creating a git repo in
  ;; 'straight-base-dir'/straight/repos with a file 'nordic-vein-theme.el'.
  (:type git :repo "https://github.com/smtracer/nordic-vein")
  :init
  (load-theme 'nordic-vein t))

(use-package powerline
  :init
  (powerline-center-theme))

;; Temporarily highlight the current line when one of 'pulsar-pulse-functions'
;; is called.
(use-package pulsar
  :init
  (pulsar-global-mode 1)
  :config
  ;; Enable pulsar when calling next-error in compilation buffers.
  (add-hook 'next-error-hook #'pulsar-pulse-line)
  (setq pulsar-face 'pulsar-yellow
        pulsar-pulse nil
        pulsar-pulse-functions
            '(backward-page
              bookmark-jump
              delete-other-windows
              delete-window
              forward-page
              goto-line
              handle-switch-frame
              handle-select-window
              move-to-window-line-top-bottom
              narrow-to-defun
              narrow-to-page
              narrow-to-region
              next-buffer
              next-multiframe-window
              org-backward-heading-same-level
              org-forward-heading-same-level
              org-next-visible-heading
              org-previous-visible-heading
              ace-window ;; TODO: conditional load
              outline-backward-same-level
              outline-forward-same-level
              outline-next-visible-heading
              outline-previous-visible-heading
              outline-up-heading
              recenter-top-bottom
              reposition-window
              scroll-down-command
              scroll-up-command
              widen)))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(provide 'init-theme)
;;; init-theme.el ends here
