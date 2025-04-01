;;; init-text-editing.el --- ...
;;; Commentary:
;;; Code:

;; -- Builtins --

(setq-default indent-tabs-mode nil)
(electric-pair-mode 1)
(global-set-key (kbd "M-;") 'comment-line)

;; -- External Packages --

;; Fast navigation & selection commands
(use-package avy
  :bind
  (:map ctl-j-map
        ("C-l" . avy-goto-line)
        ("k l" . avy-kill-whole-line)
        ("k r" . avy-kill-region)
        ("C-s" . avy-goto-char-timer)))

(use-package crux
  :bind
  (:map global-map
        ("C-o" . crux-smart-open-line)
        ("C-q" . crux-smart-open-line-above)))

;; Trim whitespace on save
(use-package whitespace-cleanup-mode
  :init
  (global-whitespace-cleanup-mode 1))

;; Edit in/around delimiter pairs, e.g. (), [], and ""
(use-package surround
  :bind
  (:map ctl-j-map
        ("TAB" . surround-kill-inner)
        ("C-a" . surround-kill-outer)
        ("C-w" . surround-mark-inner)
        ("C-r" . surround-change)))

(provide 'init-text-editing)
;;; init-text-editing.el ends here
