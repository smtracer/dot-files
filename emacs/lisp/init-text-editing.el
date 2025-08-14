;;; init-text-editing.el --- Text editing, auto-completion & cursor movement.
;;; Commentary:
;;; Code:

;; => Builtins

(define-key global-map (kbd "C-k") #'kill-whole-line)
;; Replace the default binding of 'zap-to-char' with 'zap-up-to-char'.
(define-key global-map (kbd "M-z") #'zap-up-to-char)
(define-key user-overlay-map (kbd "C-f") #'scroll-other-window)
(define-key user-overlay-map (kbd "C-v") #'scroll-other-window-down)

;; => Third-party packages

(use-package avy
  :bind
  (:map user-overlay-map
        ("C-l" . #'avy-goto-line)
        ("k l" . #'avy-kill-whole-line)
        ("k r" . #'avy-kill-region)
        ("C-s" . #'avy-goto-char-timer)))

(use-package company
  :hook (emacs-startup . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1))

(use-package crux
  :bind
  (:map global-map
        ("C-o" . crux-smart-open-line)
        ("C-q" . crux-smart-open-line-above)))

(use-package surround
  :bind
  (:map user-overlay-map
        ("TAB" . surround-kill-inner)
        ("C-a" . surround-kill-outer)
        ("C-w" . surround-mark-inner)
        ("C-r" . surround-change)))

(use-package whitespace-cleanup-mode
  :hook ((prog-mode . whitespace-cleanup-mode)
         (org-mode . whitespace-cleanup-mode)))

(provide 'init-text-editing)
;;; init-text-editing.el ends here.
