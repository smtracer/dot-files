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
  :config
  ;; TODO: Only load this if embark is available
  (defun avy-action-embark (pt)
     (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0)))
      t))
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  (define-key isearch-mode-map (kbd "C-j") 'avy-isearch)
  :bind
  (:map user-overlay-map
        ("C-l" . #'avy-goto-line)
        ("k l" . #'avy-kill-whole-line)
        ("k r" . #'avy-kill-region)
        ("C-s" . #'avy-goto-word-1)))

(use-package company
  :hook (emacs-startup . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1))

(use-package crux
  :bind
  (:map global-map
        ("C-o" . crux-smart-open-line)
        ("C-q" . crux-smart-open-line-above)))

(use-package embark
  :bind
  (:map global-map
        ("M-." . embark-act)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package expand-region
  :bind
  (:map user-overlay-map
        ("C-e" . er/expand-region)))

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
