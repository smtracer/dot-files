;;; init-builtin-misc.el -
;;; Commentary:
;;; Code:

(setq-default fill-column 80
              process-query-on-exit-flag nil)

(setq backup-directory-alist '(("." . "~/.emacs.d/.saves"))
      completion-styles '(basic substring partial-completion flex)
      create-lockfiles nil
      delete-old-versions t
      confirm-kill-processes nil
      compilation-scroll-output 'first-error
      kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(define-key user-overlay-prefix-map (kbd "C-f") 'scroll-other-window)
(define-key user-overlay-prefix-map (kbd "C-v") 'scroll-other-window-down)
(define-key user-overlay-prefix-map (kbd "C-k") 'kill-current-buffer)

(use-package winner
  :straight (:type built-in)
  :hook ((after-init . winner-mode))
  :bind
  (:map user-overlay-prefix-map
        ("C-p" . winner-undo)
        ("C-n" . winner-redo)))

(use-package dired
  :straight (:type built-in)
  :config
  (setq dired-listing-switches "-alh --group-directories-first") ;lAF
  :bind
  (:map dired-mode-map
        ("p" . dired-up-directory)))

(provide 'init-builtin-misc)
;;; init-builtin-misc.el ends here
