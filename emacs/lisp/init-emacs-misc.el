;;; init-emacs-misc --- Lightly configured builtins that don't warrant their own
;;; files.
;;; Commentary:
;;; Code:

(setq backup-directory-alist '(("." . "~/.emacs.d/saves"))
      confirm-kill-processes nil
      create-lockfiles nil
      delete-old-versions t
      process-query-on-exit-flag nil)

;; Provides undo/redo for the window configuration.
(winner-mode 1)
(define-key user-overlay-map (kbd "C-p") #'winner-undo)
(define-key user-overlay-map (kbd "C-n") #'winner-redo)

(with-eval-after-load 'dired
  (setq dired-listing-switches "-alh --group-directories-first")
  (define-key dired-mode-map (kbd "p") #'dired-up-directory))

(provide 'init-emacs-misc)
;;; init-emacs-misc.el ends here.
