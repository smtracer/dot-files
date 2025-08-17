;;; init-emacs-misc --- Things that don't warrant their own files.
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

(define-key user-overlay-map (kbd "C-k") #'kill-current-buffer)

;; => Third-party packages

(use-package consult
  :bind
  (:map global-map
        ("C-x b" . consult-buffer)
        ("C-x p b" . consult-project-buffer)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(provide 'init-emacs-misc)
;;; init-emacs-misc.el ends here.
