;;; init-emacs-misc.el --- Configure Emacs builtins
;;; Commentary:
;;; Code:

(setq backup-directory-alist '(("." . "~/.emacs.d/.saves"))
      completion-styles '(basic substring partial-completion flex)
      create-lockfiles nil
      delete-old-versions t
      fill-column 80)

(define-key ctl-j-map (kbd "C-f") 'scroll-other-window)
(define-key ctl-j-map (kbd "C-v") 'scroll-other-window-down)
(define-key ctl-j-map (kbd "C-p") 'winner-undo)
(define-key ctl-j-map (kbd "C-n") 'winner-redo)

(winner-mode t)

(use-package dired
  :straight (:type built-in)
  :bind
  (:map dired-mode-map
        ("p" . dired-up-directory)))

(provide 'init-emacs-misc)
;;; init-emacs-misc.el ends here
