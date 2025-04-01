;;; init-emacs.el --- Configure Emacs builtins
;;; Commentary:
;;; Code:

(setq-default fill-column 80)
(setq backup-directory-alist '(("." . "~/.emacs.d/.saves"))
      completion-styles '(basic substring partial-completion flex)
      create-lockfiles nil
      delete-old-versions t)

(define-key ctl-j-map (kbd "C-f") 'scroll-other-window)
(define-key ctl-j-map (kbd "C-v") 'scroll-other-window-down)

(use-package winner
  :straight
  (:type built-in)
  :init
  (winner-mode t)
  :bind
  (:map ctl-j-map
        ("C-p" . winner-undo)
        ("C-n" . winner-redo)))

(use-package dired
  :straight
  (:type built-in)
  :bind
  (:map dired-mode-map
        ("p" . dired-up-directory)))

(provide 'init-emacs)
;;; init-emacs.el ends here
