;;; core-emacs.el --- Configure Emacs builtins
;;; Commentary:
;;; Code:

(setq backup-directory-alist '(("." . "~/.emacs.d/.saves"))
      completion-styles '(basic substring partial-completion flex)
      create-lockfiles nil
      delete-old-versions t
      fill-column 80)

(winner-mode t)

(define-key ctl-j-map (kbd "C-f") 'scroll-other-window)
(define-key ctl-j-map (kbd "C-v") 'scroll-other-window-down)
(define-key ctl-j-map (kbd "C-p") 'winner-undo)
(define-key ctl-j-map (kbd "C-n") 'winner-redo)

(provide 'core-emacs)
;;; core-emacs.el ends here
