;;; core-programming.el --- Configure programming environments.
;;; Commentary:
;;; Code:

(defun default-programming-hooks ()
  (display-fill-column-indicator-mode)
  (display-line-numbers-mode))

(add-hook 'conf-mode-hook 'default-programming-hooks)
(add-hook 'prog-mode-hook 'default-programming-hooks)

(use-package hl-todo
  :init
  (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; Language Configuration -------------------------------------------------------

;; c
;; c++
;; emacs lisp
;; python
;; shell
;; ruby
;; rust
;; typescript

(provide 'core-programming)
;;; core-programming.el ends here
