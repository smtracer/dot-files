;;; core-minibuffer.el --- Minibuffer configuration.
;;; Commentary:
;;; Code:

;; Minibuffer completion
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-count 5))

;; Documentation in the minibuffer
(use-package marginalia
  :hook (emacs-startup . marginalia-mode))

(provide 'core-minibuffer)
;;; core-minibuffer.el ends here
