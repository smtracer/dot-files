;;; init-minibuffer.el - The minibuffer.
;;; Commentary:
;;; Code:

;; Display documentation in the margin.
(use-package marginalia
  :hook
  (emacs-startup . marginalia-mode))

;; Modern auto-completion in the minibuffer.
(use-package vertico
  :hook
  (emacs-startup . vertico-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here.
