;;; init-minibuffer.el ---
;;; Commentary:
;;; Code:

;; Minibuffer autocompletion, with documentation in the margin.
(use-package vertico :hook (emacs-startup . vertico-mode))
(use-package marginalia :hook (emacs-startup . marginalia-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here.
