;;; init-completion.el ---
;;; Commentary:
;;; Code:

(use-package vertico
  :hook
  (emacs-startup . vertico-mode))

(use-package marginalia
  :hook
  (emacs-startup . marginalia-mode))

(use-package company
  :init
  :hook
  (emacs-startup . global-company-mode))

(provide 'init-completion)
;;; init-completion.el ends here
