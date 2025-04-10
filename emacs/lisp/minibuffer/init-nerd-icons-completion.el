;;; init-nerd-icons-completion.el ---
;;; Commentary:
;;; Code:

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(provide 'init-nerd-icons-completion)
;;; init-nerd-icons-completion.el ends here
