;;; init.el --- User configuration entrypoint.
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'init-packaging)
(require 'init-theme)
(require 'init-custom-keymaps)

(require 'core-emacs)
(require 'core-minibuffer)
(require 'core-org)
(require 'core-programming)
(require 'core-text-editing)

(provide 'init)
;;; init.el ends here
