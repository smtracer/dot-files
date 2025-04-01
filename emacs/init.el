;;; init.el --- User configuration entrypoint.
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'bootstrap)

(require 'init-overlay)
(require 'init-theme)
(require 'init-emacs)
(require 'init-org)
(require 'init-text-editing)
(require 'init-programming)
(require 'init-completion)

(provide 'init)
;;; init.el ends here
