;;; init-misc.el ---
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/misc" user-emacs-directory))
(require 'init-builtin-misc)
(require 'init-ace-window)
(require 'init-dirvish)

(provide 'init-misc)
;;; init-misc.el ends here
