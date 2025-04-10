;;; init-editing.el ---
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/editing" user-emacs-directory))
(require 'init-builtin-editing)
(require 'init-avy)
(require 'init-company)
(require 'init-crux)
(require 'init-surround)
(require 'init-whitespace-cleanup-mode)

(provide 'init-editing)
;;; init-editing.el ends here
