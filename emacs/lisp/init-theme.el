;;; init-theme.el ---
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/theme" user-emacs-directory))
(require 'init-builtin-theme)
(require 'init-nordic-vein-theme)
(require 'init-doom-modeline)
(require 'init-nerd-icons)

(provide 'init-theme)
;;; init-theme.el ends here
