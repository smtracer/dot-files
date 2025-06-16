;;; init-misc.el ---
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/misc" user-emacs-directory))
(require 'init-builtin-misc)
(require 'init-ace-window)

(use-package consult
  :ensure t
  :config
  (setq consult-async-min-input 2)
  :bind
  (:map global-map
        ("C-x b" . consult-buffer)
        ("C-x 4 b" . consult-buffer-other-window)
        ("C-x p b" . consult-project-buffer)
        ("M-y" . consult-yank-from-kill-ring))
  (:map user-overlay-prefix-map
        ("C-c m" . consult-man)
        ("p g" . consult-grep)))

(provide 'init-misc)
;;; init-misc.el ends here
