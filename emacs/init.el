;;; init.el --- Emacs configuration.
;;; Commentary:
;;
;; This init file does two things:
;; - Initialize a bare minimum starting point for further configuration
;; - Load additional config
;;
;; The bare minimum starting point is:
;; 1. A Lisp package manager integrated with 'use-package.el' ('straight.el')
;; 2. A global minor mode for user configuration ('user-overlay-mode'), with
;;    a prefix key ('user-overlay-map').
;;
;; The bulk of the config is roughly grouped by functionality in 'lisp/'.
;; Additionally, elisp files in '~/.config/emacs' are considered as user
;; specific config, and are loaded last to allow local overrides.
;;
;;; Code:

;; Bootstrap the 'straight.el' package manager.
;; https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install 'use-package.el' ('use-package.el' is included in Emacs v29.1+)
(when (version< emacs-version "29.1")
  (straight-use-package 'use-package))
;; Remove the need to specify :straight as the package manager in 'use-package'
;; definitions.
(setq straight-use-package-by-default 't)

;; Setup a user configuration layer, with a globally available prefix "C-j".
(defvar user-overlay-mode-map (make-sparse-keymap))
(define-key user-overlay-mode-map (kbd "C-j") 'user-overlay-map)
(define-prefix-command 'user-overlay-map)
(define-minor-mode user-overlay-mode
  "A global, configurable user layer."
  :init-value t
  :global t
  :keymap user-overlay-mode-map)
(define-prefix-command 'user-overlay-map/project)
(define-key user-overlay-map (kbd "p") user-overlay-map/project)

;; Add the files in 'lisp/' to the load path & then load specific features.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-emacs-misc)
(require 'init-minibuffer)
(require 'init-org)
(require 'init-programming)
(require 'init-text-editing)
(require 'init-ui)

;; Load machine/user local elisp config.
(let ((local-config-dir "~/.config/emacs/"))
  (dolist (local-config-file (directory-files-recursively local-config-dir "\\.el$"))
    (when (file-regular-p local-config-file)
      (load-file local-config-file))))

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-variable-name-face ((t (:inherit ansi-color-blue :foreground "#7aa2f7")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
