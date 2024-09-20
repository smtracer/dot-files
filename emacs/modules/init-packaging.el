;;; init-packaging.el --- Initialize elisp package management.
;;; Commentary:
;;; Code:

;; Disable 'package.el', Emacs' default packaging system.
;; If possible, this should be done in 'early-init.el' to prevent, rather than
;; disable, loading of packages.
;; (setq package-enable-at-startup nil)

;; Install 'straight.el', an alternative git-based packaging system.
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

;; 'use-package.el' is included in Emacs v29.1+.
(when (version< emacs-version "29.1")
  (straight-use-package 'use-package))
;; Remove the need to specify :straight in 'use-package' blocks.
(setq straight-use-package-by-default 't)

(provide 'init-packaging)
;;; init-packaging.el ends here
