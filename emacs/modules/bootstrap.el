;;; bootstrap.el ---
;;; Commentary:
;;; Code:

;; Install 'straight.el', an alternative package manager. Packages are installed
;; locally as git repos and built from the local repo.
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
;; blocks.
(setq straight-use-package-by-default 't)

;; Garbage collection "advice" to prefer gcs when idle.
(use-package gcmh
  :init
  (gcmh-mode 1))

(provide 'bootstrap)
;;; bootstrap.el ends here
