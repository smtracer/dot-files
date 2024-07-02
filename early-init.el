;;; early-init.el --- User configuration loaded before 'user-init-file'.
;;; Commentary:
;;; Code:

;; --- Garbage Collection:

;; Improve start time by avoiding garbage collection. Set the threshold to
;; something that doesn't introduce noticeable lag afterwards.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda() (setq gc-cons-threshold 100000000))) ; 100Mb

;; --- Package Management:
;; Replace the default package system ('package.el') with 'straight.el'.

;; (setq package-enable-at-startup nil) ; Disable 'package.el'
;; ;; Load 'straight.el'; bootstrap logic taken directly from the 'straight.el'
;; ;; documentation.
;; ;; https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name
;;	"straight/repos/straight.el/bootstrap.el"
;;	(or (bound-and-true-p straight-base-dir)
;;	    user-emacs-directory)))
;;       (bootstrap-version 7))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;	(url-retrieve-synchronously
;;	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;	 'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))
;; ;; Make 'use-package' available if it isn't included by default.
;; (when (version< emacs-version "29.1")
;;     (straight-use-package 'use-package))
;; (setq straight-use-package-by-default 't)

;; --- UI:

(setq inhibit-splash-screen t)
(menu-bar-mode nil)

(provide 'early-init)
