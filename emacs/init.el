;;; init.el --- Emacs user configuration.
;;; Commentary:
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

;; Setup a user configuration layer.
(define-prefix-command 'user-overlay-map)
(defvar user-overlay-mode-map (make-sparse-keymap))
(define-key user-overlay-mode-map (kbd "C-j") 'user-overlay-map)
(define-minor-mode user-overlay-mode
  "A global configurable user layer that sits on top of builtin settings."
  :init-value t ; enabled by default
  :global t
  :keymap user-overlay-mode-map)

(setq-default indent-tabs-mode nil
              tab-width 4)

(electric-pair-mode 1)

(define-key global-map (kbd "M-;") #'comment-line)
(define-key global-map (kbd "C-k") #'kill-whole-line)

(use-package avy
  :bind
  (:map user-overlay-map
        ("C-l" . #'avy-goto-line)
        ("k l" . #'avy-kill-whole-line)
        ("k r" . #'avy-kill-region)
        ("C-s" . #'avy-goto-char-timer)))

;; TODO: try corfu
(use-package company
  :hook (emacs-startup . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1))

;; TODO: Just write these myself
(use-package crux
  :bind
  (:map global-map
        ("C-o" . crux-smart-open-line)
        ("C-q" . crux-smart-open-line-above)))

(use-package surround
  :bind
  (:map user-overlay-map
        ("TAB" . surround-kill-inner)
        ("C-a" . surround-kill-outer)
        ("C-w" . surround-mark-inner)
        ("C-r" . surround-change)))

(use-package whitespace-cleanup-mode
  :hook ((prog-mode . whitespace-cleanup-mode)
         (org-mode . whitespace-cleanup-mode)))

(use-package vertico
  :hook
  (emacs-startup . vertico-mode))

(use-package marginalia
  :hook
  (emacs-startup . marginalia-mode))

(setq-default fill-column 80)
(setq process-query-on-exit-flag nil
      create-lockfiles nil
      delete-old-versions t
      confirm-kill-processes nil
      compilation-scroll-output 'first-error
      backup-directory-alist '(("." . "~/.emacs.d/saves")))

(winner-mode 1)
(define-key user-overlay-map (kbd "C-p") #'winner-undo)
(define-key user-overlay-map (kbd "C-n") #'winner-redo)

(setq dired-listing-switches "-alh --group-directories-first")
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "p") #'dired-up-directory))

(menu-bar-mode -1)
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(use-package nordic-vein-theme
  :straight (:type git :repo "https://github.com/smtracer/nordic-vein-theme")
  :init
  (load-theme 'nordic-vein t))

(use-package doom-modeline
  :hook ((emacs-startup-hook . doom-modeline-mode)))

;; (use-package nerd-icons
;;   :defer t)

;; PROGRAMMING

(use-package magit
  :commands magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-save-repository-buffers 'dontask))

(use-package hl-todo
  :hook
  ((prog-mode . hl-todo-mode)
   (conf-mode . hl-todo-mode))
  :config
  (setq hl-todo-keyword-faces '(("HACK" . warning)
                                ("Note" . warning)
                                ("NOTE" . warning)
                                ("TODO" . warning)
                                ("BUG" . error)
                                ("FIXME" . error))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-programming)
(require 'mortar)

;; Load extended local initialization files.
(let ((local-config-dir "~/.config/emacs/"))
  (dolist (local-config-file (directory-files-recursively local-config-dir "\\.el$"))
    (when (file-regular-p local-config-file)
      (load-file local-config-file))))

(provide 'init)
;;; init.el ends here
