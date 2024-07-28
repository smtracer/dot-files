;; TODO Garbage collection
;; (setq gc-cons-threshold most-positive-fixnum)
;; (add-hook 'emacs-startup-hook (lambda() (setq gc-cons-threshold 100000000))) ; 100Mb

;; -----------------------------------------------------------------------------
;; Package Management
;; -----------------------------------------------------------------------------

;; TODO: compare load time here vs early-init
;; Disable the builtin package manager, 'package.el'
(setq package-enable-at-startup nil)

;; Install 'straight.el'
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

(when (version< emacs-version "29.1")
    (straight-use-package 'use-package))
;; Configure 'use-package' to use 'straight.el'.
(setq straight-use-package-by-default 't) 

;; -----------------------------------------------------------------------------
;; Custom Keymaps
;; -----------------------------------------------------------------------------

(defvar user-overlay-mode-map (make-sparse-keymap)
  "Keymap for user-overlay mode.")
(define-minor-mode user-overlay-mode
  "Global minor mode that provides a globally available configuration layer."
  :init-value t
  :global t
  :keymap user-overlay-mode-map)
;; HACK: Misusing 'emulation-mode-map-alists' to prioritize keys in
;; 'user-overlay-mode-map'.
(add-to-list 'emulation-mode-map-alists
             `((user-overlay-mode-map . ,user-overlay-mode)))
(defvar ctl-j-map (make-sparse-keymap)
  "User-level parallel to 'ctl-x-map'.")
(define-key user-overlay-mode-map (kbd "C-j") ctl-j-map)

;; -----------------------------------------------------------------------------
;; Builtins
;; -----------------------------------------------------------------------------

(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
(set-display-table-slot standard-display-table 0 ?\ )

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 2
   kept-old-versions 4
   version-control t)       ; use versioned backups

(setq completion-styles '(basic substring partial-completion flex))

(define-key ctl-j-map (kbd "C-k") 'kill-current-buffer)
(define-key ctl-j-map (kbd "0") 'kill-buffer-and-window)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(electric-pair-mode 1)

(winner-mode 1)
(define-key ctl-j-map (kbd "C-p") 'winner-undo)
(define-key ctl-j-map (kbd "C-n") 'winner-redo)

(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "M-;") 'comment-line)

(define-key ctl-j-map (kbd "f") 'zap-to-char)
(define-key ctl-j-map (kbd "t") 'zap-up-to-char)

(setq-default fill-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Keep cursor at start of search result
(defun endless/goto-match-beginning ()
  "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
  (when (and (not (eq this-command 'isearch-exit))
         isearch-forward
             (number-or-marker-p isearch-other-end)
             (not mark-active)
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))
(add-hook 'isearch-update-post-hook
          #'endless/goto-match-beginning)

(use-package org
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-directory "~/org"
	org-agenda-files '("~/org")
	org-default-notes-file (concat org-directory "/notes.org")
	org-cycle-separator-lines 1
	org-fold-catch-invisible-edits "show-and-error"
	org-todo-keywords '((sequence "TODO" "WIP" "BUILD" "REVIEW"
				      "DEPLOY" "|" "DONE" "CANCELED")))
  :bind
  (:map ctl-j-map
	("o a" . org-agenda)
	("o c" . org-capture)
	;; Break with the 'o-*' keybind pattern to mirror the default
	;; 'org-clock-*' function keybinds in 'org-mode', replacing
	;; prefix 'C-c' with 'C-j' for global access.
	("C-x C-j" . org-clock-goto)
	("C-x C-i" . org-clock-in)
	("C-x C-o" . org-clock-out)))

;; -----------------------------------------------------------------------------
;; Additional Packages
;; -----------------------------------------------------------------------------

(use-package ace-window
  :bind
  (:map global-map
	("C-x o" . ace-window)))

(use-package avy
  :after (embark)
  :bind
  (:map ctl-j-map
	("C-s" . 'avy-goto-char-timer)
	("C-l" . 'avy-goto-line)
	("c l" . 'avy-copy-line)
	("c r" . 'avy-copy-region)
	("k l" . 'avy-kill-whole-line)
	("k r" . 'avy-kill-region))
  :config
  (define-key isearch-mode-map (kbd "C-j") 'avy-isearch)
  (setq avy-single-candidate-jump nil)  
  (defun avy-action-embark (pt)
    (unwind-protect
	(save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package centaur-tabs
  :hook
  (emacs-startup . centaur-tabs-mode)
  (compilation-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
  (flymake-project-diagnostics-mode . centaur-tabs-local-mode)
  (flymake-diagnostics-buffer-mode . centaur-tabs-local-mode)
  (fundamental-mode . centaur-tabs-local-mode)
  ;; (lisp-interaction-mode . centaur-tabs-local-mode)
  ;; (messages-buffer-mode . centaur-tabs-local-mode)
  (org-mode . centaur-tabs-local-mode)
  :config
  (defun centaur-tabs-buffer-groups-project ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((memq major-mode '(org-mode org-agenda-mode diary-mode))
       "OrgMode")
      ((memq major-mode '(messages-buffer-mode lisp-interaction-mode dired-mode fundamental-mode special-mode))
       "Emacs")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  (setq centaur-tabs-ace-jump-keys '(?a ?s ?d ?f ?j ?k ?l)
        centaur-tabs-buffer-groups-function 'centaur-tabs-buffer-groups-project
        centaur-tabs-gray-out-icons t
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-plain-icons t
        centaur-tabs-set-close-button nil
        centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button nil)
  :bind
  (:map global-map
        ("C-c t t" . centaur-tabs-toggle-groups)
        ("C-c t o" . centaur-tabs-ace-jump)))

(use-package change-inner
  :bind
  (:map ctl-j-map
	("C-a" . change-outer)
	("TAB" . change-inner)))

(use-package company
  :hook ((conf-mode org-mode prog-mode) . company-mode)
  :config
  (setq company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t))

(use-package consult
  :bind
  (:map global-map
	("C-x b" . consult-buffer)
	("C-x p b" . consult-project-buffer)))

(use-package crux
  :demand t
  :bind
  (:map global-map
	("C-q" . 'crux-smart-open-line-above)
	("C-o" . 'crux-smart-open-line)))

(use-package diff-hl
  :hook
  (prog-mode . diff-hl-mode)
  (prog-mode . diff-hl-margin-mode)
  :config
  (setq diff-hl-show-staged-changes nil)
  :bind
  (:map ctl-j-map
        ("v a" . diff-hl-stage-current-hunk)
        ("v k" . diff-hl-revert-hunk)
        ("v p" . diff-hl-show-hunk-previous)
        ("v n" . diff-hl-show-hunk-next)))

(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package embark
  :demand t
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
		 (window-parameters (mode-line-format . none))
                 (window-parameters (header-line-format . none))))
  :bind
  (:map global-map
	("M-." . 'embark-act)))

(use-package hl-todo
  :init
  (global-hl-todo-mode 1))

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package marginalia
  :hook (emacs-startup . marginalia-mode))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :after
  (nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nordic-vein-theme
  :straight
  (:type git :repo "https://github.com/smtracer/nordic-vein")
  :init
  (load-theme 'nordic-vein t))

(use-package org-modern
  :init
  (with-eval-after-load 'org (global-org-modern-mode)))

(use-package powerline
  :init
  (powerline-center-theme))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-count 5))

(use-package whitespace-cleanup-mode
  :hook (emacs-startup . global-whitespace-cleanup-mode))   

(use-package workgroups2
  :bind
  (:map ctl-j-map
	("r SPC" . wg-create-workgroup)
	("r j" . wg-open-workgroup)
	("r k" . wg-kill-workgroup)))
