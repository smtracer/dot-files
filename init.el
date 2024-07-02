;;; init.el --- User configuration.
;;; Commentary:
;;; Code:



(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq package-enable-at-startup nil)
(require 'init-user-keymaps)
(require 'init-package-toolchain)
(require 'init-keyboard-pill)

(use-package ace-window
  :bind
  (:map global-map
	("C-x o" . ace-window)))

(require 'init-org)

(define-key ctl-j-map (kbd "k") 'kill-current-buffer)
(define-key ctl-j-map (kbd "0") 'kill-buffer-and-window)

(use-package consult
  :bind
  (:map global-map
	("C-x b" . consult-buffer)
	("C-x p b" . consult-project-buffer)))
(define-key isearch-mode-map (kbd "C-j") 'avy-isearch)

(use-package company
  :hook ((conf-mode org-mode prog-mode) . company-mode)
  :config
  (setq company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t))
(use-package workgroups2
  :bind
  (:map ctl-j-map
	("r SPC" . wg-create-workgroup)
	("r j" . wg-open-workgroup)
	("r k" . wg-kill-workgroup)))

(use-package whitespace-cleanup-mode
  :hook (emacs-startup . global-whitespace-cleanup-mode))   

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

(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; im not sure...

(use-package change-inner
  :bind
  (:map ctl-j-map
	("C-a" . change-outer)
	("TAB" . change-inner)))

(use-package crux
  :demand t
  :bind
  (:map global-map
	("C-q" . 'crux-smart-open-line-above)
	("C-o" . 'crux-smart-open-line)))

;; (use-package vertico
;;   :init
;;   (vertico-mode)
;;   :config
;;   (setq vertico-count 5))

(setq completion-styles '(basic substring partial-completion flex))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :hook (emacs-startup . marginalia-mode))

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

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
(require 'init-ui)
