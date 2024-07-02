;;; init-ui.el ---
;;; Commentary:
;;; Code:

(menu-bar-mode -1)
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
(set-display-table-slot standard-display-table 0 ?\ )


(use-package nordic-vein-theme
  :straight
  (:type git :repo "https://github.com/smtracer/nordic-vein")
  :init
  (load-theme 'nordic-vein t))

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 2
   kept-old-versions 4
   version-control t)       ; use versioned backups


(use-package nerd-icons)

(use-package nerd-icons-dired
  :after
  (nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

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

(use-package treemacs
  :init
  (treemacs-project-follow-mode t))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

;; (use-package solarized-theme
;;   :init
;;   (load-theme 'solarized-selenized-dark t))

(defface mol
  '((((class color) (min-colors 88))
     :background "#23262f"
     :foreground "#b9f5c9")
    )
  "fldsfjdkl"
  :group 'basic-faces)
  ;; '((((class color) (min-colors 88) (background light))
  ;;    :background "darkseagreen2")
  ;;   (((class color) (min-colors 88) (background dark))
  ;;    :background "darkolivegreen")
  ;;   (((class color) (min-colors 16) (background light))
  ;;    :background "darkseagreen2")
  ;;   (((class color) (min-colors 16) (background dark))
  ;;    :background "darkolivegreen")
  ;;   (((class color) (min-colors 8))
  ;;    :background "green" :foreground "black")
  ;;   (t :inverse-video t))
  ;; "Basic face for highlighting."
  ;; :group 'basic-faces)
(use-package powerline
  :init
  (powerline-center-theme))
;; (setq mode-line-format
;;       '("%e"
;;  (:eval
;;   (let*
;;       ((active
;; 	(powerline-selected-window-active))
;;        (mode-line-buffer-id
;; 	(if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
;;        (mode-line
;; 	(if active 'mode-line 'mode-line-inactive))
;;        (face0
;; 	(if active 'powerline-active0 'powerline-inactive0))
;;        (vc-face
;; 	(if active 'mol 'powerline-inactive1))
;;        (face1
;; 	(if active 'powerline-active1 'powerline-inactive1))
;;        (face2
;; 	(if active 'powerline-active2 'powerline-inactive2))
;;        (separator-left
;; 	(intern
;; 	 (format "powerline-%s-%s"
;; 		 (powerline-current-separator)
;; 		 (car powerline-default-separator-dir))))
;;        (separator-right
;; 	(intern
;; 	 (format "powerline-%s-%s"
;; 		 (powerline-current-separator)
;; 		 (cdr powerline-default-separator-dir))))
;;        (lhs
;; 	(list
;; 	 (powerline-raw "%*" face0 'l)
;; 	 (when powerline-display-buffer-size
;; 	   (powerline-buffer-size face0 'l))
;; 	 (powerline-buffer-id
;; 	  `(mode-line-buffer-id ,face0)
;; 	  'l)
;; 	 (powerline-raw " " face0)
;; 	 (funcall separator-left face0 face1)
;; 	 (powerline-narrow face1 'l)
;; 	 (powerline-vc vc-face)))
;;        (rhs
;; 	(list
;; 	 (powerline-raw global-mode-string face1 'r)
;; 	 (powerline-raw "%4l" face1 'r)
;; 	 (powerline-raw ":" face1)
;; 	 (powerline-raw "%3c" face1 'r)
;; 	 (funcall separator-right face1 face0)
;; 	 (powerline-raw " " face0)
;; 	 (powerline-raw "%6p" face0 'r)
;; 	 (when powerline-display-hud
;; 	   (powerline-hud face2 face1))
;; 	 (powerline-fill face0 0)))
;;        (center
;; 	(list
;; 	 (powerline-raw " " face1)
;; 	 (funcall separator-left face1 face2)
;; 	 (when
;; 	     (and
;; 	      (boundp 'erc-track-minor-mode)
;; 	      erc-track-minor-mode)
;; 	   (powerline-raw erc-modified-channels-object face2 'l))
;; 	 (powerline-major-mode face2 'l)
;; 	 (powerline-process face2)
;; 	 (powerline-raw " :" face2)
;; 	 (powerline-minor-modes face2 'l)
;; 	 (powerline-raw " " face2)
;; 	 (funcall separator-right face2 face1))))
;;     (concat
;;      (powerline-render lhs)
;;      ;; (powerline-fill-center face1
;;      ;; 			    (/
;;      ;; 			     (powerline-width center)
;;      ;; 			     2.0))
;;      ;; (powerline-render center)
;;      (powerline-fill face1
;; 		     (powerline-width rhs))
;;      (powerline-render rhs)))))))
(defun swap-mode-and-header-lines ()
  (let ((mode-line-format-copy mode-line-format))
    (setq-default mode-line-format header-line-format
		  header-line-format mode-line-format-copy)))
;; (swap-mode-and-header-lines)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" default)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(mode-line ((t (:background "#192024" :foreground "#9BA3A7" :box nil))))
;;  '(mode-line-buffer-id ((t (:weight regular))))
;;  '(mode-line-buffer-id-inactive ((t (:foreground "blue"))))
;;  '(powerline-active0 ((t (:inherit mode-line :background "#1e2129" :foreground "#eceef0"))))
;;  '(powerline-active1 ((t (:inherit mode-line :background "#23262f"))))
;;  '(powerline-active2 ((t (:inherit mode-line :background "#3b404a" :foreground "#DFDFDF"))))
;;  '(powerline-inactive0 ((t (:inherit mode-line-inactive :background "#292c38"))))
;;  '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "#2b2f3b"))))
;;  '(vertical-border ((t (:inherit default :background "#2b303b" :foreground "#1c2028")))))

(provide 'init-ui)
;;; init-ui.el ends here.
