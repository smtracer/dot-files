;;; init-emacs-misc --- Things that don't warrant their own files.
;;; files.
;;; Commentary:
;;; Code:

(setq backup-directory-alist '(("." . "~/.emacs.d/saves"))
      confirm-kill-processes nil
      create-lockfiles nil
      delete-old-versions t
      find-file-visit-truename t ; Follow symlinks - required for treemacs project follow to work with symlinks
      process-query-on-exit-flag nil)

;; Provides undo/redo for the window configuration.
(winner-mode 1)
(define-key user-overlay-map (kbd "C-p") #'winner-undo)
(define-key user-overlay-map (kbd "C-n") #'winner-redo)

(with-eval-after-load 'dired
  (setq dired-listing-switches "-alh --group-directories-first")
  (define-key dired-mode-map (kbd "p") #'dired-up-directory))

(define-key user-overlay-map (kbd "C-k") #'kill-current-buffer)
(define-key user-overlay-map (kbd ";") #'point-to-register)
(define-key user-overlay-map (kbd "'") #'jump-to-register)


(add-hook 'isearch-mode-end-hook (lambda ()
                                   (when (and isearch-forward
                                              (number-or-marker-p isearch-other-end)
                                              ;; (not mark-active)
                                              (not isearch-mode-end-hook-quit))
                                     (goto-char isearch-other-end))))

;; => Third-party packages

(use-package ace-window
  :config
  ;; Treemacs should not be select-able as a regular buffer
  (add-to-list 'aw-ignored-buffers "^\\*Treemacs\\*")
  :bind
  (:map global-map
        ("C-x o" . ace-window)))

(use-package consult
  :bind
  (:map global-map
        ("C-x b" . consult-buffer)
        ("C-x p b" . consult-project-buffer)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-j C-j"))
  :init
  (persp-mode)
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source))
  (customize-set-variable 'even-window-sizes nil))     ; avoid resizing


(provide 'init-emacs-misc)
;;; init-emacs-misc.el ends here.
