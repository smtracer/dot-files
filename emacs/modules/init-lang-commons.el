;;; init-lang-commons.el ---
;;; Commentary:
;;;
;;; Examples include 'lsp-mode' and 'flymake'. Both are useful to enable for
;;; languages when possible, and as a result, appear in most *init-lang-*.el
;;; configurations.
;;;
;;; Configuration in this file should make sure that packages are loaded before
;;; they're required by specific *init-lang-*.el files. Consequently, language
;;; specific configuration of shared packages will generally avoid force loading
;;; packages configured here (e.g. by autoloading required functions, or using
;;; use-package specific macros like :config and :bind).
;;;
;;; Code:

(defun ext/prog-like-mode-defaults ()
  (display-fill-column-indicator-mode t)
  (display-line-numbers-mode t))

(add-hook 'conf-mode-hook 'ext/prog-like-mode-defaults)
(add-hook 'prog-mode-hook 'ext/prog-like-mode-defaults)

(use-package company
  :init
  (setq company-minimum-prefix-length 3
        company-idle-delay 0.2))

(use-package flymake
  :straight (:type built-in)
  :bind
  (:map ctl-j-map
        ("e n" . flymake-goto-next-error)
        ("e p" . flymake-goto-prev-error)
        ("e l" . flymake-show-buffer-diagnostics)))

(use-package hl-todo
  :init
  (setq hl-todo-keyword-faces '(("TODO" . warning)
                               ("HACK" . warning)
                               ("BUG" . error)
                               ("FIXME" . error)))
  (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-j l"
        ;; Suggested by 'lsp-doctor'
        read-process-output-max (* 1024 1024)
        lsp-lens-auto-enable nil
        lsp-auto-execute-action nil
        lsp-modeline-code-actions-enable t
        ;; Nerd fonts for terminal dwellers
        lsp-modeline-code-action-fallback-icon ""
        lsp-progress-prefix " "))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-save-repository-buffers 'dontask)
  :bind
  (:map ctl-j-map
        ("g c" . magit-branch-create)))

(use-package treesit
  :straight (:type built-in)
  :init
  (setq treesit-font-lock-level 4))

(use-package which-key
  :init
  (which-key-mode 1))

(provide 'init-lang-commons)
;;; init-lang-commons.el ends here.