;;; init-programming.el --- Programming-related configuration.
;;; Commentary:
;;; Code:

;; => Builtins

(setq-default compilation-scroll-output 'first-error
              fill-column 80
              indent-tabs-mode nil
              tab-width 4)

(setq backup-directory-alist '(("." . "~/.emacs.d/saves"))
      confirm-kill-processes nil
      create-lockfiles nil
      delete-old-versions t
      process-query-on-exit-flag nil)

(electric-pair-mode 1)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-fill-column-indicator-mode )
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(define-key global-map (kbd "M-;") #'comment-line)

(use-package flymake
  :straight (:type built-in)
  :config
  (setq flymake-margin-indicator-position nil)
  :bind
  (:map user-overlay-map
        ("e n" . flymake-goto-next-error)
        ("e p" . flymake-goto-prev-error)))

;; Tree-sitter is a code parsing library that provides high-granularity,
;; accurate syntax highlighting using abstract syntax trees maintained according
;; to a particular language grammar.
;;
;; Emacs 29+ can be built with treesitter support ('treesit.el') using the
;; '--with-tree-sitter' flag, as is assumed here.
(use-package treesit
  :straight (:type built-in)
  :config
  (setq treesit-font-lock-level 4
        ;; Configures the sources for 'treesit-install-language-grammar'
        treesit-language-source-alist '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                                        (c "https://github.com/tree-sitter/tree-sitter-c")
                                        (cmake "https://github.com/uyha/tree-sitter-cmake")
                                        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                        (java "https://github.com/tree-sitter/tree-sitter-java")
                                        (json "https://github.com/tree-sitter/tree-sitter-json")
                                        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                                        (python "https://github.com/tree-sitter/tree-sitter-python")
                                        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
                                        (rust "https://github.com/tree-sitter/tree-sitter-rust")
                                        (toml "https://github.com/tree-sitter/tree-sitter-toml")
                                        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (defun install-missing-treesit-grammars ()
    "Installs missing grammars from 'treesit-language-source-alist'."
    (dolist (source treesit-language-source-alist)
      (unless (treesit-ready-p (car source))
        (treesit-install-language-grammar (car source)))))
  ;; NOTE: 'emacs-startup-hook' doesn't work correctly in the case where Emacs
  ;; is opened directly to a treesitter mode for which the grammar is missing.
  ;; In that case, the treesitter mode is invoked, fails to load due to a
  ;; missing grammar, _and then_ the grammar is installed. That only happens
  ;; once the first time for each language, if at all, and then is forever
  ;; fixed unless if the grammar is removed from the machine.
  (add-hook 'after-init-hook 'install-missing-treesit-grammars))

;; => Third-party packages

(use-package diff-hl
  :init
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  :config
  (setq diff-hl-show-staged-changes nil)
  :bind
  (:map user-overlay-map
        ("v a" . diff-hl-stage-current-hunk)
        ("v k" . diff-hl-revert-hunk)
        ("v n" . diff-hl-next-hunk)
        ("v p" . diff-hl-previous-hunk)))

(use-package hl-todo
  :hook
  ((prog-mode . hl-todo-mode)
   (conf-mode . hl-todo-mode))
  :config
  (setq hl-todo-keyword-faces '(("HACK" . warning)
                                ("Note" . ansi-color-blue)
                                ("NOTE" . ansi-color-blue)
                                ("TODO" . warning)
                                ("BUG" . error)
                                ("FIXME" . error))))

(use-package lsp-mode
  :defer t
  :init
  (setq lsp-keymap-prefix "C-j ;")
  :config
  (setq lsp-modeline-code-actions-enable t
        lsp-progress-prefix " "
        lsp-modeline-code-action-fallback-icon ""
        ;; Suggested by 'lsp-doctor'
        read-process-output-max (* 1024 1024)
        lsp-lens-auto-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-signature-render-documentation nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 0.5
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-diagnostics t))

(use-package magit
  :commands magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-save-repository-buffers 'dontask)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package treemacs
  :config
  (treemacs-project-follow-mode 1)
  :bind
  (:map user-overlay-map/project
        ("t" . treemacs)))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-nerd-icons
  :after (treemacs)
  :config
  (treemacs-load-theme "nerd-icons"))

;; => Additional modules

(add-to-list 'load-path (expand-file-name "lisp/programming" user-emacs-directory))
;; Config / Markdown setup
(require 'init-lang-json)
(require 'init-lang-toml)
(require 'init-lang-yaml)
;; Programming language setup
(require 'init-lang-bash)
(require 'init-lang-c)
(require 'init-lang-cpp)
(require 'init-lang-elisp)
(require 'init-lang-python)
(require 'init-lang-ruby)
(require 'init-lang-rust)
(require 'init-lang-typescript)

(provide 'init-programming)
;;; init-programming.el ends here
