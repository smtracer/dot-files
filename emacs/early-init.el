;;; early-init.el --- Emacs pre-configuration.
;;; Commentary:
;;
;; From the 'Info' page for the init file:
;;   "The difference between the early init file and the regular init file is
;;   that the early init file is loaded much earlier during the startup process,
;;   so you can use it to customize some things that are initialized before
;;   loading the regular init file.  For example, you can customize the process
;;   of initializing the package system, by setting variables such as
;;   PACKAGE-LOAD-LIST or PACKAGE-ENABLE-AT-STARTUP."
;;
;;; Code:

;; 'lsp-mode' performance optimization:
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")
(setq inhibit-splash-screen t        ; Open to *scratch* if no file is selected.
      package-enable-at-startup nil) ; Disable 'package.el', Emacs' default
                                     ; packaging system.

(provide 'early-init)
;;; early-init.el ends here
