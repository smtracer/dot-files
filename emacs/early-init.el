;;; early-init.el --- User pre-configuration entrypoint.
;;; Commentary:
;;; Code:

(setenv "LSP_USE_PLISTS" "true")
(setq inhibit-splash-screen t
      package-enable-at-startup nil) ; Disable 'package.el', Emacs' default
                                     ; packaging system.

(provide 'early-init)
;;; early-init.el ends here
