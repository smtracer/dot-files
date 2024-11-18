;;; init-compile.el ---
;;; Commentary:
;;; Code:

(use-package compile
  :straight (:type built-in)
  :config
  (setq compilation-scroll-output 'first-error
        compilation-auto-jump-to-first-error 'first-known
        compilation-always-kill t
        compilation-ask-about-save nil
        compilation-buffer-name-function 'ext/project-or-buffer-compilation-buffer-name
        compilation-context-lines t
        next-error-message-highlight t)
  :bind
  (:map ctl-j-map
        ("C-c" . recompile)))

(use-package compile-multi
  :bind
  (:map ctl-j-map
        (";" . compile-multi)))

;; Aesthetic improvement. Applies 'ansi-color-<...>' faces to the compilation
;; buffer.
(use-package ansi-color
  :straight (:type built-in)
  :hook (compilation-filter . ansi-color-compilation-filter))

(defun ext/project-or-buffer-compilation-buffer-name (name-of-mode)
  "Returns a compilation buffer name unique to the current project, when active,
or unique to the invoking-buffer otherwise."
  (let ((compilation-identifier (if (project-current)
                                   (project-name (project-current))
                                 (buffer-name))))
    (concat "*" (downcase compilation-identifier) " compilation*")))

(provide 'init-compile)
;;; init-compile.el ends here
