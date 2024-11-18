;;; init-user-overlay.el ---
;;; Commentary:
;;; Code:

(defvar user-overlay-mode-map (make-sparse-keymap))
(define-minor-mode user-overlay-mode
  "Global minor mode that provides a globally available configuration layer."
  :init-value t
  :global t
  :keymap user-overlay-mode-map)
(add-to-list 'emulation-mode-map-alists
             `((user-overlay-mode-map . ,user-overlay-mode)))
(defvar ctl-j-map (make-sparse-keymap))
(define-key user-overlay-mode-map (kbd "C-j") ctl-j-map)

(define-key ctl-j-map (kbd "R") 'rename-visited-file)

(provide 'init-user-overlay)
;;; init-user-overlay.el ends here
