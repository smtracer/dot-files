;;; init-overlay.el --- A globally available layer on top of the base emacs.
;;; Commentary:
;;; Code:

(defvar overlay-mode-map (make-sparse-keymap))
(define-minor-mode overlay-mode
  "Global minor mode that provides a user configuration layer."
  :init-value t
  :global t
  :keymap overlay-mode-map)
(add-to-list 'emulation-mode-map-alists
             `((overlay-mode-map . ,overlay-mode)))
(defvar ctl-j-map (make-sparse-keymap))
(define-key overlay-mode-map (kbd "C-j") ctl-j-map)

(provide 'init-overlay)
;;; init-overlay.el ends here
