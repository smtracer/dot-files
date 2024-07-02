;;; init-user-keymaps.el ---
;;; Commentary:
;;; Code:

(defvar user-overlay-mode-map (make-sparse-keymap)
  "Keymap for user-overlay mode.")

(define-minor-mode user-overlay-mode
  "Global minor mode with a goal of providing a globally available (i.e. with

precedence over other modes & packages) user configuration layer."
  :init-value t
  :global t
  :keymap user-overlay-mode-map)

;; HACK: Misusing 'emulation-mode-map-alists' to prioritize keys in
;; 'user-overlay-mode-map'. The documentation says that it's "for modes or
;; packages using multiple minor-mode keymaps", however the emulation alists
;; also take precedence over 'minor-mode-map-alist', meaning keys in it will
;; outrank most other major/minor mode maps.
(add-to-list 'emulation-mode-map-alists
             `((user-overlay-mode-map . ,user-overlay-mode)))

(defvar ctl-j-map (make-sparse-keymap)
  "\"Userspace\" parallel to 'ctl-x-map'.")
(define-key user-overlay-mode-map (kbd "C-j") ctl-j-map)

(provide 'init-user-keymaps)
;;; init-user-keymaps.el ends here
