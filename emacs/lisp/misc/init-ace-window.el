;;; init-ace-window.el ---
;;; Commentary:
;;; Code:

(use-package ace-window
  :config
  (setq aw-keys '(?f ?j ?k ?l)
        aw-ignored-buffers '(dired-mode)) ; TODO: Only ignore dired modes using dirvish
  :bind
  (:map ctl-x-map
        ("o" . ace-window)))

(provide 'init-ace-window)
;;; init-ace-window.el ends here
