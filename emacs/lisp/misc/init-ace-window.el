;;; init-ace-window.el ---
;;; Commentary:
;;; Code:

(use-package ace-window
  :config
  (setq aw-keys '(?f ?j ?k ?l))
  :bind
  (:map ctl-x-map
        ("o" . ace-window)))

(provide 'init-ace-window)
;;; init-ace-window.el ends here
