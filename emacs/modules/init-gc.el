;;; init-gc.el --- Garbage collection tuning.
;;; Commentary:
;;; Code:

;; Gc advice to prefer gcs when idle.
(use-package gcmh
  :init
  (gcmh-mode 1))

(provide 'init-gc)
;;; init-gc.el ends here.
