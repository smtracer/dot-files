;;; init-org.el --- org-mode & related packages.
;;; Commentary:
;;; Code:

;; TODO: Hook together all of the org-agenda/org-notes/org-capture files along
;; with org-directory
(use-package org
  :straight (:type built-in)
  :bind
  (:map user-overlay-map
        ("o a" . org-agenda)
        ("o c" . org-capture))
  :config
  (setq org-hide-emphasis-markers t))

(provide 'init-org)
;;; init-org.el ends here.
