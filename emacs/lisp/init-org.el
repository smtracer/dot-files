;;; init-org.el --- org-mode & related packages.
;;; Commentary:
;;; Code:

(use-package org
  :straight (:type built-in)
  :bind
  (:map user-overlay-map
        ("o a" . org-agenda)
        ("o c" . org-capture))
  :config
  (setq org-hide-emphasis-markers t
        org-agenda-files `(,org-directory)
        org-default-notes-file (concat org-directory "/notes.org")))

(provide 'init-org)
;;; init-org.el ends here.
