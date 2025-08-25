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
        org-return-follows-link t
        org-agenda-files `(,org-directory)
        org-agenda-window-setup 'only-windoow
        org-default-notes-file (concat org-directory "/notes.org")))

(use-package org-roam
  :init
  (add-hook 'emacs-startup-hook 'org-roam-db-autosync-mode)
  :custom
  (org-roam-directory (file-truename (concat org-directory "/roam/")))
  :bind
  (:map user-overlay-map
        ("o o" . org-roam-node-visit)
        ("o i" . org-roam-node-insert)
        ("o F" . org-roam-capture)
        ("o f" . org-roam-node-find)))

(provide 'init-org)
;;; init-org.el ends here.
