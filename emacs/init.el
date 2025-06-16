;;; init.el --- Emacs user configuration.
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'bootstrap)
;; -----------------
(require 'init-editing)
(require 'init-minibuffer)
(require 'init-misc)
(require 'init-org)
(require 'init-programming)
(require 'init-theme)

;; (defun parent-directory (dir)
;;   (unless (equal "/" dir)
;;     (file-name-directory (directory-file-name dir))))
(defun ext/parent-directory (dir)
  "TODO."
    (unless (equal "/" dir)
      (file-name-directory (directory-file-name dir))))

(defun ext/find-file-in-hierachy (dir filename)
  "TODO - make this iterative."
  (let* ((file (concat dir filename))
         (parent-dir (ext/parent-directory (expand-file-name dir))))
    (if (file-exists-p file)
        file
      (when parent-dir
        (ext/find-file-in-hierachy parent-dir filename)))))

(use-package compile-multi
  :config
  (setq compile-multi-config '(((ext/find-file-in-hierachy default-directory "Cargo.toml")
                                ("cargo build" . (:command "cargo build"))))))

;; (defun check-for-file-up-tree (filename)
;;   "Interactively walk up the directory tree from the current directory and check for FILENAME."
;;   (interactive "sEnter filename to search for: ")  ;; Prompt user for filename
;;   (let ((dir default-directory))  ;; Start from the current buffer's directory
;;     (while (and dir (not (equal dir "/")))
;;       (if (file-exists-p (expand-file-name filename dir))
;;           (return dir))  ;; Return the directory if file is found
;;       (setq dir (file-name-directory (directory-file-name dir))))  ;; Move to parent directory
;;     nil))  ;; Return nil if no file is found

;; Load extended local initialization files.
(let ((local-config-dir "~/.config/emacs/"))
  (dolist (local-config-file (directory-files-recursively local-config-dir "\\.el$"))
    (when (file-regular-p local-config-file)
      (load-file local-config-file))))

(provide 'init)
;;; init.el ends here
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    '("56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
;;      "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4"
;;      "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
;;      "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
;;      "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
;;      "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc"
;;      "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c"
;;      "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392"
;;      "6e18353d35efc18952c57d3c7ef966cad563dc65a2bba0660b951d990e23fc07" default)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037"
     "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
     "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a"
     "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9"
     "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
     "32f22d075269daabc5e661299ca9a08716aa8cda7e85310b9625c434041916af"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36"
     "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518"
     "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d"
     "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0"
     "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2"
     "e978b5106d203ba61eda3242317feff219f257f6300bd9b952726faf4c5dee7b"
     "2771ec93656faf267521dce9ffe1a6ad88cd0bea87aa0e8c4fc80bf355c58c1d"
     "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554"
     "9e36779f5244f7d715d206158a3dade839d4ccb17f6a2f0108bf8d476160a221"
     "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184"
     "2b20b4633721cc23869499012a69894293d49e147feeb833663fdc968f240873"
     "113a135eb7a2ace6d9801469324f9f7624f8c696b72e3709feb7368b06ddaccc"
     "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33"
     "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641"
     "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87"
     "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1"
     "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
     "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40"
     "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738"
     "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b"
     "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1"
     "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1"
     "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f"
     "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
     "350fef8767e45b0f81dd54c986ee6854857f27067bac88d2b1c2a6fa7fecb522"
     "3c08da65265d80a7c8fc99fe51df3697d0fa6786a58a477a1b22887b4f116f62"
     "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a"
     "c8c4baac2988652a760554e0e7ce11a0fe0f8468736be2b79355c9d9cc14b751"
     "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478"
     "7758a8b8912ef92e8950a4df461a4d510484b11df0d7195a8a3d003965127abc"
     "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350"
     "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
     "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc"
     "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c"
     "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392"
     "6e18353d35efc18952c57d3c7ef966cad563dc65a2bba0660b951d990e23fc07" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
