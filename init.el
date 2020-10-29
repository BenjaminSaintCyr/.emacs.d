;; -*- coding: utf-8; lexical-binding: t; -*-

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

;;; use straigth package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;;; Integration with use-package
(straight-use-package 'use-package)
;;; Better garbage colletion
(use-package gcmh
:straight t
:ensure t
  :config
  (gcmh-mode 1))

(org-babel-load-file "~/.emacs.d/config.org")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-blank-before-new-entry '((heading) (plain-list-item)))
 '(package-selected-packages
   '(ac-octave evil-magit gcmh dap-mode lsp-ui lsp-mode elfeed-org elfeed counsel magit avy flycheck-grammarly flycheck company ivy-rich ivy winum use-package evil doom-modeline dashboard auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))
