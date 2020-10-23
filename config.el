(use-package flycheck
  :defer t
  :diminish
  :hook ((prog-mode markdown-mode) . flycheck-mode)
  :custom
  (flycheck-global-modes
   '(not text-mode outline-mode fundamental-mode org-mode
	 diff-mode shell-mode eshell-mode term-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe)
  :init
  (use-package flycheck-grammarly :defer t)
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))

(setq org-indent-indentation-per-level 1)
(setq org-adapt-indentation nil)
(setq org-hide-leading-stars 't)

(setq org-hide-emphasis-markers t)

(customize-set-variable 'org-blank-before-new-entry 
                        '((heading . nil)
                          (plain-list-item . nil)))
(setq org-cycle-separator-lines 1)

(use-package evil
  :config
  (evil-mode 1))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package ivy
  :config
  (ivy-mode 1))

(use-package yasnippet
:diminish yas-minor-mode
:init
(use-package yasnippet-snippets :after yasnippet)
:hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
:bind
(:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
(:map yas-keymap
      (("TAB" . smarter-yas-expand-next-field)
       ([(tab)] . smarter-yas-expand-next-field)))
:config
(yas-reload-all)
(defun smarter-yas-expand-next-field ()
  "Try to `yas-expand' then `yas-next-field' at current cursor position."
  (interactive)
  (let ((old-point (point))
        (old-tick (buffer-chars-modified-tick)))
    (yas-expand)
    (when (and (eq old-point (point))
               (eq old-tick (buffer-chars-modified-tick)))
      (ignore-errors (yas-next-field))))))

(use-package company
  :custom
  (company-show-numbers t)
  (company-idle-delay 0)
  :config
  (global-company-mode 1))

(use-package avy
    :custom
    (global-set-key (kbd "C-:") 'avy-goto-word-1))

(use-package swiper
  :bind (("C-f" . swiper)))

(add-hook
 'text-mode-hook
 'auto-fill-mode)

(add-hook
 'text-mode-hook
 'olivetti-mode)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(use-package winum
  :init
  (setq winum-keymap
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-`") 'winum-select-window-by-number)
	  (define-key map (kbd "C-Â²") 'winum-select-window-by-number)
	  (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
	  (define-key map (kbd "M-1") 'winum-select-window-1)
	  (define-key map (kbd "M-2") 'winum-select-window-2)
	  (define-key map (kbd "M-3") 'winum-select-window-3)
	  (define-key map (kbd "M-4") 'winum-select-window-4)
	  (define-key map (kbd "M-5") 'winum-select-window-5)
	  (define-key map (kbd "M-6") 'winum-select-window-6)
	  (define-key map (kbd "M-7") 'winum-select-window-7)
	  (define-key map (kbd "M-8") 'winum-select-window-8)
	  map))
  :config
  (winum-mode 1))

(set-face-font 'default "Roboto Mono Light 10")

;; Line cursor and no blink
(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode 0)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Paren mode is part of the theme
(show-paren-mode t)

;; No fringe but nice glyphs for truncated and wrapped lines
(fringe-mode '(0 . 0))
(defface fallback '((t :family "Fira Code Light"
                       :inherit 'face-faded)) "Fallback")

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; or for tremacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
