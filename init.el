;;; Change frame title
(setq frame-title-format "emacs")

;;; Disable menu bar
(menu-bar-mode 0)

;;; Disable scroll bar
(scroll-bar-mode 0)

;;; Disable tool bar
(tool-bar-mode 0)

;;; Set cursor type
(set-default 'cursor-type 't)

;;; Show column number next to line number in mode line
(column-number-mode)

;;; Highlight parentheses
(show-paren-mode)

;;; Winner mode: allows for undoing and redoing of windoow configurations
;;; C-c <left> : undo
;;; C-c <right>: redo
(winner-mode t)

;;; Allow easily switching windows with Shift-{left,right,up.down}
(windmove-default-keybindings)

;;; Include package
(require 'package)

;;; Add melpa archives
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))

;;; Load and activate lisp packages
(package-initialize)

;;; Install use-package for easy package configuration
(package-install 'use-package)

;;; Change font to 12pt
(set-face-attribute 'default nil :height 120)

;;; Change theme
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;;; Evil (extensible vi layer)
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1)))

;;; Magit: a Git Porcelain inside Emacs
(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup))
  :config
  (use-package evil-magit
    :ensure t))

;;; Company (complete anything) mode
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;;; Helm: incremental completion and selection narrowing framework
(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files))
  :config
  (require 'helm-config)
  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))
  (helm-mode 1))

;;; Install all packages
(setq my-package-list '(yasnippet
			company-quickhelp
			company-c-headers
			company-tern
			auctex
			projectile
			js-import
			jade-mode
			company-web
			js2-mode
			ggtags
			elpy
			paredit
			ac-js2
			eclim
			paredit-everywhere
			engine-mode))

(condition-case nil
    (mapc 'package-install my-package-list)
  (error
   (package-refresh-contents)
   (mapc 'package-install my-package-list)))

;;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;; Quickhelp (documentation lookup) for company
(setq company-quickhelp-idle-delay 1)
(company-quickhelp-mode 1)

;;; company backend for C/C++ headers
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

;;; company backend for javascript: tern
;; (add-to-list 'company-backends 'company-tern)
;; (add-hook 'js-mode-hook (lambda() (tern-mode)))

;;; Load tern server (for javascript ide-like features)
(cd "~/.emacs.d/tern")
(shell-command "npm install")
(cd "../")
(add-to-list 'load-path "./tern/emacs")
(autoload 'tern-mode "tern.el" nil t)

;;; auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;;; jade mode
(require 'jade-mode)

;;; company-web
(require 'company-web-html)
(require 'company-web-jade)
(define-key jade-mode-map (kbd "C-'") 'company-web-jade)

;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)

;;; GNU Global front end
(require 'ggtags)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
	      (ggtags-mode 1))))

;; Paredit everywhere
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; Elpy
(elpy-enable)

;; Setup paredit for javascript
(define-key js-mode-map "{" 'paredit-open-curly)
(define-key js-mode-map "}" 'paredit-close-curly-and-newline)

;; Setup duckduckgo search engine
(require 'engine-mode)
(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")
(engine-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(initial-buffer-choice "~/.emacs.d/init.el")
 '(package-selected-packages
   (quote
    (evil-surround evil-magit use-package magit engine-mode eclim paredit-everywhere ac-js2 paredit elpy ggtags company-web jade-mode company-c-headers company-quickhelp yasnippet js2-mode projectile js-import auctex company-tern company)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
