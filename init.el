
;;; Thanks to Mohammed Ismail Ansari for the YouTube tutorial
;;; https://www.youtube.com/watch?v=FRu8SRWuUko

;;; edwargix's emacs config!

;;; Change theme
(load-theme 'adwaita)

;;; Change xfce/gnome window title
(setq frame-title-format "emacs")

;;; Enable menu bar
(menu-bar-mode 1)

;;; Disable scroll bar
(scroll-bar-mode -1)

;;; Set cursor type
(set-default 'cursor-type 't)

;;; Show column number next to line number in mode line
(column-number-mode)

;;; Highlight parentheses
(show-paren-mode)

;;; (global-hl-line-mode)

;;; Winner mode: allows for undoing and redoing of windoow configurations
;;; C-c <left> : undo
;;; C-c <right>: undo undo (aka redo)
(winner-mode t)

;;; Allow easily switching windows with Shift-{left,right,up.down}
(windmove-default-keybindings)

;;; Include package
(require 'package)

;;; Add melpa archives
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     t)

;;; Add marmalade archives
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/")
	     t)

;;; Load and activate lisp packages
(package-initialize)

;;; Change font to 13pt
(set-face-attribute 'default nil :height 130)

;;; Install company (complete anything) mode and activate it
(condition-case nil
    (package-install 'company)
  (error
    (package-refresh-contents)
    (package-install 'company)))
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;; Install yasnippet
(package-install 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)

;;; Install Quickhelp (documentation lookup) for company
(package-install 'company-quickhelp)
(setq company-quickhelp-idle-delay 1)
(company-quickhelp-mode 1)

;;; Install company backend for C/C++ headers
(package-install 'company-c-headers)
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

;;; Install company backend for javascript
(package-install 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js-mode-hook (lambda() (tern-mode)))

;;; Load tern server (for javascript ide-like features)
(cd "~/.emacs.d/tern")
(shell-command "npm install")
(cd "../")
(add-to-list 'load-path "./tern/emacs")
(autoload 'tern-mode "tern.el" nil t)

;;; Install auctex
(package-install 'auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;;; Install android mode
(package-install 'android-mode)

;;; Install projectile
(package-install 'projectile)

;;; Install js-import
(package-install 'js-import)

;;; Install jade mode
(package-install 'jade-mode)
(require 'jade-mode)

;;; Install company-web
(package-install 'company-web)
(require 'company-web-html)
(require 'company-web-jade)

(define-key html-mode-map (kbd "C-'") 'company-web-html)
(define-key jade-mode-map (kbd "C-'") 'company-web-jade)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-buffer-choice "~/.emacs.d/init.el")
 '(package-selected-packages
   (quote
    (projectile js-import android-mode auctex company-tern company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
