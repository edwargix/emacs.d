
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

;;; Simple smex
(ido-mode)

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

;;; Update package listings
(package-refresh-contents)

;;; Change font to 13pt
(set-face-attribute 'default nil :height 130)

;;; Install company (complete anything) mode and activate it
(package-install 'company)
(require 'company)
(company-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
