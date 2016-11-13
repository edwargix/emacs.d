
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

;;; Change font to 13pt
(set-face-attribute 'default nil :height 130)
