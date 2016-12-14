
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

;;; Install helm
(package-install 'helm)

;;; This config code for helm was obtained from https://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)

;;; "'C-x c' is quite close to 'C-x C-c'" Agreed!
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persisten action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

;; (setq helm-split-window)

(helm-mode 1)

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
