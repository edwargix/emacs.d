;;; Change frame title
(setq frame-title-format "emacs")

;;; Font
(set-frame-font
 (font-spec
  :name "Source Code Pro"
  :size 13
  :weight 'normal
  :width 'normal))

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

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;;; Load and activate lisp packages
(package-initialize)

;;; Refresh package repositories
(package-refresh-contents)

;;; Install use-package for easy package configuration
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;; Change theme
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;;; Evil (extensible vi layer)
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
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
   ("C-x M-g" . magit-dispatch-popup)))

;;; Evil keybindings for magit
(use-package evil-magit
  :after evil
  :ensure t)

;;; Company (complete anything) mode
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;;; Quickhelp (documentation lookup) for company
(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (setq company-quickhelp-idle-delay 1)
  (company-quickhelp-mode 1))

;;; company backend for C/C++ headers
(use-package company-c-headers
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends 'company-c-headers))

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

;;; Yasnippet: yet another snippet extension
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;;; Setup duckduckgo search engine
(use-package engine-mode
  :ensure t
  :config
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (engine-mode))

;;; Org mode for keeping notes, todo lists, planning, and fast
;;; documenting
(use-package org
  :ensure t
  :config
  (setq org-default-notes-file "~/notes.org"))

;;; Evil keybindings for org
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme))))

;;; Paradox: a modern package menu
(use-package paradox
  :ensure t
  :config
  (paradox-enable))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :after projectile
  :config
  (paradox-enable)
  (helm-projectile-on))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(initial-buffer-choice "~/.emacs.d/init.el")
 '(org-agenda-files
   (quote
    ("~/org/agenda/event.org" "~/org/agenda/todo.org" "~/org/agenda/homework.org" "~/org/agenda/exam.org")))
 '(org-capture-templates
   (quote
    (("w" "Store cool word for later study" entry
      (file "~/lists/fantastic_words.org")
      "* %(setq frypan-word (downcase (read-string \"Word: \" (current-kill 0))))
%(completing-read \"Definition: \" (get-word-definitions frypan-word))
%?")
     ("t" "Todo" entry
      (file "~/org/agenda/todo.org")
      "* TODO %^{TODO|%i}
DEADLINE: %^{Deadline}T

%?
")
     ("e" "Event" entry
      (file "~/org/agenda/event.org")
      "* %^{Event}
%^{When}T

%?
")
     ("h" "Homework" entry
      (file "~/org/agenda/homework.org")
      "* TODO %^{Homework} %^g
DEADLINE: %^{Due}T

%i%?
")
     ("x" "Exam" entry
      (file "~/org/agenda/exam.org")
      "* %^{Exam} %^G
%^{When}T

%?
"))))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
