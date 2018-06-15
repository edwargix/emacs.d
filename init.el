;;; init.el --- Setup Emacs

;;; Commentary:

;; My personal init.el for Emacs.

;;; Code:

;; (package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")

(load-file "~/.emacs.d/lisp/setup-packages.el")


(require 'setup-keys)
(require 'setup-appearance)
(require 'setup-dev)
(require 'setup-helm)
(require 'setup-defaults)
(require 'setup-lisp)
(require 'setup-python)
(require 'setup-treemacs)


;;; Winner mode: allows for undoing and redoing of windoow configurations
;;; C-c <left> : undo
;;; C-c <right>: redo
(winner-mode t)


;;; Allow easily switching windows with Shift-{left,right,up,down}
(windmove-default-keybindings)


;;; Don't make backup files
(setq make-backup-files nil)


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;;; a minor mode for dealing with pairs
(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)))


;;; magit: a Git Porcelain
(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)))


;;; Syntax/error checking
(use-package flycheck
  :ensure t
  :init
  (progn
    (global-flycheck-mode)
    (evil-define-key 'normal
      flycheck-error-list-mode-map (kbd "q") 'quit-window)))


;;; Quickhelp (documentation lookup) for company
(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (progn
    (setq company-quickhelp-idle-delay 1)
    (company-quickhelp-mode 1)))


;;; Yasnippet: yet another snippet extension
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))


(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)


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
  :init
  (progn
    (load-file "~/org/setup.el")
    (unless (package-installed-p 'org-plus-contrib)
      (package-install 'org-plus-contrib))
    (setq org-default-notes-file "~/notes.org"
          org-return-follows-link t
          org-read-date-force-compatible-dates nil)
    (setq org-src-fontify-natively t
          org-src-tab-acts-natively t)
    (setq org-latex-pdf-process
          '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    (use-package htmlize :ensure t))
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-iswitchb)
   ("C-c l" . org-store-link)
   ("C-c o" . org-open-at-point-global))
  :config
  (progn
    (require 'ox-md)
    (require 'ox-beamer)
    ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
    ;; (setq org-latex-listings 'minted)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (dot . t)
       (org . t)
       (gnuplot . t)))))


;;; UTF-8 bullets for org-mode
(use-package org-bullets
  :ensure t
  :after org
  :config
  (progn
    (add-hook 'org-mode-hook 'org-bullets-mode)))


(use-package org-contacts
  :after org
  :config
  (progn
    (setq org-contacts-files '("~/org/contacts.org"))))


;;; Paradox: a modern package menu
(use-package paradox
  :ensure t
  :commands (paradox-enable paradox-quit-and-close)
  :init
  (progn
    (paradox-enable)
    (evil-set-initial-state 'paradox-menu-mode 'motion)))


(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;;; functions to manage packages on linux distros
(use-package system-packages
  :ensure t)


;;; setup the mu4e email client
(when (file-exists-p "~/scripts/setup-mu4e.el")
  (load-file "~/scripts/setup-mu4e.el"))


;;; statistics software and R-lang integration
(use-package ess
  :ensure t)


;;; TeX/LaTeX
(use-package tex
  :ensure auctex)


;;; ability to insert random text
(use-package lorem-ipsum
  :ensure t)


;;; number windows to easily switch between them
(use-package winum
  :ensure t
  :config
  (progn
    (winum-mode)))


;;; Install local user packages
(dolist (d (file-expand-wildcards "~/.local/share/emacs/site-lisp/*"))
  (add-to-list 'load-path d t))
(add-to-list 'Info-directory-list "~/.local/share/info/")


;;; Start Emacs Daemon
(require 'server)
(unless (server-running-p)
  (server-start))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(initial-buffer-choice t)
 '(initial-scratch-message "")
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
