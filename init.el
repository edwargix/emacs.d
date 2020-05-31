;;; init.el --- Setup Emacs

;;; Commentary:

;; My personal init.el for Emacs.

;;; Code:

;;; Install local user packages
(dolist (d (apply #'append (mapcar #'file-expand-wildcards
                                '("~/.local/share/emacs/site-lisp/*"
                                  "/usr/local/share/emacs/site-lisp/*"
                                  "/usr/share/emacs/site-lisp/*"))))
  (add-to-list 'load-path d))

;;; Loading of personal config files
(load "~/.emacs.d/lisp/packages")
(load "~/.emacs.d/lisp/keys")
(load "~/.emacs.d/lisp/appearance")
(load "~/.emacs.d/lisp/org")
(load "~/.emacs.d/lisp/dev")
(load "~/.emacs.d/lisp/ivy")
(load "~/.emacs.d/lisp/lisp")
(load "~/.emacs.d/lisp/python")
(load "~/.emacs.d/lisp/go")
(load "~/.emacs.d/lisp/web")
(load "~/.emacs.d/lisp/racket")
(load "~/.emacs.d/lisp/clojure")
(load "~/.emacs.d/lisp/scala")
(load "~/.emacs.d/lisp/docker")

;;; Winner mode: allows for undoing and redoing of windoow configurations
;;; C-c <left> : undo
;;; C-c <right>: redo
(winner-mode t)
(dolist (m '(motion normal))
  (evil-global-set-key m (kbd "C-w u") #'winner-undo))

(dolist (m '(motion normal insert))
  (evil-global-set-key m (kbd "C-x d") (lambda () (interactive) (dired "."))))

;;; Allow easily switching windows with Shift-{left,right,up,down}
(windmove-default-keybindings)

;;; Don't make backup files
(setq make-backup-files nil)

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode t)))

(use-package fzf
  :commands fzf-projectile
  :init
  (progn
    (evil-global-set-key 'normal (kbd "C-n") #'fzf-projectile)
    (evil-global-set-key 'normal (kbd "C-S-n") #'fzf-directory)))

(use-package ag)

(use-package help+
  :straight help-plus)
(use-package help-fns+
  :straight help-fns-plus)
(use-package help-mode+
  :straight help-mode-plus)

(use-package scribble
  :straight nil
  :load-path "contrib/"
  :init
  (progn
    (mapc (lambda (pair)
            (or (assoc (car pair) auto-mode-alist)
                (push pair auto-mode-alist)))
          '(("\\.scrbl\\'" . scribble-mode))))
  :commands scribble-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

;;; magit: a Git Porcelain
(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)))

;;; Syntax/error checking
(use-package flycheck
  :init
  (progn
    (global-flycheck-mode)
    (evil-define-key 'normal
      flycheck-error-list-mode-map (kbd "q") 'quit-window)))

;;; Yasnippet: yet another snippet extension
(use-package yasnippet
  :bind
  ("C-c y" . yas-expand)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;;; Setup duckduckgo search engine
(use-package engine-mode
  :config
  (progn
    (defengine duckduckgo
      "https://duckduckgo.com/?q=%s"
      :keybinding "d")
    (defengine wordnik
      "https://www.wordnik.com/words/%s"
      :keybinding "w")
    (defengine pypi
      "https://pypi.org/search/?q=%s"
      :keybinding "p")
    (engine-mode)))

(use-package which-key
  :config
  (which-key-mode))

;;; functions to manage packages on linux distros
(use-package system-packages)

;;; setup the mu4e email client
(when (file-exists-p "~/scripts/setup-mu4e.el")
  (load-file "~/scripts/setup-mu4e.el"))

;;; statistics software and R-lang integration
(use-package ess)

;;; TeX/LaTeX
(use-package tex
  :straight auctex
  :init
  (progn
    (setq TeX-command-extra-options "-shell-escape")
    (setq-default TeX-engine 'xetex))
  :config
  (progn
    (setcdr (assoc 'output-pdf TeX-view-program-selection)
            '("Zathura"))))

;;; ability to insert random text
(use-package lorem-ipsum)

;;; number windows to easily switch between them
(use-package winum
  :config
  (progn
    (winum-mode)))

(use-package shell-pop
  :custom
  (shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
  (shell-pop-universal-key "C-S-s"))

(use-package yaml-mode
  :mode "\\.sls\\'")

(use-package graphviz-dot-mode
  :defer t)

;;; Start Emacs Daemon
(require 'server)
(unless (server-running-p)
  (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(fill-column 80)
 '(initial-buffer-choice t)
 '(initial-scratch-message "")
 '(url-privacy-level (quote paranoid)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
