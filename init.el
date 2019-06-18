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
(load "~/.emacs.d/lisp/setup-packages")
(load "~/.emacs.d/lisp/setup-keys")
(load "~/.emacs.d/lisp/setup-appearance")
(load "~/.emacs.d/lisp/setup-org")
(load "~/.emacs.d/lisp/setup-dev")
(load "~/.emacs.d/lisp/setup-ivy")
(load "~/.emacs.d/lisp/setup-defaults")
(load "~/.emacs.d/lisp/setup-lisp")
(load "~/.emacs.d/lisp/setup-python")
(load "~/.emacs.d/lisp/setup-go")
(load "~/.emacs.d/lisp/setup-web")


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
    (evil-global-set-key 'normal (kbd "C-n") #'fzf-projectile)))


(use-package ag)


(use-package help+)
(use-package help-fns+)
(use-package help-mode+)


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


;;; a minor mode for dealing with pairs
(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)))


;;; magit: a Git Porcelain
(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)))


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


;;; ledger
(use-package ledger-mode
  :defer t
  :init
  (progn
    (setq ledger-clear-whole-transactions t))
  :config
  (progn
    (add-to-list 'evil-emacs-state-modes 'ledger-report-mode))
  :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode))


;;; flycheck for ledger
(use-package flycheck-ledger
  :after ledger)


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
 '(initial-buffer-choice t)
 '(initial-scratch-message ""))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
