(use-package exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
  (exec-path-from-shell-initialize))

;; Winner mode: allows for undoing and redoing of windoow configurations
(use-package winner
  :straight nil
  :bind (:map evil-normal-state-map
              ("C-w u" . #'winner-undo)
         :map evil-motion-state-map
              ("C-w u" . #'winner-undo))
  :init
  ;; C-c <left> : undo
  ;; C-c <right>: redo
  (winner-mode t))

;; Allow easily switching windows with Shift-{left,right,up,down}
(windmove-default-keybindings)

(use-package dired
  :straight nil
  ;; don't show details of files in dired mode by default
  :hook (dired-mode . (lambda ()
                        (dired-hide-details-mode t)))
  :bind ("C-x d" . (lambda ()
                     (interactive)
                     (dired "."))))

(use-package ag)

(use-package help+
  :straight help-plus)
(use-package help-fns+
  :straight help-fns-plus)
(use-package help-mode+
  :straight help-mode-plus)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "markdown"))

;; Syntax/error checking
(use-package flycheck
  :config
  (global-flycheck-mode)
  (evil-define-key 'normal
    flycheck-error-list-mode-map (kbd "q") 'quit-window))

;; Yasnippet: yet another snippet extension
(use-package yasnippet
  :bind
  ("C-c y" . yas-expand)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; Setup duckduckgo search engine
(use-package engine-mode
  :config
  (defengine brave
    "https://search.brave.com/search?q=%s"
    :keybinding "b")
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine wordnik
    "https://www.wordnik.com/words/%s"
    :keybinding "w")
  (defengine pypi
    "https://pypi.org/search/?q=%s"
    :keybinding "p")
  (engine-mode))

(use-package which-key
  :config
  (which-key-mode))

;; setup the mu4e email client
(when (file-exists-p "~/lisp/setup-mu4e.el")
  (load-file "~/lisp/setup-mu4e.el"))

;; statistics software and R-lang integration
(use-package ess)

;; TeX/LaTeX
(use-package tex
  :defer
  :straight auctex
  :custom
  (TeX-command-extra-options "-shell-escape")
  (TeX-engine 'xetex)
  :config
  (setcdr (assoc 'output-pdf TeX-view-program-selection)
          '("Zathura")))

;; ability to insert random text
(use-package lorem-ipsum)

;; show eshell with C-S-s
(use-package shell-pop
  :custom
  (shell-pop-shell-type '("eshell" "*eshell*" #'eshell))
  (shell-pop-universal-key "C-S-s"))

(use-package yaml-mode
  :mode "\\.sls\\'")

(use-package graphviz-dot-mode
  :defer t)
