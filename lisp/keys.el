;; Evil (extensible vi layer)
(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil) ; needed by evil-collection
  (evil-undo-system 'undo-tree)
  (evil-overriding-maps
   '((Buffer-menu-mode-map     . nil)
     (color-theme-mode-map     . nil)
     (comint-mode-map          . nil)
     (compilation-mode-map     . nil)
     (grep-mode-map            . nil)
     (dictionary-mode-map      . nil)
     (ert-results-mode-map     . motion)
     (Info-mode-map            . motion)
     (speedbar-key-map         . nil)
     (speedbar-file-key-map    . nil)
     (speedbar-buffers-key-map . nil)))
  :config
  (define-key evil-ex-map "b " #'counsel-ibuffer)
  (define-key evil-ex-map "e " #'counsel-find-file)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (dolist (m '(normal motion visual))
    (evil-global-set-key m (kbd "K") #'man)
    (evil-global-set-key m (kbd "M-K") #'man)
    (evil-global-set-key m (kbd "SPC") mode-specific-map)
    (evil-global-set-key m (kbd "SPC u") 'universal-argument)
    (evil-global-set-key m (kbd "M-;") #'comment-dwim))
  (eval-after-load 'info
    (progn
      (evil-define-key '(normal motion) Info-mode-map (kbd "m") #'Info-menu)
      (evil-define-key '(normal motion) Info-mode-map (kbd "^") #'Info-up)
      (evil-define-key '(normal motion) Info-mode-map (kbd "RET") #'Info-follow-nearest-node)))
  (evil-mode 1))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(setq scroll-step 1
      delete-selection-mode 1)

;; Evil-like bindings for various modes
(use-package evil-collection
  :straight (evil-collection
             :type git :host github :repo "emacs-evil/evil-collection"
             :fork (:host github :repo "edwargix/evil-collection"))
  :after evil
  :init
  (evil-collection-init))

;; Easily surround text
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Evil keybindings for org
(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme))

;; opens a new terminal in default-directory
(global-set-key (kbd "C-M-<return>")
                (lambda ()
                  (interactive)
                  (start-process
                   "urxvt"
                   "*urxvt*"
                   "/usr/bin/urxvt"))) ;TODO: don't use absolute path

;; I don't always know where my frames are, and I want a way to kill Emacs 100%
;; of the time
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
(global-set-key (kbd "<f9>") (lambda ()
                               (interactive)
                               (kill-buffer)
                               (delete-window)))
(global-set-key (kbd "<f12>") (lambda ()
                                (interactive)
                                (kill-buffer)))

;; a minor mode for dealing with pairs
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (evil-global-set-key 'normal (kbd ">") #'sp-slurp-hybrid-sexp)
  (evil-global-set-key 'normal (kbd "<") #'sp-forward-barf-sexp)

  (defun radian-enter-and-indent-sexp (&rest _ignored)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  ; TODO: add more modes
  (dolist (mode '(go-mode terraform-mode))
    (sp-local-pair mode "{" nil :post-handlers
                   '((radian-enter-and-indent-sexp "RET")
                     (radian-enter-and-indent-sexp "<return>")))))

(use-package evil-matchit
  :init
  (add-hook 'python-mode-hook 'turn-on-evil-matchit-mode))

(use-package evil-numbers
  :bind
  (:map evil-normal-state-map
        ("C-a"   . evil-numbers/inc-at-pt)
        ("C-S-a" . evil-numbers/dec-at-pt)))

;; Function keys
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))
(global-set-key (kbd "<f6>") #'shell)
(global-set-key (kbd "<f7>") #'eshell)
(global-set-key (kbd "<f8>") (lambda ()
                               (interactive)
                               (switch-to-buffer "*scratch*")))
