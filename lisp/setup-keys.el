;;; Evil (extensible vi layer)
(use-package evil
  :init
  (progn
    (setq evil-want-C-u-scroll t
          evil-want-integration nil
          evil-want-keybinding nil)) ; needed by evil-collection
  :config
  (progn
    (require 'evil)
    (define-key evil-ex-map "b " #'counsel-ibuffer)
    (define-key evil-ex-map "e " #'counsel-find-file)
    (add-hook 'with-editor-mode-hook 'evil-insert-state)
    (evil-global-set-key 'normal (kbd "K") #'man)
    (evil-global-set-key 'motion (kbd "K") #'man)
    (evil-global-set-key 'normal (kbd "SPC") mode-specific-map)
    (evil-global-set-key 'motion (kbd "SPC") mode-specific-map)
    (evil-global-set-key 'normal (kbd "SPC u") 'universal-argument)
    (evil-global-set-key 'motion (kbd "SPC u") 'universal-argument)
    (evil-mode 1)))


(setq scroll-step 1
      delete-selection-mode 1)


(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))


;;; Easily surround text
(use-package evil-surround
  :after evil
  :config
  (progn
    (global-evil-surround-mode 1)))


;;; Evil keybindings for magit
(use-package evil-magit
  :after (evil magit))


;;; Evil keybindings for org
(use-package evil-org
  :after (evil org)
  :config
  (progn
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme)))))


;;; I don't always know where my frames are, and I want a way to kill
;;; Emacs 100% of the time
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
(global-set-key (kbd "<f9>") (lambda () (interactive)
                               (kill-buffer)
                               (delete-window)))
(global-set-key (kbd "<f12>") (lambda ()
                                ;; lambda needed to kill current buffer
                                (interactive)
                                (kill-buffer)))


(use-package evil-matchit
  :init
  (progn
    (add-hook 'python-mode-hook 'turn-on-evil-matchit-mode)))


;;; Function keys
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))
(global-set-key (kbd "<f6>") #'shell)
(global-set-key (kbd "<f7>") #'eshell)
(global-set-key (kbd "<f8>") (lambda ()
                               (interactive)
                               (switch-to-buffer "*scratch*")))
