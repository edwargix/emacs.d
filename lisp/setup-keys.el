;;; setup-keys --- Setup keys

;;; Commentary:

;; Setup keybindings to my liking

;;; Code:


;;; Evil (extensible vi layer)
(use-package evil
  :ensure t
  :init
  (progn
    (setq evil-want-C-u-scroll t
	  evil-want-integration nil))
  :config
  (progn
    (require 'evil)
    (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
    (evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button)
    (define-key evil-ex-map "b " 'helm-mini)
    (define-key evil-ex-map "e " 'helm-find-files)
    (add-hook 'with-editor-mode-hook 'evil-insert-state)
    (evil-global-set-key 'normal (kbd "SPC") mode-specific-map)
    (evil-global-set-key 'motion (kbd "SPC") mode-specific-map)
    (evil-mode 1)))


(setq scroll-step 1
      delete-selection-mode 1)


(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))


;;; Easily surround text
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (progn
    (global-evil-surround-mode 1)))


;;; Evil keybindings for magit
(use-package evil-magit
  :after (evil magit)
  :ensure t)


;;; Evil keybindings for org
(use-package evil-org
  :ensure t
  :after (evil org)
  :hook ((org-mode . evil-org-mode)
	 (evil-org-mode . evil-org-set-key-theme))
  :bind (:map org-mode-map
	      ("<return>" . evil-org-return)))


;;; I don't always know where my frames are, and I want a way to kill
;;; Emacs 100% of the time
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)


(provide 'setup-keys)
;;; setup-keys.el ends here
