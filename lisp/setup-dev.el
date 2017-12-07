;;; setup-dev --- Setup packages for easy development

;;; Commentary:

;; This sets up packages which aid heavily in development

;;; Code:

(require 'evil)


;; show unnecessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
		  (lambda () (interactive)
			(setq show-trailing-whitespace 1)))


;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)


(global-set-key (kbd "<f5>") (lambda ()
							   (interactive)
							   (setq-local compilation-read-command nil)
							   (call-interactively 'compile)))


;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file contatining the main routine at startup
 gdb-show-main t)


;;; Company (complete anything) mode
(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
	(delete 'company-semantic company-backends)
    (define-key company-active-map (kbd "M-j") 'company-select-next)
    (define-key company-active-map (kbd "M-k") 'company-select-previous)))


(use-package helm-gtags
  :ensure t
  :commands (helm-gtags-mode)
  :init
  (progn
	(setq helm-gtags-ignore-case t
		  helm-gtags-auto-update t
		  helm-gtags-use-input-at-cursor t
		  helm-gtags-pulse-at-cursor t
		  helm-gtags-prefix-key (kbd "C-c g")
		  helm-gtags-suggested-key-mapping t)
	(add-hook 'dired-mode-hook 'helm-gtags-mode)
	(add-hook 'eshell-mode-hook 'helm-gtags-mode)
	(add-hook 'c-mode-hook 'helm-gtags-mode)
	(add-hook 'c++-mode-hook 'helm-gtags-mode)
	(add-hook 'asm-made-hook 'helm-gtags-mode))
  :config
  (progn
	(evil-define-key 'normal helm-gtags-mode-map (kbd "C-c g a")
	  'helm-gtags-tags-in-this-function)
	(evil-define-key 'nomral helm-gtags-mode-map (kbd "C-j")
	  'helm-gtags-select)
	(evil-define-key 'normal helm-gtags-mode-map (kbd "M-.")
	  'helm-gtags-dwim)
	(evil-define-key 'normal helm-gtags-mode-map (kbd "M-,")
	  'helm-gtags-pop-stack)
	(evil-define-key 'normal helm-gtags-mode-map (kbd "C-c <")
	  'helm-gtags-previous-history)
	(evil-define-key 'normal helm-gtags-mode-map (kbd "C-c >")
	  'helm-gtags-next-history)))


(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-enable-caching t))


(use-package helm-projectile
  :ensure t
  :after (projectile helm)
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien))


(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))


(provide 'setup-dev)
;;; setup-dev.el ends here
