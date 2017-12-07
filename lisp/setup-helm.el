;;; setup-helm --- Setup helm

;;; Commentary:

;; Set up helm and related packages

;;; Code:


;;; Helm: incremental completion and selection narrowing framework
(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files))
  :config
  (progn
    (require 'helm-config)
    (when (executable-find "curl")
      (setq helm-net-prefer-curl t))
    (add-hook 'helm-after-initialize-hook
	      ;; hide the cursor in helm buffers
	      (lambda ()
		(with-helm-buffer
		  (setq cursor-in-non-selected-windows nil))))

	(global-set-key (kbd "C-c h") 'helm-command-prefix)
	(global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-map (kbd "M-j") 'helm-next-line)
    (define-key helm-map (kbd "M-k") 'helm-previous-line)

    (define-key helm-map (kbd "C-h") 'helm-next-source)
    (define-key helm-map (kbd "C-S-h") 'describe-key)
    (define-key helm-map (kbd "C-l") (kbd "RET"))
    (define-key helm-map [escape] 'helm-keyboard-quit)
	(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
	(define-key helm-map (kbd "C-i") (kbd "<tab>")) ;; make <tab> work in terminal
    (helm-mode 1)
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
      (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
      (define-key keymap (kbd "C-S-h") 'describe-key))))


;;; Helm extension for yasnippet
(use-package helm-c-yasnippet
  :ensure t
  :after yasnippet
  :config
  (progn
	(setq helm-yas-space-match-any-greedy t)
	(global-set-key (kbd "C-c y") 'helm-yas-complete)))


;;; Helm extension for gtags
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

;;; Helm extension for projectile
(use-package helm-projectile
  :ensure t
  :after (projectile helm)
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien))


(provide 'setup-helm)
;;; setup-helm.el ends here
