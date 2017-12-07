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



(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-enable-caching t))


(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))


(provide 'setup-dev)
;;; setup-dev.el ends here
