;;; setup-dev --- Setup packages for easy development

;;; Commentary:

;; This sets up packages which aid heavily in development

;;; Code:

(require 'evil)


;; show unnecessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
	  (lambda () (interactive)
	    (setq show-trailing-whitespace 1)))


;; set appearance of a tab that is represented by 8 spaces
(setq-default tab-width 8)


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
  :bind (:map company-active-map
	      ("M-j" . company-select-next)
	      ("M-k" . company-select-previous)
	      ("C-j" . company-select-next)
	      ("C-k" . company-select-previous))
  :init
  (progn
    (add-hook 'after-init-hook 'global-company-mode))
  :config
  (progn
    (delete 'company-semantic company-backends)))


;;; company backend for C/C++ headers
(use-package company-c-headers
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (dolist (folder (file-expand-wildcards "/usr/include/c++/*"))
    (add-to-list 'company-c-headers-path-system "/usr/include/c++/7.2.1/")))


(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-mode)
    (setq projectile-enable-caching t)))


(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))


(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))


(use-package zeal-at-point
  :ensure t
  :bind
  (("C-c d" . zeal-at-point))
  :config
  (progn
    (add-to-list 'zeal-at-point-mode-alist '(c-mode . "c"))
    (add-to-list 'zeal-at-point-mode-alist '(python-mode . "python"))))


(provide 'setup-dev)
;;; setup-dev.el ends here
