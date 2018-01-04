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
  :bind
  (:map mode-specific-map
  	("p" . projectile-command-map))
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


(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

(defun rtags-hook ()
  "Setup rtags and flycheck."
  (require 'flycheck-rtags)
  (rtags-start-process-unless-running)
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)
  ;; (rtags-mode)
  )

(use-package rtags
  :ensure t
  :hook
  (c-mode . rtags-hook)
  (c++-mode . rtags-hook)
  (objc-mode . rtags-hook)
  :config
  (progn
    (evil-define-key '(normal motion) 'global (kbd "M-.") 'rtags-find-symbol-at-point)
    (evil-define-key '(normal motion) 'global (kbd "M-,") 'rtags-find-references-at-point)
    (evil-define-key '(normal motion) 'global (kbd "M-;") 'rtags-find-file)
    (evil-define-key '(normal motion) 'global (kbd "C-.") 'rtags-find-symbol)
    (evil-define-key '(normal motion) 'global (kbd "C-,") 'rtags-find-references)
    (evil-define-key '(normal motion) 'global (kbd "C-<") 'rtags-find-virtuals-at-point)
    (evil-define-key '(normal motion) 'global (kbd "C->") 'rtags-diagnostics)
    (evil-define-key '(normal motion) 'global (kbd "M-[") 'rtags-location-stack-back)
    (evil-define-key '(normal motion) 'global (kbd "M-]") 'rtags-location-stack-forward)
    (use-package company-rtags :ensure t)
    (use-package flycheck-rtags :ensure t)
    (use-package helm-rtags :ensure t)
    (setq rtags-autostart-diagnostics t
	  rtags-completions-enabled t)
    (with-eval-after-load 'company
      ;; (push 'company-rtags company-backends)
      (add-to-list 'company-backends 'company-rtags)
      )
    (setq rtags-display-result-backend 'helm)))


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
