(require 'evil)


;; show unnecessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))


(setq-default indent-tabs-mode nil)


;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file contatining the main routine at startup
 gdb-show-main t)


;;; Reload file's buffer when the file changes on disk
(global-auto-revert-mode t)


;;; Company (complete anything) mode
(use-package company
  :init
  (progn
    (add-hook 'after-init-hook 'global-company-mode))
  :config
  (progn
    (delete 'company-semantic company-backends)
    (evil-global-set-key 'insert (kbd "C-SPC") #'company-complete)
    (define-key company-active-map (kbd "RET") 'company-complete-selection)))


;;; Quickhelp (documentation lookup) for company
(use-package company-quickhelp
  :after company
  :config
  (progn
    (setq company-quickhelp-idle-delay 1)
    (company-quickhelp-mode 1)))


;;; company backend for C/C++ headers
(use-package company-c-headers
  :after company
  :config
  (progn
    (add-to-list 'company-backends 'company-c-headers)
    (dolist (folder (file-expand-wildcards "/usr/include/c++/*"))
      (add-to-list 'company-c-headers-path-system folder))))


(use-package projectile
  :demand
  :bind
  (:map evil-normal-state-map
        ("C-p" . projectile-command-map))
  :config
  (progn
    (projectile-mode)))


(use-package eglot)


(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))


(use-package editorconfig
  :config
  (progn
    (editorconfig-mode 1)))


(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-new-buffer-setup-functions
             (cons 'emacs-lisp-mode #'semantic-default-elisp-setup))
(semantic-mode 1)
(use-package stickyfunc-enhance)

(defun rtags-hook ()
  "Setup rtags and flycheck."
  (require 'flycheck-rtags)
  (rtags-start-process-unless-running)
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)
  ;; (rtags-mode)
  )

(use-package rtags
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
    (use-package company-rtags)
    (use-package flycheck-rtags)
    (setq rtags-autostart-diagnostics t
          rtags-completions-enabled t)
    (with-eval-after-load 'company
      ;; (push 'company-rtags company-backends)
      (add-to-list 'company-backends 'company-rtags))
    (setq rtags-display-result-backend 'ivy)))


(use-package zeal-at-point
  :bind
  (("C-c d" . zeal-at-point))
  :config
  (progn
    (add-to-list 'zeal-at-point-mode-alist '(c-mode . "c"))
    (add-to-list 'zeal-at-point-mode-alist '(python-mode . "python"))))


(use-package pkgbuild-mode
  :mode ("\\`PKGBUILD\\'" . pkgbuild-mode)
  :defer t)
