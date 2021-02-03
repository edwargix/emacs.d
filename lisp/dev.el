(require 'evil)

;; show unnecessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace t)))

(setq-default indent-tabs-mode nil)

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file contatining the main routine at startup
 gdb-show-main t)

;; Reload file's buffer when the file changes on disk
(global-auto-revert-mode t)

;; Company (complete anything) mode
(use-package company
  :hook
  (after-init . global-company-mode)
  :config
  (delete 'company-semantic company-backends)
  (evil-global-set-key 'insert (kbd "C-SPC") #'company-complete)
  (define-key company-active-map (kbd "RET") #'company-complete-selection)
  (define-key company-active-map (kbd "M-<") #'company-select-first)
  (define-key company-active-map (kbd "M->") #'company-select-last))

;; Quickhelp (documentation lookup) for company
(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1))

;; company backend for C/C++ headers
(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (dolist (folder (file-expand-wildcards "/usr/include/c++/*"))
    (add-to-list 'company-c-headers-path-system folder)))

(use-package projectile
  :demand
  :bind
  (:map evil-normal-state-map
        ("C-p" . projectile-command-map)
        ("C-p C-p" . projectile-switch-project))
  :config
  (projectile-mode))

(use-package eglot
  :defer t)

(use-package lsp-mode
  :defer t
  :custom
  ;; this does conflict with an Org Mode binding, but lsp is useless in org-mode
  (lsp-keymap-prefix "C-c C-l")
  (lsp-prefer-flymake nil)
  :config
  (evil-collection-define-key 'normal 'lsp-mode-map
    "gd" 'lsp-find-definition
    (kbd "C-t") 'xref-pop-marker-stack
    "K" 'lsp-describe-thing-at-point))

(use-package company-lsp
  :after (company lsp-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package cc-mode
  :straight nil
  :hook
  (c-mode . (lambda ()
              (setq-local indent-tabs-mode t)
              (setq-local c-basic-offset 8)))
  :init
  (setq-default c-file-style "linux"))

(use-package semantic
  :straight nil
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (add-to-list 'semantic-new-buffer-setup-functions
               (cons 'emacs-lisp-mode #'semantic-default-elisp-setup))
  (semantic-mode 1))


(use-package stickyfunc-enhance)

(defun rtags-hook ()
  "Setup rtags and flycheck."
  (require 'flycheck-rtags)
  (rtags-start-process-unless-running)
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil))

(use-package rtags
  :straight nil
  :hook
  (c-mode . rtags-hook)
  (c++-mode . rtags-hook)
  (objc-mode . rtags-hook)
  :config
  (dolist (map (list c-mode-map c++-mode-map objc-mode-map))
    (evil-define-key 'normal map (kbd "gd") 'rtags-find-symbol-at-point)
    (evil-define-key 'normal map (kbd "M-?") 'rtags-find-references-at-point)
    (evil-define-key 'normal map (kbd "C-<") 'rtags-find-virtuals-at-point)
    (evil-define-key 'normal map (kbd "C->") 'rtags-diagnostics)
    (evil-define-key 'normal map (kbd "C-t") 'rtags-location-stack-back)
    (evil-define-key 'normal map (kbd "M-[") 'rtags-location-stack-back)
    (evil-define-key 'normal map (kbd "M-]") 'rtags-location-stack-forward))
  (use-package company-rtags
    :straight nil)
  (use-package flycheck-rtags
    :straight nil)
  (setq rtags-autostart-diagnostics t
        rtags-completions-enabled t)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-rtags))
  (setq rtags-display-result-backend 'ivy))

(use-package pkgbuild-mode
  :mode ("\\`PKGBUILD\\'"
         "APKBUILD")
  :hook (pkgbuild-mode . (lambda ()
                           ; only APKBUILDs consistently use tabs of 8 columns
                           (when (string= (buffer-name (current-buffer))
                                          "APKBUILD")
                             (setq-local indent-tabs-mode t)
                             (setq-local sh-basic-offset 8)))))

(use-package ebuild-mode
  :defer t)

(use-package nginx-mode
  :defer t)

(use-package smarty-mode
  :defer t)
