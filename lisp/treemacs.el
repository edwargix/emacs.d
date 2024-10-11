(use-package treemacs
  :bind ("M-0" . treemacs)
  :config
  (treemacs-project-follow-mode))

(use-package treemacs-evil
  :after treemacs)

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package lsp-treemacs
  :after lsp)
