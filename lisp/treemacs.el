(use-package treemacs
  :bind ("M-0" . treemacs))

(use-package treemacs-evil
  :after treemacs)

(use-package treemacs-projectile
  :after (treemacs projectile))
