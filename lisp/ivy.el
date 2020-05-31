(use-package ivy
  :bind
  (:map evil-normal-state-map
        ("C-k" . ivy-resume)))

(use-package counsel
  :demand
  :bind
  ("C-x b" . counsel-ibuffer)
  :config
  (counsel-mode))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package swiper
  :bind
  (("C-s" . swiper)
   :map Info-mode-map
   ("C-s" . isearch-forward)
   :map isearch-mode-map
   ("C-n" . isearch-repeat-forward)))
