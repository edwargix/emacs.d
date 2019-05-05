(use-package ivy)


(use-package counsel
  :defer nil
  :bind
  ("C-x b" . counsel-ibuffer)
  :config
  (progn
    (counsel-mode)))


(use-package counsel-projectile
  :config
  (progn
    (counsel-projectile-mode)))


(use-package swiper
  :bind
  ("C-s" . swiper))
