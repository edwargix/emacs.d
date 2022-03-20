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
  (counsel-projectile-mode)
  (require 'dash)
  (setcar counsel-projectile-switch-project-action
        (1+ (--find-index (eq (cadr it) #'counsel-projectile-switch-project-action-vc) (cdr counsel-projectile-switch-project-action)))))

(use-package swiper
  :bind
  (("C-s" . swiper)
   :map Info-mode-map
   ("C-s" . isearch-forward)
   :map isearch-mode-map
   ("C-n" . isearch-repeat-forward)))
