(use-package ivy
  :demand
  :bind
  (:map evil-normal-state-map
        ("C-k" . ivy-resume))
  :config
  ;; Route `completing-read' through ivy.  Without this only the commands
  ;; counsel remaps get an ivy UI; everything reading with `completing-read'
  ;; -- all of projectile's commands, in particular -- falls back to the
  ;; default minibuffer completion.
  (ivy-mode 1))

(use-package counsel
  :demand
  :bind
  ("C-x b" . counsel-ibuffer)
  :config
  (counsel-mode))

(use-package swiper
  :bind
  (("C-s" . swiper)
   :map Info-mode-map
   ("C-s" . isearch-forward)
   :map isearch-mode-map
   ("C-n" . isearch-repeat-forward)))
