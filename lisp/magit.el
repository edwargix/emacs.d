;; magit: a Git Porcelain
(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-c g b" . magit-blame)
   ("C-c g l" . magit-log)))

(use-package forge
  :after magit)
