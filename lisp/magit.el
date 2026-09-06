;; magit: a Git Porcelain
(use-package magit
  :custom
  (magit-diff-fontify-hunk 'all)
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-c g b" . magit-blame)
   ("C-c g l" . magit-log)))

(use-package sqlite3)

(use-package forge
  :after (magit sqlite3))
