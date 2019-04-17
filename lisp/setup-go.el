(use-package go-mode
  :defer t)


(use-package company-go
  :after company
  :config
  (progn
    (add-to-list 'company-backends 'company-go)))
