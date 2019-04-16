(use-package go-mode
  :ensure t
  :defer t)


(use-package company-go
  :ensure t
  :after company
  :config
  (progn
    (add-to-list 'company-backends 'company-go)))
