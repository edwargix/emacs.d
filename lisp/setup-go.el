;;; setup-go -- Setup Golang-related packages

;;; Commentary:

;; This sets up golang-related packages

;;; Code:


(use-package go-mode
  :ensure t
  :defer t)


(use-package company-go
  :ensure t
  :after company
  :config
  (progn
    (add-to-list 'company-backends 'company-go)))


(provide 'setup-go)
;;; setup-go.el ends here
