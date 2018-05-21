;;; setup-python --- Setup Python-related packages

;;; Commentary:

;; This sets up python-related packages

;;; Code:


(use-package pyvenv
  :ensure t
  :commands (pyvenv-activate pyvenv-workon))


(use-package anaconda-mode
  :ensure t
  :after python
  :config (progn
            (add-hook 'python-mode-hook 'anaconda-mode)
            (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))


(use-package company-anaconda
  :ensure t
  :after anaconda-mode)


(use-package importmagic
  :ensure t
  :config
  (add-hook 'python-mode-hook 'importmagic-mode))


(provide 'setup-python)
;;; setup-python.el ends here
