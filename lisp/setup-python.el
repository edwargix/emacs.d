;;; setup-python --- Setup Python-related packages

;;; Commentary:

;; This sets up python-related packages

;;; Code:


(add-hook 'python-mode-hook #'lsp)


(use-package pyvenv
  :ensure t
  :commands (pyvenv-activate pyvenv-workon))


(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :commands (pipenv-activate))


(provide 'setup-python)
;;; setup-python.el ends here
