(add-hook 'python-mode-hook #'lsp)


(use-package pyvenv
  :ensure t
  :commands (pyvenv-activate pyvenv-workon))


(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :commands (pipenv-activate))
