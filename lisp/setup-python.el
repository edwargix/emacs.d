(use-package pyvenv
  :commands (pyvenv-activate pyvenv-workon))


(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :commands (pipenv-activate))
