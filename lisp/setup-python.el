(use-package pyvenv
  :commands (pyvenv-activate pyvenv-workon))


(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :custom (pipenv-with-projectile nil)
  :commands (pipenv-activate))


(use-package python-docstring
  :hook (python-mode . (lambda () (python-docstring-mode t))))
