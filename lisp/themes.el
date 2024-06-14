;; Setup theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t t)
  (load-theme 'gruvbox-light-hard t t)

  (defun dark-theme ()
    (interactive)
    (disable-theme 'gruvbox-light-hard)
    (enable-theme 'gruvbox-dark-hard))

  (defun light-theme ()
    (interactive)
    (disable-theme 'gruvbox-dark-hard)
    (enable-theme 'gruvbox-light-hard))

  ;; dark theme by default
  (dark-theme))

(use-package monokai-theme
  :config
  (load-theme 'monokai t t))

(use-package doom-themes
  :config
  (load-theme 'doom-one t t)
  (load-theme 'doom-one-light t t)
  (load-theme 'doom-vibrant t t))
