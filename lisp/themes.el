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

(defun switch-theme (theme)
  ;; The (interactive ...) and subsequent (unless ...) code was copied from the
  ;; enable-theme function in custom.el.gz
  (interactive (list (intern
                      (completing-read
                       "Switch to custom theme: "
                       obarray (lambda (sym) (get sym 'theme-settings)) t))))
  (unless (custom-theme-p theme)
    (error "Undefined Custom theme %s" theme))
  ;; end of copied code

  (disable-current-themes)
  (enable-theme theme))

(defun disable-current-themes ()
  "disabled all currently-enabled themes"
  (interactive)
  (dolist (thm custom-enabled-themes)
    (disable-theme thm)))
