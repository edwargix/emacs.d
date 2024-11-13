(defun disable-current-themes ()
  "disabled all currently-enabled themes"
  (interactive)
  (dolist (thm custom-enabled-themes)
    (disable-theme thm)))

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

(defmacro use-theme (pkg themes)
  `(use-package ,pkg
     :config
     ;; load each theme
     ,@(cl-loop for thm in themes collect
                `(load-theme (quote ,thm) t t))
     ;; define an interactive function for each theme
     ,@(cl-loop for thm in themes collect
                `(defun ,(intern (format "%s-theme" thm)) ()
                   ,(format "Switch current theme to %s." thm)
                   (interactive)
                   (switch-theme (quote ,thm))))))

(use-theme gruvbox-theme (gruvbox-dark-hard
                          gruvbox-light-hard))

(use-theme monokai-theme (monokai))

(use-theme doom-themes (doom-one
                        doom-one-light
                        doom-vibrant))

(use-theme zenburn-theme (zenburn))

(defun dark-theme ()
  (interactive)
  (switch-theme 'gruvbox-dark-hard))

(defun light-theme ()
  (interactive)
  (switch-theme 'gruvbox-light-hard))

;; enable dark theme by default
(dark-theme)

(global-set-key (kbd "C-c C-t C-d") #'dark-theme)
(global-set-key (kbd "C-c C-t C-l") #'light-theme)
(global-set-key (kbd "C-c C-t C-t") #'switch-theme)
