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

(use-theme darktooth-theme (darktooth
                            darktooth-dark
                            darktooth-darker))

(use-theme color-theme-modern (pierson))


(use-theme doom-themes (doom-1337
                        doom-acario-dark
                        doom-acario-light
                        doom-ayu-dark
                        doom-ayu-light
                        doom-ayu-mirage
                        doom-badger
                        doom-challenger-deep
                        doom-city-lights
                        doom-dark+
                        doom-dracula
                        doom-earl-grey
                        doom-ephemeral
                        doom-fairy-floss
                        doom-feather-dark
                        doom-feather-light
                        doom-flatwhite
                        doom-gruvbox
                        doom-gruvbox-light
                        doom-henna
                        doom-homage-black
                        doom-homage-white
                        doom-horizon
                        doom-Iosvkem
                        doom-ir-black
                        doom-lantern
                        doom-laserwave
                        doom-manegarm
                        doom-material
                        doom-material-dark
                        doom-meltbus
                        doom-miramare
                        doom-molokai
                        doom-monokai-classic
                        doom-monokai-machine
                        doom-monokai-octagon
                        doom-monokai-pro
                        doom-monokai-ristretto
                        doom-monokai-spectrum
                        doom-moonlight
                        doom-nord
                        doom-nord-aurora
                        doom-nord-light
                        doom-nova
                        doom-oceanic-next
                        doom-oksolar-dark
                        doom-oksolar-light
                        doom-old-hope
                        doom-one
                        doom-one-light
                        doom-opera
                        doom-opera-light
                        doom-outrun-electric
                        doom-palenight
                        doom-peacock
                        doom-pine
                        doom-plain
                        doom-plain-dark
                        doom-rouge
                        doom-shades-of-purple
                        doom-snazzy
                        doom-solarized-dark
                        doom-solarized-dark-high-contrast
                        doom-solarized-light
                        doom-sourcerer
                        doom-spacegrey
                        doom-tokyo-night
                        doom-tomorrow-day
                        doom-tomorrow-night
                        doom-vibrant
                        doom-wilmersdorf
                        doom-xcode
                        doom-zenburn))

(use-theme gruvbox-theme (gruvbox-dark-hard
                          gruvbox-light-hard))

(use-theme material-theme (material
                           material-light))

(use-theme monokai-theme (monokai))

(use-theme soothe-theme (soothe
                         soothe-darker
                         soothe-obsidian))

(use-theme zenburn-theme (zenburn))

(defalias 'no-themes #'disable-current-themes)
(defalias 'dark-theme #'gruvbox-dark-hard-theme)
(defalias 'light-theme #'gruvbox-light-hard-theme)

;; defalut theme
(zenburn-theme)
