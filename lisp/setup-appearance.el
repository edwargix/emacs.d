;;; Change frame title
(setq frame-title-format "emacs")

;;; Font
(set-frame-font
 (font-spec
  :name "Source Code Pro"
  :size 13
  :weight 'normal
  :width 'normal)
 nil t)

;;; Disable menu bar
(menu-bar-mode 0)

;;; Disable scroll bar
(scroll-bar-mode 0)

;;; Disable tool bar
(tool-bar-mode 0)

;;; Turn off cursor blinking
(blink-cursor-mode 0)

;;; Show column number next to line number in mode line
(column-number-mode)

;;; Highlight parentheses
(show-paren-mode)

;;; Custom themes
;; Default theme
(unless (package-installed-p 'monokai-theme)
  (package-install 'monokai-theme))
(load-theme 'monokai t t)
(when (display-graphic-p)
  (enable-theme 'monokai))
;; Alternate theme (bright)
(unless (package-installed-p 'leuven-theme)
  (package-install 'leuven-theme))
(load-theme 'leuven t t)
;; Theme for late-at-night
(unless (package-installed-p 'solarized-theme)
  (package-install 'solarized-theme))
(load-theme 'solarized-dark t t)
