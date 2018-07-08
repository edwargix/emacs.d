;;; setup-appearance --- Setup look of Emacs

;;; Commentary:

;; This sets up the look of Emacs to my liking

;;; Code:


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


;;; Highlight stuff with M-s h
(global-hi-lock-mode 1)


;;; Move mouse if it gets in the way of the cursor
(mouse-avoidance-mode 'animate)


;;; Spell check in comments and strings
(flyspell-prog-mode)


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


(use-package pretty-mode
  :ensure t
  :hook ((emacs-lisp-mode python) . turn-on-pretty-mode))


(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)


(provide 'setup-appearance)
;;; setup-appearance.el ends here
