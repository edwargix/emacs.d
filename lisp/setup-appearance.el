;;; setup-appearance --- Setup look of Emacs

;;; Commentary:

;; This sets up the look of Emacs to my liking

;;; Code:


;;; Change frame title
(setq frame-title-format "emacs")


;;; Font
(ignore-errors
  (set-frame-font
   (font-spec
    :name "Source Code Pro"
    :size 13
    :weight 'normal
    :width 'normal)
   nil t))


(menu-bar-mode 0) ; Disable menu bar
(scroll-bar-mode 0) ; Disable scroll bar
(tool-bar-mode 0) ; Disable tool bar
(blink-cursor-mode 0) ; Turn off cursor blinking
(column-number-mode 1) ; Show column number next to line number in mode line
(show-paren-mode 1) ; Highlight parentheses
(global-hi-lock-mode 1) ; Highlight stuff with M-s h
(mouse-avoidance-mode 'animate) ; Move mouse if it gets in the way of the cursor


;;; Spell check in comments and strings
(flyspell-prog-mode)


;;; Setup theme
(unless (package-installed-p 'doom-themes)
  (package-install 'doom-themes))

(use-package doom-themes
  :ensure t
  :init
  (progn
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t))
  :config
  (progn
    (when (display-graphic-p)
      (load-theme 'doom-city-lights t)
      (doom-themes-visual-bell-config)
      (doom-themes-treemacs-config)
      (doom-themes-org-config))))


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
