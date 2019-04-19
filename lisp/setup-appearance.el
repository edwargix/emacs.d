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
(use-package darktooth-theme)


(defun toggle-transparency ()
  "Toggle the current frame's transparency."
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
