;;; Change frame title
(setq frame-title-format "emacs")


;;; Font
(ignore-errors
  (set-frame-font
   (font-spec
    :name "Iosevka"
    :size 13
    :weight 'normal
    :width 'normal)
   nil t))


(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)


(menu-bar-mode 0) ; Disable menu bar
(scroll-bar-mode 0) ; Disable scroll bar
(tool-bar-mode 0) ; Disable tool bar
(blink-cursor-mode 0) ; Turn off cursor blinking
(setq visible-cursor nil) ; Turn off cursor blinking in terminals
(column-number-mode 1) ; Show column number next to line number in mode line
(show-paren-mode 1) ; Highlight parentheses
(global-hi-lock-mode 1) ; Highlight stuff with M-s h
(mouse-avoidance-mode 'none) ; Move mouse if it gets in the way of the cursor


;;; Spell check in comments and strings
(flyspell-prog-mode)


;;; Setup theme
(use-package badger-theme
  :config
  (progn
    (enable-theme 'badger)))

;;; Transparency control

(defvar new-frames-are-transparent t
  "Whether new frames should be transparent")

(defun toggle-transparency (&optional frame)
  "Toggle FRAME's transparency."
  (interactive)
  (let ((alpha (frame-parameter frame 'alpha)))
    (set-frame-parameter
     frame 'alpha
     (if (memq (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               '(nil 100))
         80 100))))

(global-set-key (kbd "C-c t") 'toggle-transparency)

(add-to-list 'after-make-frame-functions
             (lambda (frame)
               (if new-frames-are-transparent
                   (set-frame-parameter frame 'alpha 80))))
