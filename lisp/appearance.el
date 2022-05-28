;; Change frame title
(setq frame-title-format "emacs")

;; Font
(ignore-errors
  (set-frame-font
   (font-spec
    :name "Fira Code"
    :size 13
    :weight 'normal
    :width 'normal)
   nil t))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(blink-cursor-mode 0)        ; Turn off cursor blinking
(column-number-mode 1)       ; Show column number next to line number in mode line
(global-hi-lock-mode 1)      ; Highlight stuff with M-s h
(menu-bar-mode 0)            ; Disable menu bar
(mouse-avoidance-mode 'none) ; Move mouse if it gets in the way of the cursor
(scroll-bar-mode 0)          ; Disable scroll bar
(setq visible-cursor nil)    ; Turn off cursor blinking in terminals
(show-paren-mode 1)          ; Highlight parentheses
(tool-bar-mode 0)            ; Disable tool bar

;; Spell check in comments and strings
(flyspell-prog-mode)

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

;; Transparency control
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

(global-set-key (kbd "C-c t") #'toggle-transparency)

(add-to-list 'after-make-frame-functions
             (lambda (frame)
               (if new-frames-are-transparent
                   (set-frame-parameter frame 'alpha 80))))

;; display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))
