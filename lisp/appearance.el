;;; Frame title
(setq frame-title-format "emacs")

;;; Font
(setq my-font-size 17)
(defun set-my-font-size (font-size)
  (interactive (list (read-number "size: " my-font-size)))
  (setq my-font-size font-size)
  (setq my-font (font-spec
                 :name "Fira Code"
                 :size font-size
                 :weight 'normal
                 :width 'normal))
  (set-frame-font my-font nil t))
(set-my-font-size my-font-size)
(ignore-errors (set-frame-font my-font nil t))
(add-to-list 'after-make-frame-functions
             (lambda (frame)
               (set-frame-font my-font nil t)))

;;; Coding systems
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;;; General UI
(blink-cursor-mode 0)        ; Turn off cursor blinking
(column-number-mode 1)       ; Show column number next to line number in mode line
(global-hi-lock-mode 1)      ; Highlight stuff with M-s h
(menu-bar-mode 0)            ; Disable menu bar
(mouse-avoidance-mode 'none) ; Move mouse if it gets in the way of the cursor
(scroll-bar-mode 0)          ; Disable scroll bar
(setq visible-cursor nil)    ; Turn off cursor blinking in terminals
(show-paren-mode 1)          ; Highlight parentheses
(tool-bar-mode 0)            ; Disable tool bar

;;; Spell checking in comments and strings
(flyspell-prog-mode)

;;; Transparency control
(defvar transparent-frame-alpha 80
  "Background opacity, in percent, used when a frame is transparent.")

(defvar new-frames-are-transparent nil
  "Whether new frames should be transparent")

(defun frame-transparent-p (&optional frame)
  "Return non-nil if FRAME's background is transparent."
  (let ((alpha (frame-parameter frame 'alpha-background)))
    (and (numberp alpha) (< alpha 100))))

;; `alpha' is a no-op under pgtk: it asks the window system for whole-window
;; opacity, which Wayland has no protocol for.  `alpha-background' is drawn by
;; Emacs itself, and keeps foreground text opaque.
(defun toggle-transparency (&optional frame)
  "Toggle FRAME's background transparency."
  (interactive)
  (set-frame-parameter frame 'alpha-background
                       (if (frame-transparent-p frame)
                           100
                         transparent-frame-alpha)))

(global-set-key (kbd "C-c t") #'toggle-transparency)

(add-to-list 'after-make-frame-functions
             (lambda (frame)
               (if new-frames-are-transparent
                   (set-frame-parameter frame 'alpha-background
                                        transparent-frame-alpha))))

;;; Page breaks
;; display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))
