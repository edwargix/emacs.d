;;; Emacs LISP

(use-package elisp-slime-nav
  :after evil
  :config
  (dolist (map (list emacs-lisp-mode-map
                     ;; ,ielm-map
                     lisp-interaction-mode-map))
    (evil-define-key 'normal map (kbd "g d") #'elisp-slime-nav-find-elisp-thing-at-point)
    (evil-define-key 'normal map (kbd "M-?") #'xref-find-references)
    (evil-define-key 'normal map (kbd "K") #'elisp-slime-nav-describe-elisp-thing-at-point)))

(use-package auto-compile
  :defer t
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-use-mode-line nil)
  :hook
  (emacs-lisp-mode . auto-compile-mode))

;;; Common LISP

(use-package slime
  :custom
  (inferior-lisp-program "sbcl")
  :config
  (add-to-list 'slime-contribs 'slime-scratch)
  (slime-setup))

(use-package common-lisp-snippets
  :after (yasnippet slime))

;;; Racket

(use-package racket-mode
  :defer t)

(use-package scribble
  :straight nil
  :load-path "contrib/"
  :mode ("\\.scrbl\\'" . scribble-mode))

;;; Clojure

(use-package cider
  :custom
  (cider-prompt-for-symbol nil))
