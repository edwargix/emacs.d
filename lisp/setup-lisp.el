;;; setup-lisp --- Setup lisp-related packages

;;; Commentary:

;; This sets up lisp-related packages, regardless of dialect

;;; Code:


(use-package elisp-slime-nav
  :ensure t
  :config
  (progn
    (dolist (map `(,emacs-lisp-mode-map
                   ;; ,ielm-map
                   ,lisp-interaction-mode-map))
      (evil-define-key 'normal map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)
      (evil-define-key 'normal map (kbd "M-,") 'pop-tag-mark))))


(provide 'setup-lisp)
;;; setup-lisp.el ends here
