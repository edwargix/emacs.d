;;; init.el --- Setup Emacs

;;; Commentary:

;; My personal init.el for Emacs.

;;; Code:

;; Install local user packages
(dolist (d (apply #'append (mapcar #'file-expand-wildcards
                                   '("/usr/share/emacs/site-lisp/*"
                                     "/usr/local/share/emacs/site-lisp/*"
                                     "~/.local/share/emacs/site-lisp/*"))))
  (add-to-list 'load-path d))

;; Loading of personal config files.  The top two files have priority
(load "~/.emacs.d/lisp/packages")
(load "~/.emacs.d/lisp/keys")

(load "~/.emacs.d/lisp/appearance")
(load "~/.emacs.d/lisp/dev")
(load "~/.emacs.d/lisp/docker")
(load "~/.emacs.d/lisp/go")
(load "~/.emacs.d/lisp/ivy")
(load "~/.emacs.d/lisp/java")
(load "~/.emacs.d/lisp/kubernetes")
(load "~/.emacs.d/lisp/lisps")
(load "~/.emacs.d/lisp/misc")
(load "~/.emacs.d/lisp/org")
(load "~/.emacs.d/lisp/python")
(load "~/.emacs.d/lisp/rust")
(load "~/.emacs.d/lisp/scala")
(load "~/.emacs.d/lisp/terraform")
(load "~/.emacs.d/lisp/treemacs")
(load "~/.emacs.d/lisp/web")

;; Start Emacs Daemon
(require 'server)
(unless (server-running-p)
  (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(fill-column 80)
 '(initial-buffer-choice t)
 '(initial-scratch-message "")
 '(url-privacy-level (quote paranoid))
 '(make-backup-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
