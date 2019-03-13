;;; setup-org.el --- Setup Org-mode packages

;;; Commentary:

;; This sets up org-mode related packages

;;; Code:


;;; Org mode for keeping notes, todo lists, planning, and fast
;;; documenting
(use-package org
  :ensure org-plus-contrib
  :init
  (progn
    (when (file-exists-p "~/org/setup.el")
      (load-file "~/org/setup.el"))
    (setq org-default-notes-file "~/org/notes.org"
          org-return-follows-link t
          org-read-date-force-compatible-dates nil)
    (setq org-src-fontify-natively t
          org-src-tab-acts-natively t)
    (setq org-latex-compiler "xelatex"
          org-latex-pdf-process
          '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")))
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-iswitchb)
   ("C-c l" . org-store-link)
   ("C-c o" . org-open-at-point-global))
  :config
  (progn
    (require 'ox-md)
    (require 'ox-beamer)
    ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
    ;; (setq org-latex-listings 'minted)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (dot . t)
       (org . t)
       (gnuplot . t)))))


;;; UTF-8 bullets for org-mode
(use-package org-bullets
  :ensure t
  :after org
  :config
  (progn
    (add-hook 'org-mode-hook 'org-bullets-mode)))


(use-package org-contacts
  :after org
  :config
  (progn
    (setq org-contacts-files '("~/org/contacts.org"))))


(provide 'setup-org)
;;; setup-org.el ends here
