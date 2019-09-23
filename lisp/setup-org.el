;;; Org mode for keeping notes, todo lists, planning, and fast
;;; documenting
(use-package org
  :straight org-plus-contrib
  :init
  (progn
    (if (not (file-exists-p "~/org"))
        (make-directory "~/org"))
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
   ("C-c o" . org-open-at-point-global)
   ("C-c M-t i" . org-timer)
   ("C-c M-t c" . org-timer-pause-or-continue)
   ("C-c M-t r" . org-timer-stop) ; r for reset
   ("C-c M-t s" . org-timer-start)))


;;; UTF-8 bullets for org-mode
(use-package org-bullets
  :after org
  :config
  (progn
    (add-hook 'org-mode-hook 'org-bullets-mode)))


(use-package org-contacts
  :straight nil
  :after org
  :config
  (progn
    (setq org-contacts-files '("~/org/contacts.org"))))
