;;; setup-packages --- Setup packages
;;;
;;; Commentary:

;; This sets up package archives / preliminaries for later config

;;; Code:


(require 'package)
(require 'tls)


(setq tls-checktrust t)


(setq-default package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))


;;; Add org and melpa package archives
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (melpa-url (concat (if no-ssl "http" "https") "://melpa.org/packages/"))
       (org-url (concat (if no-ssl "http" "https") "://orgmode.org/elpa/")))
  (add-to-list 'package-archives (cons "melpa" melpa-url))
  (add-to-list 'package-archives (cons "org" org-url)))


;;; Load and activate lisp packages
(package-initialize)


;;; Fetch the list of available packages
(unless package-archive-contents
  (package-refresh-contents))


;;; Install use-package for easy package configuration
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


(require 'use-package)

(add-to-list 'Info-directory-list "~/.local/share/info/")
;;; setup-packages.el ends here
