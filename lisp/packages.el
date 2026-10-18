;; The following bootstrap code was copied from the "Getting started" section of
;; straight.el's README.md
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; end of copied code

(setq straight-use-package-by-default t)
(straight-use-package '(use-package :type built-in))

(with-eval-after-load 'info
  ;; Initialize first, otherwise the default directories are never added
  (info-initialize)
  (add-to-list 'Info-directory-list "~/.local/share/info/"))

;; Packages held back from upstream HEAD.  `my/straight-bump' checks these
;; commits out after pulling, so a bump can't silently undo them.  Each entry is
;; (PACKAGE COMMIT REASON); drop it once upstream is fixed.
(defvar my/straight-holds
  '(("forge" "4233cbba4f566988d78cdb9c5f85af8a171f518d"
     "74f3d04 binds magit-repolist-mode-map before magit-repos defines it"))
  "Packages to keep at a fixed commit across `my/straight-bump'.")

(defun my/straight-apply-holds ()
  "Check out the commits listed in `my/straight-holds'."
  (dolist (hold my/straight-holds)
    (let* ((repo (car hold))
           (commit (cadr hold))
           (recipe (gethash repo straight--recipe-cache)))
      (if (not recipe)
          (message "Hold on %s ignored: package not registered" repo)
        (unless (straight-vc-commit-present-p recipe commit)
          (straight-vc-fetch-from-remote recipe))
        (straight-vc-check-out-commit recipe commit)
        (message "Held %s at %s (%s)" repo (substring commit 0 7)
                 (nth 2 hold))))))

(defun my/straight-bump ()
  "Pull all packages, re-apply holds, freeze, and stage the lockfile."
  (interactive)
  (straight-pull-all)
  (my/straight-apply-holds)
  (straight-freeze-versions)
  (let ((default-directory user-emacs-directory))
    (call-process "git" nil nil nil "add" "straight/versions")
    (magit-status default-directory))
  (message "Lockfile staged; restart Emacs to load the new versions"))
