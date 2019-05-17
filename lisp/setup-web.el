(use-package js2-mode
  :mode (("\\.m?js\\'"  . js2-mode)))

(defun react-inside-string-q ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (let ((result (nth 3 (syntax-ppss))))
    result))

(defun react-inside-comment-q ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (let ((result (nth 4 (syntax-ppss))))
    result))

(defun react-inside-string-or-comment-q ()
  "Return non-nil if point is inside string, documentation string or a comment.
If optional argument P is present, test this instead of point."
  (or (react-inside-string-q)
      (react-inside-comment-q)))

(use-package rjsx-mode
  :defer t
  :init
  (progn
    ;; enable rjsx mode by using magic-mode-alist
    (defun +javascript-jsx-file-p ()
      (and buffer-file-name
           (or (equal (file-name-extension buffer-file-name) "js")
               (equal (file-name-extension buffer-file-name) "jsx"))
           (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
                              magic-mode-regexp-match-limit t)
           (progn (goto-char (match-beginning 1))
                  (not (react-inside-string-or-comment-q)))))

    (add-to-list 'magic-mode-alist (cons #'+javascript-jsx-file-p 'rjsx-mode))))
