(use-package web-mode
  :mode ("\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.dhtml\\'"
         "\\.djhtml\\'"
         "\\.erb\\'"
         "\\.html?\\'"
         "\\.js\\'"
         "\\.jsx\\'"
         "\\.mustache\\'"
         "\\.phtml\\'"
         "\\.tmpl\\'"
         "\\.tpl\\.php\\'"
         "\\.ts\\'"
         "\\.tsx\\'"
         "\\.vue\\'"
         "\\.xhtml\\'")
  :custom
  (web-mode-content-types-alist '(("jsx"  . ".*\\.js[x]?\\'")))
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-engines-alist '(("django" . "\\.html?\\'")))
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(web-mode . ("javascript-typescript-stdio")))))
