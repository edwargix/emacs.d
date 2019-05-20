(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :init
  (progn
    (setq web-mode-markup-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-engines-alist '(("django" . "\\.html?\\'")))
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(web-mode . ("javascript-typescript-stdio"))))))
