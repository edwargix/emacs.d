(use-package scala-mode
  :hook (scala-mode . lsp)
  :custom
  (scala-indent:step 4))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
