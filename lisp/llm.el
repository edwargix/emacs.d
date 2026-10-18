(use-package agent-shell
  :custom
  (agent-shell-prefer-viewport-interaction t)
  (agent-shell-session-restore-verbosity 'full)
  ;; The @ and / CAPFs are always registered and return
  ;; `:company-prefix-length t', so company pops on a bare @ or /
  ;; itself.  Turn off agent-shell's own popup, which calls
  ;; `completion-at-point' and opens *Completions* instead.
  (agent-shell-file-completion-enabled nil)
  (agent-shell-command-completion-enabled nil)
  :config
  (setq acp-logging-enabled t))
