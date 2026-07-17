(defun my-agent-shell-company-trigger ()
  "Pop up company for agent-shell @ and / completions at a word boundary.
`company-manual-begin' ignores `company-minimum-prefix-length', so the
command list appears even with no characters typed after the `/'."
  (when (and (memq (char-before) '(?@ ?/))
             (or (= (point) (1+ (line-beginning-position)))
                 (memq (char-before (1- (point))) '(?\s ?\t ?\n))))
    (company-manual-begin)))

(defun my-agent-shell-use-company-completion ()
  "Route agent-shell @/ completion through company.
agent-shell's own trigger calls `completion-at-point', which opens the
default *Completions* buffer from `post-self-insert-hook' before company's
`post-command-hook' trigger runs, so company never fires on a bare `/'.
Swap it for a trigger that invokes company directly.  Runs from
`agent-shell-completion-mode-hook', after that mode installs its trigger.

Guarded on the (private) `agent-shell--trigger-completion-at-point' still
being installed on the hook: if agent-shell renames or drops it, this
no-ops and leaves upstream's completion untouched rather than
double-triggering."
  (when (and agent-shell-completion-mode
             (memq #'agent-shell--trigger-completion-at-point
                   post-self-insert-hook))
    (remove-hook 'post-self-insert-hook
                 #'agent-shell--trigger-completion-at-point t)
    (add-hook 'post-self-insert-hook
              #'my-agent-shell-company-trigger nil t)))

(use-package agent-shell
  :custom
  (agent-shell-prefer-viewport-interaction t)
  (agent-shell-session-restore-verbosity 'full)
  :hook (agent-shell-completion-mode . my-agent-shell-use-company-completion)
  :config
  (setq acp-logging-enabled t))

(use-package gptel
  :config
  (setq
   gptel-model 'llama3:latest
   gptel-backend (gptel-make-ollama "Ollama"             ;Any name of your choosing
                   :host "localhost:11434"               ;Where it's running
                   :stream t                             ;Stream responses
                   :models '(llama3:latest))             ;List of models
   ))
