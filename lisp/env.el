;; Seed the daemon's base environment from the login shell ONCE at startup.  The
;; daemon does not inherit an interactive shell's PATH, so this puts the tools that
;; back the per-project layer (direnv, and e.g. nix) plus GOPATH/SSH_AUTH_SOCK on
;; PATH, both for buffers without a project environment and so envrc can shell out
;; to direnv.  Must precede envrc below.
(use-package exec-path-from-shell
  :demand t
  :config
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
  (exec-path-from-shell-initialize))

;; inheritenv must load before envrc: envrc declares it as a dependency and wraps
;; its own direnv subprocess calls with the `inheritenv' macro.  Loading inheritenv
;; installs no global advice by itself -- it only provides the `inheritenv' macro
;; and `inheritenv-add-advice'/`inheritenv-apply' to opt individual commands in.
;; Commands that spawn with the file buffer current (e.g. lsp-mode via lsp-deferred)
;; already inherit the buffer-local environment directly; add advice only for a
;; command that spawns from a `with-temp-buffer' and thus loses the project PATH:
;;   (inheritenv-add-advice 'the-leaky-command)
(use-package inheritenv
  :demand t)

(use-package envrc
  :after inheritenv
  :demand t
  :config
  ;; Enable as the LAST globalized minor mode so envrc-mode is positioned to run
  ;; FIRST in each buffer's setup, before lsp/flycheck read the environment.
  ;; Globalized modes prepend their per-buffer setup to after-change-major-mode-hook,
  ;; so "enabled last" == "runs first per buffer".
  (envrc-global-mode))
