(use-package gptel
  :config
  (setq
   gptel-model 'llama3:latest
   gptel-backend (gptel-make-ollama "Ollama"             ;Any name of your choosing
                   :host "localhost:11434"               ;Where it's running
                   :stream t                             ;Stream responses
                   :models '(llama3:latest))             ;List of models
   ))
