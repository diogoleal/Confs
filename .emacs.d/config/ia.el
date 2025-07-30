;; ollama-setup.el --- Emacs integration with local Ollama LLM -*- lexical-binding: t; -*-

(require 'llm)
(require 'company)
(require 'yasnippet)
(require 'expand-region)

(setq llm-backend 'ollama)
(setq llm-ollama-model "llama3")
(setq llm-request-timeout 60)
(setq llm-chat-save-file "~/.emacs.d/llm-chat-history.org")

(global-set-key (kbd "C-c o") #'llm-chat)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Ativa modos
(add-hook 'after-init-hook 'global-company-mode)
(yas-global-mode 1)

(defun my/ollama-completion-at-point ()
  "Use LLM via Ollama to provide completions at point."
  (when (and (bound-and-true-p company-mode)
             (memq major-mode '(emacs-lisp-mode python-mode go-mode c-mode)))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (list (car bounds)
              (cdr bounds)
              (lambda (prefix)
                (llm-complete prefix :backend 'ollama))
              :exclusive 'no)))))

(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions #'my/ollama-completion-at-point nil t)))

(provide 'ollama-setup)
;;; ollama-setup.el ends here
