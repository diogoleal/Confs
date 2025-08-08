;; Flycheck

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :after lsp-mode
  :config
  (flycheck-define-checker ansible-lint
    "A checker for Ansible playbooks using ansible-lint."
    :command ("ansible-lint" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ": [WARNING] " (message) line-end))
    :modes (yaml-mode))
  (add-to-list 'flycheck-checkers 'ansible-lint))

;; Integration with lsp-mode
(with-eval-after-load 'lsp-mode
  (setq lsp-diagnostics-provider :flycheck))
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode          . lsp)
         (python-mode     . lsp)
         (go-mode         . lsp)
         (sh-mode         . lsp)
         (c-mode          . lsp)
         (yaml-mode       . lsp))
  :commands lsp)

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; sudo dnf install clang-tools-extra nodejs npm
;; go install golang.org/x/tools/gopls@latest
;; npm install bash-language-server yaml-language-server pyright
;; M-x lsp

(add-hook 'prog-mode-hook #'company-mode)

(use-package company
  :ensure t
  :hook ((after-init . global-company-mode)
         (prog-mode . company-mode))
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-show-quick-access t))

(use-package yasnippet :ensure t :config (yas-global-mode 1))
(use-package consult :ensure t)
(use-package consult-project-extra :ensure t :after consult)
(use-package vertico :ensure t :config (vertico-mode))
(use-package orderless :ensure t :config (setq completion-styles '(orderless)))

(provide 'code)
;;; code.el ends here
