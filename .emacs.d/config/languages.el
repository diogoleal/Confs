;;; languages.el --- languages                       -*- lexical-binding: t; -*-

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; bash-completion
(use-package bash-completion
  :config
  (bash-completion-setup))

;; terraform-mode
(use-package terraform-mode
  :custom
  (terraform-indent-level 4)
  :hook
  (terraform-mode . my-terraform-mode-init)
  :config
  (defun my-terraform-mode-init ()))

;; YAML mode com melhorias
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook ((yaml-mode . ansible)
         (yaml-mode . flycheck-mode)))

;; Ansible mode
(use-package ansible
  :hook (yaml-mode . ansible-mode))

(use-package ansible-doc
  :after ansible
  :hook (yaml-mode . ansible-doc-mode))


;; Opcional: Execução via compile
(defun my/ansible-run-playbook ()
  "Run ansible-playbook on the current file."
  (interactive)
  (let ((playbook (buffer-file-name)))
    (compile (concat "ansible-playbook " playbook))))
;; Atalho para rodar playbook
;(global-set-key (kbd "C-c a r") 'my/ansible-run-playbook)


(use-package k8s-mode
  :ensure t
  :after yasnippet
  :mode ("\\.k8s\\.yaml\\'" . k8s-mode)
  :hook (k8s-mode . yas-minor-mode))

(add-hook 'k8s-mode-hook #'company-mode)
(add-hook 'k8s-mode-hook #'yas-minor-mode)

;; Groovy
(add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode))
(use-package groovy-mode
  :ensure t
  :mode ("Jenkinsfile\\'" . groovy-mode)
  :config
  (setq groovy-indent-offset 4))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package htmlize :ensure t)

(provide 'languages)
