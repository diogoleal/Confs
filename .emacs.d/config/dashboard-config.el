;;; Project organization

(use-package neotree
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Workspace/" "~/org/")
	      projectile-switch-project-action 'neotree-projectile-action
	      projectile-indexing-method 'alien
	      projectile-use-git-grep 1))

(projectile-register-project-type 'python-toml '("pyproject.toml")
                                  :project-file "pyproject.toml"
                                  :compile "poetry build"
                                  :test "task test"
                                  :test-prefix "test_"
                                  :test-suffix "_test")

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-items '((recents . 5)
			  (projects . 10))
        dashboard-set-heading-icons t
	      dashboard-projects-backend 'projectile
	      dashboard-set-init-info nil)
  :config
  (dashboard-setup-startup-hook))

(use-package page-break-lines
  :ensure t
  :hook (dashboard-mode . page-break-lines-mode))

(provide 'dashboard-config)
;;; dashboard-config.el ends here
