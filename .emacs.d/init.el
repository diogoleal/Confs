;;; init.el --- Personal Emacs configuration
;;; Commentary:
;; This is the Emacs configuration file.

;;; Code:
;(load-theme 'tsdh-dark t)

(use-package catppuccin-theme
  :ensure t
  :init
  (setq catppuccin-flavor 'macchiato)
  :config
  (catppuccin-reload))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(use-package consult :ensure t)
(use-package auto-complete
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))
(use-package k8s-mode)
(use-package dockerfile-mode)

(use-package highlight-parentheses
  :hook
  ((minibuffer-setup . highlight-parentheses-minibuffer-setup)))

(use-package multiple-cursors
  :ensure t)

(use-package bash-completion
  :config
  (bash-completion-setup))
(use-package terraform-mode
  :custom
  (terraform-indent-level 4)
  :hook
  (terraform-mode . my-terraform-mode-init)
  :config
  (defun my-terraform-mode-init ()
    ))

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode))

(use-package pyvenv
  :config
  (pyvenv-mode t)
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))
(use-package flymake-shell
  :hook
  (sh-set-shell . flymake-shell-load))
(use-package magit)
(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(use-package dashboard
  :ensure t
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))
(setq dashboard-items '((recents   . 5)
                        (bookmarks . 5)
                        (projects  . 5)
                        (registers . 5)))
(setq dashboard-projects-backend 'projectile)
(setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-banner-title
                                  dashboard-insert-navigator
                                  dashboard-insert-init-info
                                  dashboard-insert-items
                                  dashboard-insert-newline
                                  dashboard-insert-footer))
;(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
(setq dashboard-icon-type 'all-the-icons)

;;; Treemacs Configuration

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-display-in-side-window        t
        treemacs-follow-after-init             t
        treemacs-expand-after-init             t
        treemacs-find-workspace-method         'find-for-file-or-pick-first
        treemacs-hide-dot-git-directory        t
        treemacs-indentation                   2
        treemacs-is-never-other-window         nil
        treemacs-litter-directories            '("/node_modules" "/.venv" "/.cask")
        treemacs-no-delete-other-windows       t
        treemacs-position                      'left
        treemacs-show-hidden-files             t
        treemacs-sorting                       'alphabetic-asc
        treemacs-space-between-root-nodes      t
        treemacs-width                         35
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)

  (when (and (executable-find "git") treemacs-python-executable)
    (treemacs-git-mode 'deferred))

  (use-package treemacs-projectile
    :after (treemacs projectile)
    :ensure t)

  (use-package treemacs-icons-dired
    :hook (dired-mode . treemacs-icons-dired-enable-once)
    :ensure t)

  (use-package treemacs-magit
    :after (treemacs magit)
    :ensure t)

  (use-package treemacs-persp
    :after (treemacs persp-mode)
    :ensure t
    :config (treemacs-set-scope-type 'Perspectives))

  (use-package treemacs-tab-bar
    :after treemacs
    :ensure t
    :config (treemacs-set-scope-type 'Tabs)))

(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "/usr/bin/fish")
  (setq vterm-max-scrollback 50000)
  (setq vterm-kill-buffer-on-exit t)
  )

(global-set-key (kbd "C-x <f12>")
                (lambda ()
                  (interactive)
                  (split-window-below)
                  (other-window 1)
                  (vterm)))

(global-set-key (kbd "C-x <f11>")
                (lambda ()
                  (interactive)
                  (when (eq major-mode 'vterm-mode)
                    (kill-buffer-and-window))))


(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t))

(use-package highlight-symbol
  :ensure t
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-on-navigation-p t
        highlight-symbol-idle-delay 0.5)
  )

(dolist (binding
         '(
           ("M-0"            . treemacs-select-window)
           ("C-x t 1"        . treemacs-delete-other-windows)
           ("C-x t p"        . treemacs-projectile)
           ("C-x t t"        . treemacs)
           ("C-x t d"        . treemacs-select-directory)
           ("C-x t B"        . treemacs-bookmark)
           ("C-x t C-t"      . treemacs-find-file)
           ("C-x t M-t"      . treemacs-find-tag)
           ("C-<tab>"        . other-window)
           ("M-<up>"         . enlarge-window)
           ("M-<down>"       . shrink-window)
           ("M-<right>"      . enlarge-window-horizontally)
           ("M-<left>"       . shrink-window-horizontally)
           ("C-<up>"         . mc/mark-previous-like-this)
           ("C-<down>"       . mc/mark-next-like-this)
           ("C-c C-<down>"   . mc/mark-all-like-this)
           ("C-<escape>"     . mc/keyboard-quit)
           ("C-c b k"        . close-buffers-by-extension)
           ("C-x g"          . magit-status)
           ("C-s"            . consult-line)
           ("C-c s"          . consult-ripgrep)
           ("C-x b"          . consult-buffer)
           ("M-y"            . consult-yank-pop)
           ("C-c p f"        . consult-project-extra-find)
           ("C-c p g"        . consult-project-extra-ripgrep)
           ("C-c t"          . vterm)
           ("<f9>"           . rainbow-delimiters-mode)
           )
         )
  (global-set-key (kbd (car binding)) (cdr binding)))

;;; init.el ends here
