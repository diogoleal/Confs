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
;; Syntax highlighting para Jenkinsfile
(use-package groovy-mode
  :mode ("Jenkinsfile\\'" . groovy-mode)
  :config
  (setq groovy-indent-offset 4))

(use-package highlight-parentheses
  :hook
  ((minibuffer-setup . highlight-parentheses-minibuffer-setup)))

(use-package multiple-cursors :ensure t)

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

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t))

;; (use-package visual-fill-column
;;   :ensure t
;;   :hook (markdown-mode . visual-line-mode)
;;   :hook (markdown-mode . visual-fill-column-mode)
;;   :config
;;   (setq visual-fill-column-width 90
;;         visual-fill-column-center-text t))

(setq markdown-fontify-code-blocks-natively t)

(use-package emojify
  :ensure t
  :hook (markdown-mode . emojify-mode))

(use-package nerd-icons :ensure t)

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner 'official
        dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)))
  (dashboard-setup-startup-hook))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-no-png-images t
        treemacs-width 32
        treemacs-indentation 2)
  (add-hook 'emacs-startup-hook #'treemacs))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package magit
  :ensure t
  :commands magit-status)

(global-set-key (kbd "C-x g")
                (lambda ()
                  (interactive)
                  (magit-status)
                  (treemacs-select-window)))

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
