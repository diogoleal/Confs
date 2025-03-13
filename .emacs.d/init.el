(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Global configs
(set-face-attribute 'default nil :font "Hack" :height 100)
(load-theme 'tsdh-dark t)
(show-paren-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(savehist-mode 1)
(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(global-display-line-numbers-mode)

;; auto-complete
(use-package auto-complete
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

;; k8s-mode
(use-package k8s-mode)

;; dockerfile-mode
(use-package dockerfile-mode)

;; highlight-parentheses
(use-package highlight-parentheses
  :hook
  ((minibuffer-setup . highlight-parentheses-minibuffer-setup)))

;; global shortcuts
(global-set-key [f9] 'rainbow-delimiters-mode)
(global-set-key [f2] 'neotree-toggle)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)

;; multiple-cursors
(use-package multiple-cursors
  :bind
  (("C-<up>" . mc/mark-previous-like-this)
   ("C-<down>" . mc/mark-next-like-this)
   ("C-c C-<down>" . mc/mark-all-like-this)
   ("C-<escape>" . mc/keyboard-quit)))

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
  (defun my-terraform-mode-init ()
    ))

;; flycheck
(use-package flycheck
  :hook
  (after-init . global-flycheck-mode))

;; pyvenv
(use-package pyvenv
  :config
  (pyvenv-mode t)
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

;; flymake-shell
(use-package flymake-shell
  :hook
  (sh-set-shell . flymake-shell-load))

;; magit
(use-package magit
  :bind ("C-x g" . magit-status))

;; git-gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;; backup
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; ;; Integration with GNOME
;; (use-package dbus
;;   :config
;;   (defun my-register-signals (client-path)
;;     "Registrar para os sinais 'QueryEndSession' e 'EndSession' do Gnome SessionManager."
;;     (setq my-gnome-client-path client-path)
;;     (let ((end-session-response
;;            (lambda (&optional arg)
;;              (dbus-call-method-asynchronously
;;               :session "org.gnome.SessionManager" my-gnome-client-path
;;               "org.gnome.SessionManager.ClientPrivate" "EndSessionResponse"
;;               nil t "")))))
;;       (dbus-register-signal
;;        :session "org.gnome.SessionManager" client-path
;;        "org.gnome.SessionManager.Client" "QueryEndSession"
;;        `(lambda ()
;;           (,end-session-response)
;;           t))
;;       (dbus-register-signal
;;        :session "org.gnome.SessionManager" client-path
;;        "org.gnome.SessionManager.Client" "EndSession"
;;        `(lambda ()
;;           (add-hook 'kill-emacs-hook ,end-session-response t)
;;           (kill-emacs)))))

;;   (defun my-gnome-register ()
;;     "Registrar este Emacs como cliente do GNOME SessionManager."
;;     (dbus-call-method-asynchronously
;;      :session "org.gnome.SessionManager" "/org/gnome/SessionManager"
;;      "org.gnome.SessionManager" "RegisterClient"
;;      #'my-register-signals (user-login-name) (system-name)))

;;   (when (and (eq window-system 'x)
;;              (string-match "GNOME" (shell-command-to-string "echo $XDG_CURRENT_DESKTOP")))
;;     (my-gnome-register))

;; enable tab-bar-mode
(setq tab-bar-show 1)
(tab-bar-mode 1)

(setq tab-bar-format '(tab-bar-format-history
                       tab-bar-format-tabs
                       tab-bar-separator
                       tab-bar-format-add-tab))

;; Shortcus
(global-set-key (kbd "C-x t n") 'tab-new)
(global-set-key (kbd "C-x t c") 'tab-close)
(global-set-key (kbd "C-x t o") 'tab-next)
(global-set-key (kbd "C-x t p") 'tab-previous)
(global-set-key (kbd "C-x t r") 'tab-rename)
(global-set-key (kbd "C-x t s") 'tab-switch)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; vterm
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

;; shortcut vterm
(global-set-key (kbd "C-c t") 'vterm)

;; Salva buffers e posição do cursor
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

;; Multiples workspaces
(use-package persp-mode
  :ensure t
  :init
  (setq persp-auto-save-fname "~/.emacs.d/workspace")
  :config
  (persp-mode 1))

;; Window layout
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

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper))
  )

;;; silent warming and packages build
;(setq warning-suppress-types '((comp) (initialization)))

;; reload when change init.el
(add-hook 'after-init-hook
          (lambda () (add-to-list 'load-path "~/.emacs.d/")
            (load-file "~/.emacs.d/init.el")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(treemacs-tab-bar treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs terraform-mode pyvenv multiple-cursors magit k8s-mode highlight-parentheses git-gutter flymake-shell flycheck dockerfile-mode bash-completion auto-complete)))
