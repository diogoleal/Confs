;;; init.el --- Personal Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;; This is the Emacs configuration file.

;;; Code:

;;; Packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; The daemon may start with a shorter PATH than the login shell.
(dolist (dir '("~/.local/bin" "~/bin" "~/bin/go/bin" "~/.local/share/mise/shims"))
  (let ((dir (expand-file-name dir)))
    (when (and (file-directory-p dir) (not (member dir exec-path)))
      (add-to-list 'exec-path dir)
      (setenv "PATH" (concat dir path-separator (getenv "PATH"))))))

;;; Omarchy theme and font
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'omarchy)
(unless (omarchy-sync)
  (load-theme 'modus-vivendi t)
  (add-to-list 'default-frame-alist '(font . "CaskaydiaCove Nerd Font-12")))

;; Wayland: no title bar and no gaps, Hyprland draws the borders.
(when (featurep 'pgtk)
  (add-to-list 'default-frame-alist '(undecorated . t))
  (setq pgtk-wait-for-event-timeout nil))

;;; Editing defaults
(setq-default indent-tabs-mode nil
              tab-width 2)
(setq visible-bell nil
      ring-bell-function #'ignore
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      use-short-answers t
      confirm-kill-processes nil
      require-final-newline t
      sentence-end-double-space nil
      enable-recursive-minibuffers t
      read-extended-command-predicate #'command-completion-default-include-p
      tab-always-indent 'complete
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      dired-listing-switches "-alh --group-directories-first"
      dired-dwim-target t)

;; https://themkat.net/2025/03/25/simple_smoother_emacs_scrolling.html
(setq scroll-conservatively 10
      scroll-margin 15)
(pixel-scroll-precision-mode 1)

(show-paren-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(column-number-mode 1)
(winner-mode 1)
(repeat-mode 1)
(which-key-mode 1)
;; Claude edits files on disk, buffers have to follow.
(global-auto-revert-mode 1)

(defun my-editing-buffer-init ()
  "Line numbers and trailing whitespace for buffers that hold a file."
  (display-line-numbers-mode 1)
  (setq show-trailing-whitespace t))
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'my-editing-buffer-init))
(add-hook 'prog-mode-hook #'hl-line-mode)

(defun close-buffers-by-extension (extension)
  "Kill every buffer visiting a file with EXTENSION."
  (interactive "sExtension: ")
  (let ((extension (string-remove-prefix "." extension)))
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer))
                  ((equal (file-name-extension file) extension)))
        (kill-buffer buffer)))))

;;; Minibuffer and completion
(use-package vertico
  :init (vertico-mode 1)
  :custom (vertico-cycle t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package consult-project-extra)

(use-package embark
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :init (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :config
  (corfu-popupinfo-mode 1))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

;;; Interface
(use-package nerd-icons)

(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-completion
  :after marginalia
  :config (nerd-icons-completion-mode 1)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-buffer-file-name-style 'relative-from-project))

(use-package dashboard
  :config
  (setq dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)))
  (dashboard-setup-startup-hook)
  ;; emacsclient frames of the daemon start on the dashboard too.
  (setq initial-buffer-choice
        (lambda () (get-buffer-create dashboard-buffer-name))))

(use-package treemacs
  :defer t
  :bind ("<f8>" . treemacs)
  :config
  (setq treemacs-no-png-images t
        treemacs-width 32
        treemacs-indentation 2))

(use-package treemacs-nerd-icons
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package eyebrowse
  :config
  (eyebrowse-mode t))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-on-navigation-p t
        highlight-symbol-idle-delay 0.5))

(use-package highlight-parentheses
  :hook
  ((minibuffer-setup . highlight-parentheses-minibuffer-setup)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package multiple-cursors)

;;; Git
(use-package magit
  :commands magit-status)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;;; Terminal
(use-package vterm
  :commands vterm
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t))

;;; Languages
(use-package k8s-mode)
(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package go-mode)
;; Syntax highlighting para Jenkinsfile
(use-package groovy-mode
  :mode ("Jenkinsfile\\'" . groovy-mode)
  :config
  (setq groovy-indent-offset 4))

(use-package terraform-mode
  :custom
  (terraform-indent-level 4)
  (terraform-format-on-save t))

(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package pyvenv
  :config
  (pyvenv-mode t)
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t))

;; LSP through the servers installed by `make arch'.
(use-package eglot
  :ensure nil
  :hook ((python-mode python-ts-mode
          sh-mode bash-ts-mode
          go-mode go-ts-mode
          c-mode c-ts-mode c++-mode c++-ts-mode
          yaml-mode yaml-ts-mode
          js-json-mode json-ts-mode)
         . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc-doc-buffer))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format full)))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;;; Claude
;; Claude Code inside Emacs: the CLI runs in vterm and talks to Emacs as its
;; IDE, so it sees the selection and diagnostics and proposes edits as ediff.
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c c" . claude-code-ide-menu)
  :custom
  (claude-code-ide-terminal-backend 'vterm)
  (claude-code-ide-window-side 'right)
  (claude-code-ide-window-width 100)
  :config
  ;; Give Claude the xref, imenu, tree-sitter and project tools of Emacs.
  (claude-code-ide-emacs-tools-setup))

(defun my-anthropic-api-key ()
  "Return the Anthropic API key from the environment or auth-source."
  (or (getenv "ANTHROPIC_API_KEY")
      (auth-source-pick-first-password :host "api.anthropic.com")))

;; Claude through the API, in any buffer. Needs ANTHROPIC_API_KEY or a
;; "machine api.anthropic.com login apikey password ..." line in ~/.authinfo.gpg
(use-package gptel
  :bind (("C-c a a" . gptel)
         ("C-c a s" . gptel-send)
         ("C-c a r" . gptel-rewrite)
         ("C-c a f" . gptel-add-file)
         ("C-c a m" . gptel-menu))
  :config
  (setq gptel-model 'claude-opus-5-5
        gptel-default-mode 'markdown-mode
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key #'my-anthropic-api-key)))

;;; Keys
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
           ("M-g g"          . consult-goto-line)
           ("M-g i"          . consult-imenu)
           ("C-c p f"        . consult-project-extra-find)
           ("C-c p g"        . consult-project-extra-ripgrep)
           ("C-c t"          . vterm)
           ("<f9>"           . rainbow-delimiters-mode)
           )
         )
  (global-set-key (kbd (car binding)) (cdr binding)))

;;; init.el ends here
