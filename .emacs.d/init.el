;;; init.el --- Personal Emacs configuration
;;; Commentary:
;; This is the Emacs configuration file.

;;; Code:

;;(setq initial-scratch-message nil)

;; Temporarily increase garbage collection threshold to improve startup performance
(setq gc-cons-threshold 100000000)

(setq inhibit-startup-message t)

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq global-auto-revert-mode t)

;; https://themkat.net/2025/03/25/simple_smoother_emacs_scrolling.html
(setq scroll-conservatively 10
      scroll-margin 15)

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
(set-frame-font "Inconsolata-14" nil t)
(load-theme 'leuven t)

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
(setq make-backup-files nil)
(setq version-control t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 10)
(setq kept-old-versions 2)
(setq auto-save-default nil)

;; ;; Enable tab-bar-mode
;; (tab-bar-mode 1)
;; (defun my/open-file-in-new-tab (orig-fun &rest args)
;;   "Opens files in a new tab when using interactive commands such as find-file."
;;   (let ((file (car args)))
;;     (tab-new)
;;     (apply orig-fun args)))

;; (advice-add 'find-file :around #'my/open-file-in-new-tab)
;; (advice-add 'find-file-other-window :around #'my/open-file-in-new-tab)
;; (advice-add 'find-file-other-frame :around #'my/open-file-in-new-tab)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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

;; Flycheck
(use-package flycheck
  :ensure t
  :after lsp-mode
  :init (global-flycheck-mode))

;; Integration with lsp-mode
(with-eval-after-load 'lsp-mode
  (setq lsp-diagnostics-provider :flycheck))
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

;; magit
(use-package magit
  :ensure t
  :config
  (setq magit-diff-refine-hunk nil)
  (with-eval-after-load 'magit-diff
    (require 'magit-ediff)
    (setq magit-ediff-dwim-show-on-hunks t)))

;; Ediff
(use-package ediff
  :ensure nil
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-highlight-all-diffs t)
  (setq ediff-auto-refine 'off)
  (setq ediff-forward-word-function 'forward-word)
  (setq ediff-diff-options "-w"))

;; Diff-hl
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-side 'left)
  (diff-hl-flydiff-mode 1))

(defun my-ediff-files ()
  (interactive)
  (call-interactively 'ediff-files))

(defun my-ediff-buffers ()
  (interactive)
  (call-interactively 'ediff-buffers))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon t))

(use-package org
  :ensure t
  :hook (org-mode . visual-line-mode)
  :config
  (setq
   org-startup-folded 'showeverything
   org-hide-emphasis-markers t
   org-log-done 'time))

(setq org-directory "~/org/")
(setq org-agenda-files
      (list (expand-file-name "tasks.org" org-directory)
            (expand-file-name "notes.org" org-directory)
            (expand-file-name "meetings.org" org-directory)
            (expand-file-name "journal.org" org-directory)))
(setq org-capture-templates
      '(("n" "Quick Note" entry (file "~/org/notes.org")
         "* %?\n%U\n")
        ("t" "Task" entry (file "~/org/tasks.org")
         "* TODO %?\nSCHEDULED: %t\n%U\n")
        ("m" "Meeting" entry (file "~/org/meetings.org")
         "* MEETING with %? :meeting:\nSCHEDULED: %t\n%U\n")
        ("j" "Journal Entry" entry (file+olp+datetree "~/org/journal.org")
         "* %?\n%U\n")))

(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'visual-line-mode)

;; Display images automatically when opening org files
(setq org-startup-with-inline-images t)

;; Show images when entering org-mode
(add-hook 'org-mode-hook #'org-display-inline-images)

;; Shortcut to refresh inline images (C-c i) only after org-mode is loaded
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c i") #'org-display-inline-images))

(setq org-image-actual-width nil)

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode))

(auto-insert-mode 1)
(setq auto-insert-query nil)

(define-auto-insert
  "\\.org\\'"
  (lambda ()
    (let ((type (completing-read "Template type: " '("note" "project" "diary"))))
      (insert
       (pcase type
         ("note"
          (concat "#+TITLE: " (read-string "Title: ") "\n"
                  "#+AUTHOR: Diogo\n"
                  "#+DATE: " (format-time-string "<%Y-%m-%d>") "\n"
                  "#+LANGUAGE: en\n"
                  "#+OPTIONS: toc:nil num:nil ^:nil\n"
                  "#+STARTUP: content inlineimages indent hidestars entitiespretty logdone\n"
                  "#+EXPORT_FILE_NAME: " (file-name-base (buffer-file-name)) "\n\n"
                  "* Tasks\n** TODO \n\n* Notes\n\n* Ideas\n"))
         ("project"
          (concat "#+TITLE: Project: " (read-string "Project Name: ") "\n"
                  "#+AUTHOR: Diogo\n"
                  "#+DATE: " (format-time-string "<%Y-%m-%d>") "\n"
                  "#+LANGUAGE: en\n"
                  "#+OPTIONS: toc:nil num:nil ^:nil\n"
                  "#+STARTUP: overview indent\n"
                  "* Goals\n\n* Tasks\n\n* Milestones\n\n"))
         ("diary"
          (concat "#+TITLE: Journal Entry\n"
                  "#+DATE: " (format-time-string "<%Y-%m-%d>") "\n"
                  "#+STARTUP: showall inlineimages\n\n"
                  "* Morning\n\n* Afternoon\n\n* Notes\n")))))))

;; backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Groovy
(add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode))
(use-package groovy-mode
  :ensure t
  :mode ("Jenkinsfile\\'" . groovy-mode)
  :config
  (setq groovy-indent-offset 4))

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-show-quick-access t))
(add-hook 'prog-mode-hook #'company-mode)

;; sudo dnf install clang-tools-extra nodejs npm
;; go install golang.org/x/tools/gopls@latest
;; npm install bash-language-server yaml-language-server pyright
;; M-x lsp

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode          . lsp)
         (python-mode     . lsp)
         (go-mode         . lsp)
         (sh-mode         . lsp)
         (yaml-mode       . lsp))
  :commands lsp)

(use-package all-the-icons :ensure t)
(use-package multiple-cursors :ensure t)
(use-package htmlize :ensure t)
(use-package multiple-cursors :ensure t)
(use-package consult :ensure t)
(use-package consult-project-extra :ensure t :after consult)
(use-package vertico :ensure t :init (vertico-mode))
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

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
          treemacs-workspace-switch-cleanup        nil))

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;; (treemacs-resize-icons 30)

  (use-package treemacs-projectile
    :ensure t)
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

(use-package treemacs-evil :after (treemacs evil) :ensure t)
(use-package treemacs-projectile :after (treemacs projectile) :ensure t)

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
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(defun open-in-window (buffer)
  "Open BUFFER in a selected window, prompting for window choice if multiple windows exist."
  (interactive "bBuffer: ")
  (let ((buffer-obj (get-buffer buffer)))
    (if (not buffer-obj)
        (error "Buffer '%s' does not exist" buffer)
      (if (> (count-windows) 1)
          (let* ((windows (window-list))
                 (window-names (mapcar (lambda (w)
                                         (format "%s: %s"
                                                 (window-number w)
                                                 (buffer-name (window-buffer w))))
                                       windows))
                 (chosen (completing-read "Select window: " window-names nil t))
                 (window (nth (string-to-number (car (split-string chosen ":")))
                              windows)))
            (select-window window)
            (switch-to-buffer buffer-obj))
        (switch-to-buffer buffer-obj)))))

(defun my-find-file (filename)
  "File opening with window choice"
  (interactive "FFind file: ")
  (find-file filename)
  (open-in-window (current-buffer)))

(defun my-treemacs-find-file ()
  "Open file via treemacs and choose window."
  (interactive)
  (treemacs-visit-node)
  (open-in-window (current-buffer)))

(use-package page-break-lines
  :ensure t
  :hook (dashboard-mode . page-break-lines-mode))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-startup-banner 'official) ;; ou um caminho para imagem
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)))
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-footer t)
  :config
  (dashboard-setup-startup-hook))

(use-package highlight-symbol
  :ensure t
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-on-navigation-p t
        highlight-symbol-idle-delay 0.5))

;; Load auth-source for secure credential management
(require 'auth-source)
(setq auth-sources '(default-secret))
(setq auth-source-debug t)

(defun close-buffers-by-extension (extension)
  "Closes all buffers that have files with the given EXTENSION."
  (interactive "sEnter the extension (e.g. yaml, py, el, bash)")
  (let ((pattern (concat "\\." (regexp-quote extension) "\\'")))
    (mapc (lambda (buf)
            (when (and (buffer-file-name buf)
                       (string-match pattern (buffer-file-name buf)))
              (kill-buffer buf)))
          (buffer-list)))
  (message "Buffers with .%s extension closed" extension))

;; vterm
(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "/usr/bin/fish")
  (setq vterm-max-scrollback 50000)
  (setq vterm-kill-buffer-on-exit t))

(dolist (binding
         '(("C-<tab>"        . other-window)
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
           ("C-c d f"        . 'my-ediff-files)
           ("C-c d b"        . 'my-ediff-buffers)
           ("C-s"            . consult-line)
           ("C-c s"          . consult-ripgrep)
           ("C-x b"          . consult-buffer)
           ("M-y"            . consult-yank-pop)
           ("C-c p f"        . consult-project-extra-find)
           ("C-c p g"        . consult-project-extra-ripgrep)
           ("C-c a"          . org-agenda)
           ("C-c c"          . org-capture)
           ("M-0"            . treemacs-select-window)
           ("C-x t t"        . treemacs)
           ("C-x t d"        . treemacs-select-directory)
           ("C-x t B"        . treemacs-bookmark)
           ("C-x t C-t"      . treemacs-find-file)
           ("C-x t M-t"      . treemacs-find-tag)
           ("C-c v"          . vterm)
           )
         )
  (global-set-key (kbd (car binding)) (cdr binding)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#ddddff" :foreground "black"))))
 '(diff-hl-delete ((t (:background "#ffdddd" :foreground "black"))))
 '(diff-hl-insert ((t (:background "#ddffdd" :foreground "black"))))
 '(ediff-current-diff-A ((t (:background "#ff5555" :foreground "white"))))
 '(ediff-current-diff-B ((t (:background "#55ff55" :foreground "white"))))
 '(ediff-even-diff-A ((t (:background "#f0f0f0"))))
 '(ediff-even-diff-B ((t (:background "#f0f0f0"))))
 '(ediff-fine-diff-A ((t (:background "#ffaaaa" :foreground "black"))))
 '(ediff-fine-diff-B ((t (:background "#aaffaa" :foreground "black"))))
 '(ediff-odd-diff-A ((t (:background "#e0e0e0"))))
 '(ediff-odd-diff-B ((t (:background "#e0e0e0"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#3498db"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#2ecc71"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#f1c40f"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#e67e22"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#e74c3c")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))

(put 'downcase-region 'disabled nil)
;;(put 'scroll-left 'disabled nil)
