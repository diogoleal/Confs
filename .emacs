; Set font
(set-default-font "Inconsolata-12")

; No beep warning
(setq visible-bell t)

; No scroll bar
(scroll-bar-mode -1)

; No tool bar
(tool-bar-mode -1)

; No menu bar
;(menu-bar-mode -1)

; keys
(global-set-key "\C-c\C-x" 'kill-whole-line)

; Set the frame size
(defun set-frame-size()
  (interactive)
  (if window-system
  (progn
     (setq initial-frame-alist '((width . 140) (height . 42))))))
(set-frame-size)

; Display line and column number
(setq column-number-mode t)
(setq line-number-mode t)

;Show line numbers, dynamically with spaces on either side:
(global-linum-mode 1)

(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

; Display the paren
(show-paren-mode t)

; Only file name in the title bar
;(setq frame-title-format (concat "%b"))

; Replace tab with spaces and set tab width 
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

; Set backup functions
(setq
  backup-by-copying t ; auto backup
  backup-directory-alist '(("."."~/.emacsbak")) ; store backup files in "~/.emacsbak"
  delete-old-versions t ; automatically delete old backup files
  kept-new-versions 6 ; keep only 6 version of backup
  kept-old-versions 2 ; keep the 2 oldest version of backup
  version-control t ; backup version control
)

; set auto-save
(setq auto-save-default t)
(setq auto-save-visited-file-name t )

; Set default major mode to text-mode
(setq default-major-mode 'text-mode)

; packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  )
(package-initialize)

; cperl-mode is preferred to perl-mode                                        
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.rex\\'" . cperl-mode)) ; load recipies .rex with cperl-mode
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; python mode
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

; undohist.el -> Persistent Undo History for GNU Emacs
;(require 'undohist)
;(undohist-initialize)

; auto-complete 
(add-to-list 'load-path "~/.emacs.d")    ; This may not be appeared if you have already added.
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(require 'auto-complete-extension)
(ac-config-default)

; markdown
;(autoload 'markdown-mode "markdown-mode"
;  "Major mode for editing Markdown files" t)
;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; -*- emacs-lisp -*-
(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load
(unless (package-installed-p 'org)  ;; Make sure the Org package is
  (package-install 'org))           ;; installed, install it if not
(package-initialize)                ;; Initialize & Install Package
;; (setq org-...)                   ;; Your custom settings

;http://emacsthemes.caisah.info/spacegray-theme/
;(load-theme 'spacegray t)

;; Please set your themes directory to 'custom-theme-load-path
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "/home/diogo/.emacs.d/replace-colorthemes"))

;; load your favorite theme
(load-theme 'snow t t)
(enable-theme 'snow)

