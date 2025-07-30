;;; ui.el

;; https://themkat.net/2025/03/25/simple_smoother_emacs_scrolling.html
(setq scroll-conservatively 10
      scroll-margin 15)

(global-hl-line-mode 1)
(set-frame-font "Fira Code-12" nil t)

(use-package catppuccin-theme
  :ensure t
  :init
  (setq catppuccin-flavor 'macchiato) ;; 'latte, 'frappe, 'macchiato, 'mocha
  :config
  (load-theme 'catppuccin :no-confirm)
  (catppuccin-reload))

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

(use-package ligature
  :config
  (ligature-set-ligatures 't '("www" "**" "***" "==" "!=" "===" "!=="
                               "->" "->>" "<-" "<<" ">>" "::" "===" "=>"
                               "=>>" "<=>" "<=" ">=" "<>" "!!" "??" "%%"
                               "&&" "||" "++" "--" "..." "##" "###" "#{" "#["
                               "]#" "}#" ":::" "|>" "<|" "<|>" ">>=" "=<<"))
  (global-ligature-mode t))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons :ensure t)
(use-package nerd-icons :defer t :ensure t :custom
  (nerd-icons-scale-factor 1.0))

(use-package highlight-symbol
  :ensure t
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-on-navigation-p t
        highlight-symbol-idle-delay 0.5))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match nil :weight 'bold)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(add-hook 'text-mode-hook 'visual-line-mode)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("NOTE"   . "#1E90FF"))))

;; Font rendering optimizations
(setq inhibit-compacting-font-caches t)

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
 '(package-selected-packages nil))

(put 'downcase-region 'disabled nil)
;;(put 'scroll-left 'disabled nil)

(provide 'ui)
;;; ui.el ends here
