;;(setq initial-scratch-message nil)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-battery-mode t)
 '(package-selected-packages
   '(smart-tabs-mode bash-completion neotree k8s-mode highlight-parentheses magit dracula-theme dockerfile-mode python yaml-mode))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 112 :width normal)))))
(load-theme 'dracula t)
(show-paren-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq-default show-trailing-whitespace t)

(require 'dockerfile-mode)
(require 'k8s-mode)

;(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-display-line-numbers-mode)

(require 'highlight-parentheses)
(add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'bash-completion)
(bash-completion-setup)

(global-set-key (kbd "<mouse-2>") 'clipboard-yank)

(smart-tabs-insinuate 'c 'javascript 'python)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)



;; credit: yorickvP on Github
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)


;; magit
(setq magit-refresh-status-buffer nil)
