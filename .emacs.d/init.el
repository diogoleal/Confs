;;; init.el --- Personal Emacs configuration

(add-to-list 'load-path "~/.emacs.d/config/")

(require 'melpa-config)
(require 'base)
(require 'ui)
(require 'functions)
(require 'keys)
(require 'code)
(require 'languages)
(require 'git)
(require 'org-config)
(require 'treemacs-config)
(require 'dashboard-config)

;; (advice-add 'find-file :around #'my/open-file-in-new-tab)
;; (advice-add 'find-file-other-window :around #'my/open-file-in-new-tab)
;; (advice-add 'find-file-other-frame :around #'my/open-file-in-new-tab)


;; vterm
(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "/usr/bin/fish")
  (setq vterm-max-scrollback 50000)
  (setq vterm-kill-buffer-on-exit t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
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
