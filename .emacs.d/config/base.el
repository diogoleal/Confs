;;; base.el --- Configurações básicas

;; Temporarily increase garbage collection threshold to improve startup performance
(setq gc-cons-threshold 100000000)

;; Automatic insertion of content into new files
(auto-insert-mode 1)
(setq auto-insert-query nil)

(setq inhibit-startup-message t)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(global-auto-revert-mode t)

;; Scrolling suave
(setq scroll-conservatively 10
      scroll-margin 15)

;; backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(use-package async :defer t :ensure t)
(use-package request :defer t :ensure t)
(use-package multiple-cursors :ensure t)

(provide 'base)
;;; base.el ends here
