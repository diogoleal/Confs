;;; init.el --- Personal Emacs configuration
;;; Commentary:
;; This is the Emacs configuration file.

;;; Code:
(load-file "~/.emacs.d/elpaca.el")

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package terraform-mode
  :ensure t
  :custom
  (terraform-indent-level 4)
  :hook
  (terraform-mode . my-terraform-mode-init)
  :config
  (defun my-terraform-mode-init ()))

(add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode))
(use-package groovy-mode
  :ensure t
  :mode ("Jenkinsfile\\'" . groovy-mode)
  :config
  (setq groovy-indent-offset 4))

(use-package transient :ensure t)
(use-package magit :ensure t)

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

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (corfu-popupinfo-mode))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
    :after yasnippet)

(use-package eglot
  :ensure t
  :hook ((c-mode      . eglot-ensure)
         (python-mode . eglot-ensure)
         (go-mode     . eglot-ensure)
         (sh-mode     . eglot-ensure)
         (yaml-mode   . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("jedi-language-server"))))

(use-package all-the-icons :ensure t)
(use-package multiple-cursors :ensure t)
(use-package consult :ensure t)
;;(use-package consult-project-extra :ensure t :after consult)
;;(use-package vertico :ensure t :init (vertico-mode))
(use-package neotree :ensure t)

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

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

(use-package dashboard
  :config
  (setq dashboard-center-content t)
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

;; Initial buffer
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

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
           ("C-s"            . consult-line)
           ("C-c s"          . consult-ripgrep)
           ("C-x b"          . consult-buffer)
           ("M-y"            . consult-yank-pop)
           ;; ("C-c p f"        . consult-project-extra-find) 
           ;; ("C-c p g"        . consult-project-extra-ripgrep)
           ("C-c a"          . org-agenda)
           ("C-c c"          . org-capture)
           ("<f8>"           . neotree-toggle)
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
 '(package-selected-packages nil)
) 
(put 'downcase-region 'disabled nil)
;;(put 'scroll-left 'disabled nil)
