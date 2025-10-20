;;; init.el --- Personal Emacs configuration
;;; Commentary:
;; This is the Emacs configuration file.

;;; Code:
(load-file "~/.emacs.d/elpaca.el")

(load-theme 'manoj-dark t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

(setq whitespace-style
      '(face
        trailing
        tabs
        spaces
        newline
        empty
        space-mark
        tab-mark))

(use-package projectile
  :defer 1
  :config
  (projectile-mode +1))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil))

(use-package cape
  :after corfu)

(use-package orderless
  :custom (completion-styles '(orderless basic))
  :config
  (setq completion-category-defaults nil))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
    :after yasnippet)

(use-package flymake
  :ensure nil
  :hook ((prog-mode . flymake-mode)))

(use-package eglot
  :defer t
  :config
  (setq eglot-server-programs
        '((python-ts-mode . ("pyright-langserver" "--stdio"))
          (python-mode . ("pyright-langserver" "--stdio"))
          (go-ts-mode . ("gopls"))
          (go-mode . ("gopls"))
          (c-ts-mode . ("clangd"))
          (c-mode . ("clangd"))
          (c++-ts-mode . ("clangd"))
          (c++-mode . ("clangd"))
          ;; (yaml-ts-mode . ("yaml-language-server" "--stdio"))
          (yaml-mode . ("yaml-language-server" "--stdio"))
          (json-ts-mode . ("vscode-json-languageserver" "--stdio"))
          (json-mode . ("vscode-json-languageserver" "--stdio"))
          (sh-mode . ("bash-language-server" "start"))
          (terraform-mode . ("terraform-ls" "serve"))
          (hcl-mode . ("terraform-ls" "serve")))))

(defun my/enable-eglot-if-available ()
  "Start eglot if an LSP server is available for this buffer."
  (condition-case nil
      (eglot-ensure)
    ((error "message" format-args) nil)))

(dolist (hook '(python-ts-mode-hook
                json-ts-mode-hook
                go-ts-mode-hook
                c-ts-mode-hook
                sh-mode-hook
                terraform-mode-hook
                hcl-mode-hook))
  (add-hook hook #'my/enable-eglot-if-available))

(use-package tree-sitter
  :hook (prog-mode . turn-on-tree-sitter-mode)
  :config
  )
(use-package tree-sitter-langs
  :ensure t
  :config
  (tree-sitter-require 'yaml))

(when (fboundp 'treesit-available-p)
 (when (treesit-available-p)
   (setq my/treesit-langs
         '(bash c cpp go python json yaml hcl))

   (dolist (lang my/treesit-langs)
     (unless (treesit-language-available-p lang)
       (condition-case err
           (progn
             (message "Installing treesit grammar for %s ..." lang)
             (ignore-errors (treesit-install-language-grammar lang)))
         (error (message "Failed to install %s: %s" lang err)))))

   (setq major-mode-remap-alist
         '((sh-mode . bash-ts-mode)
           (c-mode . c-ts-mode)
           (c++-mode . c++-ts-mode)
           (python-mode . python-ts-mode)
           (json-mode . json-ts-mode)
           (yaml-mode . yaml-mode)))

   (setq treesit-font-lock-level 4)))

(use-package terraform-mode
  :defer t
  :mode ("\\.tf\\'" . terraform-mode)
  :hook (terraform-mode . (lambda ()
                            (when (and (fboundp 'treesit-language-available-p)
                                       (treesit-language-available-p 'hcl))
                              (when (fboundp 'hcl-ts-mode)
                                (hcl-ts-mode))))))
(use-package go-mode
  :defer t
  :hook ((go-mode . my/enable-eglot-if-available)))
;;(use-package cc-mode
;;  :defer t)
(use-package yaml-mode
  :defer t
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :hook (yaml-mode . my/enable-eglot-if-available))
(use-package json-mode
  :defer t
  :mode ("\\.json\\'" . json-mode)
  :hook (json-mode . my/enable-eglot-if-available))
(use-package sh-mode
  :ensure nil
  :mode ("\\.sh\\'" . sh-mode)
  :hook (sh-mode . my/enable-eglot-if-available))
(use-package python
  :defer t
  :config
  (setq python-shell-interpreter "python3"))
(use-package groovy-mode
  :ensure t
  :mode (("Jenkinsfile\\'" . groovy-mode))
  :interpreter ("groovy" . groovy-mode)
  :config
  (setq groovy-indent-offset 4))
(use-package jenkinsfile-mode
  :ensure t
  :mode ("Jenkinsfile\\'" . jenkinsfile-mode))
(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile\\'" . dockerfile-mode)))

(use-package all-the-icons :ensure t)
(use-package multiple-cursors :ensure t)
(use-package consult :ensure t)
;;(use-package consult-project-extra :ensure t :after consult)
;;(use-package vertico :ensure t :init (vertico-mode))
(use-package neotree :ensure t)
(with-eval-after-load 'neotree
  (dolist (face '(neo-root-dir-face
                  neo-dir-link-face
                  neo-file-link-face
                  neo-expand-btn-face
                  neo-banner-face))
    (set-face-attribute face nil :height 90)))
(setq neo-hidden-regexp-list '("^\\.git$"))
(setq neo-theme 'ascii)

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
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))
(setq dashboard-items '((recents   . 5)
                        (bookmarks . 5)
                        (projects  . 5)
                        (registers . 5)))

(setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-banner-title
                                  dashboard-insert-navigator
                                  dashboard-insert-init-info
                                  dashboard-insert-items
                                  dashboard-insert-newline
                                  dashboard-insert-footer))
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
(setq dashboard-icon-type 'all-the-icons)

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
           ("C-c e r"        . eglot-rename)
           ("C-c e f"        . eglot-format)
           ("C-c e a"        . eglot-code-actions)
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
