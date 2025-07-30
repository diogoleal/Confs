;;; functions.el ---                              -*- lexical-binding: t; -*-

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

(provide 'functions)
;;; functions.el ends here
