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

(provide 'org-config)
;;; org-config.el ends here
