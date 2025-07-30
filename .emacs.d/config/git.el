;;; git.el ---                                       -*- lexical-binding: t; -*-

;; magit
(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-diff-refine-hunk nil)
  (with-eval-after-load 'magit-diff
    (require 'magit-ediff)
    (setq magit-ediff-dwim-show-on-hunks t)))

;; Git time machine to navigate through file history
(use-package git-timemachine
  :defer t)

;; Display git changes in the gutter
(use-package git-gutter
  :diminish git-gutter-mode
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

;; Git blame information
(use-package blamer
  :defer t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t))))

(provide 'git)
;;; git.el ends here
