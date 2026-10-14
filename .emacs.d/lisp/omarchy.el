;;; omarchy.el --- Follow the Omarchy theme and font -*- lexical-binding: t -*-

;;; Commentary:

;; Builds the `omarchy' theme from the palette of the current Omarchy theme
;; (read through `omarchy-theme-color') and uses the Omarchy monospace font.
;; The theme-set and font-set hooks call `omarchy-sync' through emacsclient,
;; so a running Emacs follows `omarchy theme set' and `omarchy font set'.
;; On a machine without Omarchy `omarchy-sync' returns nil and changes nothing.

;;; Code:

(require 'color)
(require 'cl-lib)

(defgroup omarchy nil
  "Follow the Omarchy desktop theme."
  :group 'faces)

(defcustom omarchy-font-height 120
  "Height of the default face, in 1/10 pt."
  :type 'integer)

(defcustom omarchy-after-sync-hook nil
  "Hook run after the Omarchy theme and font were applied."
  :type 'hook)

(defvar omarchy-colors nil
  "Alist of (NAME . VALUE) strings of the current Omarchy palette.")

(deftheme omarchy "Colors of the current Omarchy theme.")

(defun omarchy--read-colors ()
  "Return the current Omarchy palette as an alist, or nil without Omarchy."
  (when-let* ((bin (executable-find "omarchy-theme-color")))
    (ignore-errors
      (delq nil
            (mapcar (lambda (line)
                      (pcase (split-string line "\t")
                        (`(,key ,value) (cons key value))))
                    (process-lines bin "--all"))))))

(defun omarchy-color (name)
  "Return the color NAME of the current Omarchy palette."
  (cdr (assoc name omarchy-colors)))

(defun omarchy--mix (from to amount)
  "Mix the colors FROM and TO, AMOUNT being the share of TO from 0.0 to 1.0."
  (apply #'color-rgb-to-hex
         (append (cl-mapcar (lambda (a b) (+ (* a (- 1 amount)) (* b amount)))
                            (color-name-to-rgb from)
                            (color-name-to-rgb to))
                 '(2))))

(defun omarchy--faces ()
  "Return the face specs of the `omarchy' theme for `omarchy-colors'."
  (let* ((bg (omarchy-color "background"))
         (bg-dark (omarchy-color "dark_background"))
         (bg-light (omarchy-color "lighter_background"))
         (fg (omarchy-color "foreground"))
         (fg-dim (omarchy-color "light_foreground"))
         (fg-dark (omarchy-color "dark_foreground"))
         (fg-bright (omarchy-color "bright_foreground"))
         (muted (omarchy-color "muted"))
         (accent (omarchy-color "accent"))
         (selection (omarchy-color "selection_background"))
         (red (omarchy-color "red"))
         (green (omarchy-color "green"))
         (yellow (omarchy-color "yellow"))
         (orange (omarchy-color "orange"))
         (blue (omarchy-color "blue"))
         (magenta (omarchy-color "magenta"))
         (cyan (omarchy-color "cyan"))
         (added (omarchy--mix bg green 0.18))
         (removed (omarchy--mix bg red 0.18))
         (changed (omarchy--mix bg blue 0.18))
         (faces
          `((default :background ,bg :foreground ,fg)
            (cursor :background ,(omarchy-color "cursor"))
            (region :background ,selection :extend t)
            (secondary-selection :background ,muted :extend t)
            (highlight :background ,bg-light)
            (hl-line :background ,bg-light :extend t)
            (fringe :background ,bg :foreground ,fg-dark)
            (vertical-border :foreground ,muted)
            (window-divider :foreground ,muted)
            (window-divider-first-pixel :foreground ,bg)
            (window-divider-last-pixel :foreground ,bg)
            (shadow :foreground ,fg-dim)
            (minibuffer-prompt :foreground ,accent :weight bold)
            (link :foreground ,accent :underline t)
            (link-visited :foreground ,magenta :underline t)
            (error :foreground ,red :weight bold)
            (warning :foreground ,orange :weight bold)
            (success :foreground ,green :weight bold)
            (match :background ,muted :foreground ,fg-bright)
            (isearch :background ,accent :foreground ,bg :weight bold)
            (lazy-highlight :background ,muted :foreground ,fg-bright)
            (isearch-fail :background ,removed)
            (show-paren-match :foreground ,accent :weight bold :underline t)
            (show-paren-mismatch :background ,red :foreground ,bg)
            (trailing-whitespace :background ,removed)
            (line-number :foreground ,fg-dark)
            (line-number-current-line :foreground ,accent :weight bold)
            (fill-column-indicator :foreground ,muted)
            (header-line :background ,bg-light :foreground ,fg)
            (mode-line :background ,bg-light :foreground ,fg
                       :box (:line-width 4 :color ,bg-light))
            (mode-line-active :inherit mode-line)
            (mode-line-inactive :background ,bg-dark :foreground ,fg-dim
                                :box (:line-width 4 :color ,bg-dark))
            (mode-line-emphasis :foreground ,accent :weight bold)
            (mode-line-buffer-id :weight bold)
            (tab-bar :background ,bg-dark :foreground ,fg-dim)
            (tab-bar-tab :background ,bg :foreground ,accent :weight bold)
            (tab-bar-tab-inactive :background ,bg-dark :foreground ,fg-dim)
            (tab-line :inherit tab-bar)
            (tooltip :background ,bg-light :foreground ,fg)
            (help-key-binding :background ,bg-light :foreground ,accent
                              :box (:line-width 1 :color ,muted))
            (completions-common-part :foreground ,accent :weight bold)
            (completions-annotations :foreground ,fg-dim)
            (escape-glyph :foreground ,cyan)
            (homoglyph :foreground ,cyan)

            (font-lock-comment-face :foreground ,fg-dim :slant italic)
            (font-lock-comment-delimiter-face :inherit font-lock-comment-face)
            (font-lock-doc-face :foreground ,fg-dim)
            (font-lock-string-face :foreground ,green)
            (font-lock-keyword-face :foreground ,magenta)
            (font-lock-builtin-face :foreground ,cyan)
            (font-lock-function-name-face :foreground ,blue)
            (font-lock-function-call-face :foreground ,blue)
            (font-lock-variable-name-face :foreground ,fg-bright)
            (font-lock-variable-use-face :foreground ,fg)
            (font-lock-type-face :foreground ,yellow)
            (font-lock-constant-face :foreground ,orange)
            (font-lock-number-face :foreground ,orange)
            (font-lock-preprocessor-face :foreground ,magenta)
            (font-lock-property-name-face :foreground ,cyan)
            (font-lock-property-use-face :foreground ,cyan)
            (font-lock-operator-face :foreground ,fg-dim)
            (font-lock-punctuation-face :foreground ,fg-dim)
            (font-lock-escape-face :foreground ,cyan)
            (font-lock-regexp-face :foreground ,cyan)
            (font-lock-negation-char-face :foreground ,red)
            (font-lock-warning-face :foreground ,orange :weight bold)

            (ansi-color-black :background ,(omarchy-color "color0")
                              :foreground ,(omarchy-color "color0"))
            (ansi-color-red :background ,red :foreground ,red)
            (ansi-color-green :background ,green :foreground ,green)
            (ansi-color-yellow :background ,yellow :foreground ,yellow)
            (ansi-color-blue :background ,blue :foreground ,blue)
            (ansi-color-magenta :background ,magenta :foreground ,magenta)
            (ansi-color-cyan :background ,cyan :foreground ,cyan)
            (ansi-color-white :background ,fg :foreground ,fg)
            ,@(cl-loop for (name . index) in '((black . 8) (red . 9) (green . 10)
                                               (yellow . 11) (blue . 12)
                                               (magenta . 13) (cyan . 14)
                                               (white . 15))
                       for color = (omarchy-color (format "color%d" index))
                       collect `(,(intern (format "ansi-color-bright-%s" name))
                                 :background ,color :foreground ,color))

            (diff-added :background ,added :extend t)
            (diff-removed :background ,removed :extend t)
            (diff-changed :background ,changed :extend t)
            (diff-refine-added :background ,(omarchy--mix bg green 0.35))
            (diff-refine-removed :background ,(omarchy--mix bg red 0.35))
            (diff-refine-changed :background ,(omarchy--mix bg blue 0.35))
            (diff-header :background ,bg-light :foreground ,fg)
            (diff-file-header :background ,bg-light :foreground ,accent
                              :weight bold)
            (diff-hunk-header :background ,bg-light :foreground ,fg-dim)
            (ediff-current-diff-A :background ,removed :extend t)
            (ediff-current-diff-B :background ,added :extend t)
            (ediff-current-diff-C :background ,changed :extend t)
            (ediff-fine-diff-A :background ,(omarchy--mix bg red 0.35))
            (ediff-fine-diff-B :background ,(omarchy--mix bg green 0.35))
            (ediff-fine-diff-C :background ,(omarchy--mix bg blue 0.35))
            (ediff-even-diff-A :background ,bg-light :extend t)
            (ediff-even-diff-B :background ,bg-light :extend t)
            (ediff-even-diff-C :background ,bg-light :extend t)
            (ediff-odd-diff-A :background ,bg-light :extend t)
            (ediff-odd-diff-B :background ,bg-light :extend t)
            (ediff-odd-diff-C :background ,bg-light :extend t)
            (magit-section-highlight :background ,bg-light :extend t)
            (magit-section-heading :foreground ,accent :weight bold)
            (magit-branch-local :foreground ,blue)
            (magit-branch-remote :foreground ,green)
            (magit-hash :foreground ,fg-dim)
            (magit-diff-added :background ,added :extend t)
            (magit-diff-added-highlight :background ,added :extend t)
            (magit-diff-removed :background ,removed :extend t)
            (magit-diff-removed-highlight :background ,removed :extend t)
            (magit-diff-context :foreground ,fg-dim :extend t)
            (magit-diff-context-highlight :background ,bg-light
                                          :foreground ,fg :extend t)
            (magit-diff-hunk-heading :background ,muted :foreground ,fg
                                     :extend t)
            (magit-diff-hunk-heading-highlight :background ,muted
                                               :foreground ,fg-bright
                                               :weight bold :extend t)
            (git-gutter:added :foreground ,green)
            (git-gutter:modified :foreground ,blue)
            (git-gutter:deleted :foreground ,red)

            (flymake-error :underline (:style wave :color ,red))
            (flymake-warning :underline (:style wave :color ,orange))
            (flymake-note :underline (:style wave :color ,fg-dark))
            (eglot-highlight-symbol-face :background ,muted)
            (eglot-inlay-hint-face :foreground ,fg-dark :height 0.9)
            (highlight-symbol-face :background ,muted)

            (vertico-current :background ,selection :extend t)
            (orderless-match-face-0 :foreground ,accent :weight bold)
            (orderless-match-face-1 :foreground ,magenta :weight bold)
            (orderless-match-face-2 :foreground ,green :weight bold)
            (orderless-match-face-3 :foreground ,cyan :weight bold)
            (corfu-default :background ,bg-light :foreground ,fg)
            (corfu-current :background ,selection :foreground ,fg-bright)
            (corfu-border :background ,muted)
            (corfu-bar :background ,accent)
            (which-key-key-face :foreground ,accent :weight bold)
            (which-key-command-description-face :foreground ,fg)
            (which-key-group-description-face :foreground ,magenta)

            (markdown-code-face :background ,bg-dark :extend t)
            (markdown-inline-code-face :foreground ,green)
            (markdown-header-face :foreground ,accent :weight bold)
            (org-block :background ,bg-dark :extend t)
            (org-block-begin-line :background ,bg-dark :foreground ,fg-dark
                                  :extend t)
            (org-block-end-line :inherit org-block-begin-line)
            (org-code :foreground ,green)
            (org-verbatim :foreground ,cyan)
            (outline-1 :foreground ,accent :weight bold)
            (outline-2 :foreground ,blue :weight bold)
            (outline-3 :foreground ,magenta :weight bold)
            (outline-4 :foreground ,green :weight bold)

            (dashboard-banner-logo-title :foreground ,accent :weight bold)
            (dashboard-heading :foreground ,accent :weight bold)
            (treemacs-root-face :foreground ,accent :weight bold)
            (treemacs-directory-face :foreground ,fg)
            (treemacs-git-modified-face :foreground ,blue)
            (treemacs-git-untracked-face :foreground ,green))))
    (mapcar (lambda (face) `(,(car face) ((t ,@(cdr face))))) faces)))

(defun omarchy-apply-theme ()
  "Rebuild and enable the `omarchy' theme. Return nil without Omarchy."
  (interactive)
  (when-let* ((colors (omarchy--read-colors))
              ((assoc "background" colors)))
    (setq omarchy-colors colors)
    ;; The early-init colors would otherwise pin new frames to the old theme.
    (dolist (param '(background-color foreground-color))
      (setq default-frame-alist (assq-delete-all param default-frame-alist)))
    (setq frame-background-mode
          (if (equal (omarchy-color "mode") "light") 'light 'dark))
    (mapc #'frame-set-background-mode (frame-list))
    (apply #'custom-theme-set-faces 'omarchy (omarchy--faces))
    (enable-theme 'omarchy)
    t))

(defun omarchy-apply-font ()
  "Use the Omarchy monospace font. Return nil without Omarchy."
  (interactive)
  (when-let* ((bin (executable-find "omarchy-font-current"))
              (family (car (ignore-errors (process-lines bin)))))
    (set-face-attribute 'default nil :family family
                        :height omarchy-font-height)
    (set-face-attribute 'fixed-pitch nil :family family)
    t))

;;;###autoload
(defun omarchy-sync (&rest _)
  "Apply the current Omarchy theme and font. Return nil without Omarchy."
  (interactive)
  (let ((themed (omarchy-apply-theme)))
    (omarchy-apply-font)
    (when themed
      (run-hooks 'omarchy-after-sync-hook))
    themed))

(provide 'omarchy)

;;; omarchy.el ends here
