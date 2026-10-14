;;; early-init.el --- Early Init -*- lexical-binding: t -*-

;;; Commentary:

;; This file is to be executed before anything when emacs starts.

;;; Code:

;; Less garbage collection during startup, back to a sane value afterwards.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 64 1024 1024))))

;; Bigger chunks from LSP servers and the Claude CLI.
(setq read-process-output-max (* 4 1024 1024))

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      inhibit-startup-message t
      initial-scratch-message nil
      native-comp-async-report-warnings-errors 'silent)

;; Set through the frame alists so new emacsclient frames get them too.
(dolist (param '((tool-bar-lines . 0)
                 (menu-bar-lines . 0)
                 (vertical-scroll-bars)
                 (horizontal-scroll-bars)))
  (add-to-list 'default-frame-alist param))

;; Paint the first frame with the Omarchy background to avoid a white flash.
(let ((bin (executable-find "omarchy-theme-color")))
  (when bin
    (ignore-errors
      (let ((bg (car (process-lines bin "background")))
            (fg (car (process-lines bin "foreground"))))
        (when (and bg (string-prefix-p "#" bg))
          (add-to-list 'default-frame-alist `(background-color . ,bg)))
        (when (and fg (string-prefix-p "#" fg))
          (add-to-list 'default-frame-alist `(foreground-color . ,fg)))))))

;;; early-init.el ends here
