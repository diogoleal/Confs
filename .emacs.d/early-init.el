;;; early-init.el --- Early Init -*- lexical-binding t -*-

;;; Commentary:

;; This file is to be executed before anything when emacs starts.

;;; Code:

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; https://themkat.net/2025/03/25/simple_smoother_emacs_scrolling.html
(setq scroll-conservatively 10
      scroll-margin 15)

;; Enable line numbers
;;(setq display-line-numbers-type 'relative)
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode 1)

;; Autocomplete for brackets
(electric-pair-mode 1)

;; Typing overwrites selected text
(delete-selection-mode 1)

;; Disable tab indent
(setq-default indent-tabs-mode nil)

;; Disable package.el
(setq package-enable-at-startup nil)

;; Fonts
(add-to-list 'default-frame-alist '(font . "CaskaydiaCove Nerd Font-16"))

;; Theme
(load-theme 'leuven-dark t)

;; Fido mode
(fido-mode 1)
(fido-vertical-mode 1)

;; Annoying Files
;; Disable backup~ files
(setq make-backup-files nil)

;; Disable #autosave# files
(setq auto-save-default nil)

;; Disable .#lockfile
(setq create-lockfiles nil)

;;; early-init.el ends here
