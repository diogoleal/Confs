;;; early-init.el --- Early Init -*- lexical-binding t -*-

;;; Commentary:

;; This file is to be executed before anything when emacs starts.

;;; Code:

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode t)

(savehist-mode 1)
(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'default-frame-alist '(font . "CaskaydiaCove Nerd Font-12"))
(global-font-lock-mode 1)

;; https://themkat.net/2025/03/25/simple_smoother_emacs_scrolling.html
(setq scroll-conservatively 10
      scroll-margin 15)

;(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode 1)

(electric-pair-mode 1)
(delete-selection-mode 1)

(fido-mode 1)
(fido-vertical-mode 1)

;;; early-init.el ends here
