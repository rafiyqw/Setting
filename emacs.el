;; emacs.el --- Emacs configuration file -*- lexical-binding: t -*-

;; Startup screen
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

;; Graphical elements 
(when (display-graphic-p)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (tool-bar-mode -1))

;; Frame title
(setq frame-title-format "%b [%m]")

;; Directory
(defconst history-dir (concat user-emacs-directory "history/"))
(defconst etc-dir (concat user-emacs-directory "etc/"))
(defconst lisp-dir (concat user-emacs-directory "lisp/"))

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4)
;; Feedback
(setq echo-keystrokes 1e-6
      ring-bell-function #'ignore
      visible-bell t)
(fset #'yes-or-no-p #'y-or-n-p)

;; Scrolling
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; utf-8
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; Backup files
(setq auto-save-list-file-name (concat history-dir "autosave")
      backup-directory-alist '(("." . ,(concat history-dir "backup/")))
      make-backup-files nil
      backup-by-copying t
      create-lockfiles nil
      auto-save-default nil
      find-file-visit-truename t
      find-file-suppress-same-file-warnings t
      confirm-kill-emacs #'y-or-n-p
      )
