;; emacs.el --- Emacs configuration file -*- lexical-binding: t -*-

; Optimization
;; built-in emacs package manager
(setq package-enable-at-startup nil)

;; Resize frame
(setq frame-inhibit-implied-resize t)

;; Startup screen
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

;; Graphical elements 
(when (display-graphic-p)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (tool-bar-mode -1))

;; Dialog box
(setq use-file-dialog nil)


; Setting
;; Frame title
(setq frame-title-format "%b [%m]")

;; Line and column number
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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

(fset #'yes-or-no-p #'y-or-n-p))

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

;; files.el
;; find-file-visit-truename t
;; find-file-suppress-same-file-warnings t
;; confirm-kill-emacs #'y-or-n-p

;;vc-hooks.el
;; vc-follow-symlinks t

;; simple.el
;; (column-number-mode 1)



