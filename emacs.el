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



