;; emacs.el --- Emacs configuration file -*- lexical-binding: t -*-

;;; Optimization
;; set default value of gc-cons and file-name-handler
(defvar default-gc-cons-threshold gc-cons-threshold)
(defvar default-gc-cons-percentage gc-cons-percentage)
(defvar default-file-name-handler-alist file-name-handler-alist)

;; change the default value
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(setq file-name-handler-alist nil)

;; restore default value after loading init files
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq gc-cons-threshold default-gc-cons-threshold
                    gc-cons-percentage default-gc-cons-percentage
                    file-name-handler-alist default-file-name-handler-alist)))

;; change gc-cons value when entering minibuffer and restore when exit
(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (garbage-collect)
              (setq gc-cons-threshold default-gc-cons-threshold)))


;;; Package Manager
;; disable built-in emacs package manager
(setq package-enable-at-startup nil)

;; detect package modifications
(cond ((memq system-type '(cygwin windows-nt ms-dos))
       (setq straight-check-for-modifications
	     '(check-on-save find-when-checking)))
      ((and (executable-find "watchexec")
	    (executable-find "python3"))
       (setq straight-check-for-modifications
	     '(watch-files find-when-checking)))
      (t (setq straight-check-for-modifications
	       '(find-at-startup find-when-checking))))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

;; local package
(defmacro use-feature (name &rest args)
  (declare (indent defun))
  `(use-package ,name
		:straight nil
		:ensure nil
		,@args))


;;; Settings
(use-feature emacs
  :config
  (setq frame-inhibit-implied-resize t)
  (setq frame-title-format "%b [%m]")
  (setq use-file-dialog nil)
  (setq window-resize-pixelwise t)
  (setq echo-keystrokes 1e-6)
  (setq ring-bell-function #'ignore)
  (setq visible-bell t)
  (setq frame-resize-pixelwise t)
  (setq scroll-margin 0)
  (setq scroll-conservatively 1)
  (setq scroll-preserve-screen-position t)
  (setq auto-window-vscroll nil)
  (setq display-line-numbers-widen t)
  (setq locale-coding-system 'utf-8)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

