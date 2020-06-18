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
(setq frame-inhibit-implied-resize t)

;;; Built-in packages
;; startup
(use-feature startup
  :config
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message nil)
  (setq initial-major-mode 'fundamental-mode))

;; scroll-bar
(use-feature scroll-bar
  :config
  (scroll-bar-mode -1))
