;; init.el -*- lexical-binding: t; -*-

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

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Make sure `use-package' is available.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(tool-bar-mode -1)

(setq use-file-dialog nil)
(setq use-dialog-box t)               ; only for mouse events
(setq inhibit-splash-screen t)
