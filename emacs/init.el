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

;;(require 'package)
;;(package-initialize)
;;(add-to-list 'package-archives
;;             '("melpa" . "https://melpa.org/packages/"))
;;
;;;; Make sure `use-package' is available.
;;(unless (package-installed-p 'use-package)
;;  (package-refresh-contents)
;;  (package-install 'use-package))
;;
;;;; Configure `use-package' prior to loading it.
;;(eval-and-compile
;;  (setq use-package-always-ensure nil)
;;  (setq use-package-always-defer nil)
;;  (setq use-package-always-demand nil)
;;  (setq use-package-expand-minimally nil)
;;  (setq use-package-enable-imenu-support t)
;;  (setq use-package-compute-statistics nil)
;;  (setq use-package-hook-name-suffix nil))
;;
;;(eval-when-compile
;;  (require 'use-package))

;;; Settings
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
(setq-default display-line-numbers-widen t)
(setq locale-coding-system 'utf-8)
(setq load-prefer-newer t)
(setq x-underline-at-descent-line t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(tool-bar-mode -1)
;(menu-bar-mode -1)
(fset #'yes-or-no-p #'y-or-n-p)

;;;;; Built-in packages
;;;; startup
;;;;(use-package startup
;;;;  :init
;;  (setq inhibit-startup-screen t)
;;  (setq initial-scratch-message nil)
;;  (setq initial-major-mode 'fundamental-mode)
;;  (fset #'display-startup-echo-area-message #'ignore)
;;
;;;; window
;;;; (use-package window
;;;;  :init
;;  (setq split-height-threshold nil)
;;
;;;; simple.el
;;(use-package simple
;;  :config
;;  (setq save-interprogram-paste-before-kill t)
;;  (setq shift-select-mode nil)
;;  (setq column-number-mode 1))
;;
;;;; mouse wheel
;;(use-package mwheel
;;  :config
;;  (setq mouse-wheel-scroll-amount
;;        '(1 ((shift) . 5) ((control)))))
;;
;;;; tooltip
;;(use-package tooltip
;;  :config
;;  (setq tooltip-delay 0.5)
;;  (setq tooltip-use-echo-area t)
;;  (setq x-gtk-use-system-tooltips nil))
;;  
;;;; font
;;(use-package frame
;;  :preface
;;  (defun func/default-font ()
;;    (interactive)
;;    (when (member "Roboto Mono" (font-family-list))
;;      (set-face-attribute 'default nil :family "Roboto Mono"))
;;    (set-face-attribute 'default nil
;;                        :height 100
;;                        :weight 'normal))
;;  :config
;;  (func/default-font))
;;
;;;; display line number
;;(use-package display-line-numbers
;;  :preface
;;  (defun func/cicle-line-numbers-mode ()
;;  "Cicle between line numbers mode or don't display line number."
;;  (interactive)
;;  (if (bound-and-true-p display-line-numbers-mode)
;;    (if (string= display-line-numbers-type 'visual)
;;        (display-line-numbers-mode -1)
;;      (and (setq-local display-line-numbers-type 'visual)
;;           (display-line-numbers-mode)))
;;    (and (setq-local display-line-numbers-type t)
;;         (display-line-numbers-mode))))
;;  :config
;;  (setq display-line-numbers-grow-only t)
;;  :hook
;;  (prog-mode . display-line-numbers-mode)
;;  :bind
;;  ("<f5>" . func/cicle-line-numbers-mode))
;;
;;;; Highlight line
;;(use-package hl-line
;;  :hook
;;  (prog-mode . hl-line-mode))
;;
;;;; paranthesis
;;(use-package paren
;;  :defer 1
;;  :config
;;  (setq show-paren-delay 0)
;;  (setq show-paren-when-point-in-periphery t)
;;  (setq show-paren-when-point-inside-paren t)
;;  (setq blink-matching-paren nil)
;;  (show-paren-mode 1))
;;
;;;; selection
;;(use-package delsel
;;  :defer 1
;;  :config
;;  (delete-selection-mode +1))
;;
;;;; subword
;;(use-package subword
;;  :hook
;;  (prog-mode-hook . subword-mode))
;;
;;;; advanced command
;;(use-package novice
;;  :init
;;  (setq disabled-command-function nil))
;;
;;;; files
;;(use-package files
;;  :defer 1
;;  :preface
;;  (defconst history-dir (concat user-emacs-directory "history/"))
;;  (defconst etc-dir (concat user-emacs-directory "etc/"))
;;  (defconst lisp-dir (concat user-emacs-directory "lisp/"))
;;  :config
;;  (setq auto-save-list-file-name (concat history-dir "autosave"))
;;  (setq backup-directory-alist `(("." . ,(concat history-dir "backup/"))))
;;  (setq backup-by-copying t)
;;  (setq version-control t)
;;  (setq delete-old-versions t)
;;  (setq kept-new-versions 6)
;;  (setq create-lockfiles nil)
;;  (setq find-file-visit-truename t)
;;  (setq find-file-suppress-same-file-warnings t)
;;  (setq revert-without-query '(".*"))
;;  (setq confirm-kill-processes nil)
;;  (setq confirm-kill-emacs #'y-or-n-p))
;;
;;;; recent files
;;(use-package recentf
;;  :defer 1
;;  :config
;;  (setq recentf-save-file (concat history-dir "recentf"))
;;  (setq recentf-auto-cleanup 'never)
;;  (recentf-mode 1))
;;
;;;; save history
;;(use-package savehist
;;  :defer 1
;;  :config
;;  (setq savehist-file (concat history-dir "savehist"))
;;  (setq savehist-save-minibuffer-history t)
;;  (setq savehist-autosave-interval nil)
;;  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
;;  (savehist-mode 1))
;;
;;;; save places
;;(use-package saveplace
;;  :defer 1
;;  :config
;;  (setq save-place-file (concat history-dir "saveplace"))
;;  (save-place-mode 1))
;;
;;;; desktop history
;;(use-package desktop
;;  :defer 1
;;  :config
;;  (setq desktop-dirname (concat etc-dir "desktop"))
;;  (setq desktop-base-file-name "autosave")
;;  (setq desktop-base-lock-name "autosave-lock"))
;;
;;;; custom edit file
;;(use-package cus-edit
;;  :defer 1
;;  :config
;;  (setq custom-file (concat etc-dir "custom.el")))
;;
;;;; autorevert
;;(use-package autorevert
;;  :defer 2
;;  :config
;;  (setq auto-revert-verbose t)
;;  (setq global-auto-revert-non-file-buffers t)
;;  (global-auto-revert-mode 1))
;;
;;;; org-mode
;;(use-package org
;;  :commands org-mode)
;;
;;;; dired
;;(use-package dired
;;  :commands dired-mode
;;  :preface
;;  (defun func/dired-goto-subdirectory ()
;;    "Go to subdirectory without creating new buffer"
;;    (interactive)
;;    (find-alternate-file ".."))
;;  :config
;;  (setq dired-auto-revert-buffer t)
;;  (setq dired-recursive-copies 'always)
;;  (setq dired-recursive-deletes 'top)
;;  (setq delete-by-moving-to-trash t)
;;  (setq dired-listing-switches "-AlFhv --color=always --group-directories-first")
;;  (setq dired-dwim-target t)
;;  :bind
;;  (:map dired-mode-map
;;              ("z" . #'func/dired-goto-subdirectory))
;;  :hook
;;  (dired-mode-hook . dired-hide-details-mode)
;;  (dired-mode . hl-line-mode))
;;
;;;;; ibuffer
;;(use-package ibuffer
;;  :config
;;  (setq ibuffer-expert t)
;;  (setq ibuffer-use-other-window nil)
;;  (setq ibuffer-show-empty-filter-groups nil)
;;  (setq ibuffer-saved-filter-groups
;;        '(("Main"
;;           ("Directories" (mode . dired-mode))
;;           ("Org" (mode . org-mode))
;;           ("Programming" (mode . prog-mode))
;;           ("Markdown" (mode . markdown-mode))
;;           ("Ledger" (mode . ledger-mode))
;;           ("Terminal" (mode . vterm))
;;           ("Magit" (or
;;                    (mode . magit-blame-mode)
;;                    (mode . magit-cherry-mode)
;;                    (mode . magit-diff-mode)
;;                    (mode . magit-log-mode)
;;                    (mode . magit-process-mode)
;;                    (mode . magit-status-mode)))
;;           ("Emacs" (or
;;                    (name . "^\\*Help\\*$")
;;                    (name . "^\\*Custom.*")
;;                    (name . "^\\*Org Agenda\\*$")
;;                    (name . "^\\*info\\*$")
;;                    (name . "^\\*scratch\\*$")
;;                    (name . "^\\*Backtrace\\*$")
;;                    (name . "^\\*Completions\\*$")
;;                    (name . "^\\*straight-process\\*$")
;;                    (name . "^\\*Messages\\*$"))))))
;;  :hook
;;  (ibuffer-mode . hl-line-mode)
;;  (ibuffer-mode . (lambda ()
;;                    (ibuffer-switch-to-saved-filter-groups "Main")))
;;  :bind
;;  (([remap list-buffers] . #'ibuffer)))
;;
;;;;; Third-party Packages
;;(use-package icomplete-vertical
;;  :ensure t
;;  :demand t
;;  :custom
;;  (completion-styles '(partial-completion substring))
;;  (completion-category-overrides '((file (styles basic substring))))
;;  (read-file-name-completion-ignore-case t)
;;  (read-buffer-completion-ignore-case t)
;;  (completion-ignore-case t)
;;  :config
;;  (icomplete-mode)
;;  (icomplete-vertical-mode)
;;  :bind
;;  (:map icomplete-minibuffer-map
;;        ("<down>" . icomplete-forward-completions)
;;        ("C-n" . icomplete-forward-completions)
;;        ("<up>" . icomplete-backward-completions)
;;        ("C-p" . icomplete-backward-completions)
;;        ("C-v" . icomplete-vertical-toggle)
;;        ("<tab>" . icomplete-force-complete)
;;        ("<backtab>" . icomplete-fido-backward-updir)
;;        ("<return>" . icomplete-force-complete-and-exit) ))
;;
;;(use-package orderless
;;  :ensure t
;;  :after icomplete-vertical
;;  :custom
;;  (completion-styles '(orderless)))
;;
;;;; magit
;;(use-package with-editor
;;  :after magit
;;  :ensure t)
;;
;;(use-package transient
;;  :after magit
;;  :ensure t
;;  :config
;;  (setq transient-levels-file (concat etc-dir "transient/levels"))
;;  (setq transient-values-file (concat etc-dir "transient/values"))
;;  (setq transient-history-file (concat etc-dir "transient/history"))
;;  (transient-bind-q-to-quit))
;;
;;(use-package magit
;;  :ensure t
;;  :bind
;;  ("C-x g" . #'magit-status)
;;  ("C-x M-g" . #'magit-dispatch)
;;  ("C-c M-g" . #'magit-file-dispatch))
;;
;;;; Ledger
;;(use-package ledger-mode
;;  :ensure t
;;  :mode
;;  ("\\.ldg\\'"
;;   "\\.ledger\\'"))
;;
;;;; Modus theme
;;(use-package modus-operandi-theme
;;  :ensure t
;;  :config
;;  (use-package modus-vivendi-theme
;;    :ensure t)
;;  (defun modus-themes-toggle ()
;;    "Toggle between `modus-operandi' and `modus-vivendi' themes."
;;    (interactive)
;;    (if (eq (car custom-enabled-themes) 'modus-operandi)
;;        (progn
;;          (disable-theme 'modus-operandi)
;;          (load-theme 'modus-vivendi t))
;;      (disable-theme 'modus-vivendi)
;;      (load-theme 'modus-operandi t)))
;;  :hook
;;  (after-init-hook . modus-themes-toggle))
;;
;;;; SML
;;(use-package sml-mode
;;  :ensure t
;;  :commands sml-mode)
