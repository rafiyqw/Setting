;; emacs.el --- Emacs configuration file -*- lexical-binding: t -*-

;; SETTING
;;; Optimization
;; disable built-in emacs package manager
(setq package-enable-at-startup nil)

;; inhibit resize frame
(setq frame-inhibit-implied-resize t)

;; startup screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)
(fset #'display-startup-echo-area-message #'ignore)

;; Graphical elements 
(when (display-graphic-p)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (tool-bar-mode -1))
(menu-bar-mode -1)

;; frame title
(setq frame-title-format "%b [%m]")

;; disable dialog box
(setq use-file-dialog nil)

;; frame resize behaviour
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; Feedback
(setq echo-keystrokes 1e-6)
(setq ring-bell-function #'ignore)
(setq visible-bell t)
(fset #'yes-or-no-p #'y-or-n-p)

;; Scrolling
(setq hscroll-margin 2)
(setq hscroll-step 1)
(setq scroll-conservatively 1)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)

;; Font
(add-to-list 'default-frame-alist '(font . "Cascadia Code-10"))

;; Line number format
(setq display-line-numbers-widen t)


;;; File and Backup
;; directory
(defconst history-dir (concat user-emacs-directory "history/"))
(defconst etc-dir (concat user-emacs-directory "etc/"))
(defconst lisp-dir (concat user-emacs-directory "lisp/"))


;;; Editing
;; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Preserve contents of system clipboard
(setq save-interprogram-paste-before-kill t)

;; utf-8
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)


;; PACKAGE MANAGER
;;; straight.el
;; Detect package modifications
(if (and (executable-find "watchexec")
         (executable-find "python3"))
    (setq straight-check-for-modifications '(watch-files find-when-checking))
  (setq straight-check-for-modifications
        '(find-at-startup find-when-checking)))

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
     ,@args))


;;; Built-in Packages
;; OPTIMIZATIONS
;; Mouse wheel
(use-feature mwheel
  :init
  (setq mouse-wheel-scroll-amount
        '(1 ((shift) . 5) ((control)))))

;; Prefer vertical split
(use-feature window
  :init
  (setq split-height-threshold nil))

;; Display line number
(use-feature display-line-numbers
  :init
  (setq display-line-numbers-grow-only t)
  :config
  (defun func/cicle-line-numbers-mode ()
  "Cicle between line numbers mode or don't display line number."
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
    (if (string= display-line-numbers-type 'visual)
        (display-line-numbers-mode -1)
      (and (setq-local display-line-numbers-type 'visual)
           (display-line-numbers-mode)))
    (and (setq-local display-line-numbers-type t)
         (display-line-numbers-mode))))
  :hook
  (prog-mode . display-line-numbers-mode)
  :bind
  ("<f5>" . func/cicle-line-numbers-mode))


;; FILES
;; Backup files
(use-feature files
  :defer 2
  :config
  (setq auto-save-list-file-name (concat history-dir "autosave"))
  (setq backup-directory-alist `(("." . ,(concat history-dir "backup/"))))
  ;(setq make-backup-files nil)
  (setq backup-by-copying t)
  (setq version-control t)
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq create-lockfiles nil)
  ;(setq auto-save-default nil)
  (setq find-file-visit-truename t)
  (setq find-file-suppress-same-file-warnings t)
  (setq revert-without-query '(".*"))
  (setq confirm-kill-emacs #'y-or-n-p))

;; History
(use-feature recentf
  :defer 2
  :config
  (setq recentf-save-file (concat history-dir "recentf"))
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-feature savehist
  :defer 2
  :config
  (setq savehist-file (concat history-dir "savehist"))
  (setq savehist-save-minibuffer-history t)
  (setq savehist-autosave-interval nil)
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode 1))

(use-feature saveplace
  :defer 2
  :config
  (setq save-place-file (concat history-dir "saveplace"))
  (save-place-mode 1))

(use-feature desktop
  :defer 2
  :config
  (setq desktop-dirname (concat etc-dir "desktop"))
  (setq desktop-base-file-name "autosave")
  (setq desktop-base-lock-name "autosave-lock"))

;; Custom edit
(use-feature cus-edit
  :defer 3
  :config
  (setq custom-file (concat etc-dir "custom.el")))

;; Autorevert
(use-feature autorevert
  :defer 2
  :config
  (setq auto-revert-verbose t)
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

;; Emacs vc
(use-feature vc-hooks
  :defer 2
  :config
  (setq vc-handled-backends nil)
  (setq vc-follow-symlinks t))


;;; Editing
;; Highlight line
(use-feature hl-line
  :hook
  (prog-mode . hl-line-mode))

;; Matching Paranthesis
(use-feature paren
  :config
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren t)
  (setq blink-matching-paren nil)
  (show-paren-mode 1))

;; Selection
(use-feature delsel
  :init
  (delete-selection-mode +1))

;; simple.el
(use-feature simple
  :init
  (setq shift-select-mode nil)
  (setq column-number-mode 1))

;; subword
(use-feature subword
  :hook
  (prog-mode-hook . subword-mode))

;; Advanced command
(use-feature novice
  :init
  (setq disabled-command-function nil))


;; FEATURES
;; org-mode
(use-feature org
  :commands org-mode)

;; dired
(use-feature dired
  :commands dired-mode
  :config
  (setq dired-auto-revert-buffer t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-AlFhv --color=always --group-directories-first")
  (setq dired-dwim-target t)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)))

;;; ibuffer
(use-feature ibuffer
  :commands ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups
        '(("Main"
           ("Directories" (mode . dired-mode))
           ("Org" (mode . org-mode))
           ("Programming" (mode . prog-mode))
           ("Markdown" (mode . markdown-mode))
           ("Ledger" (mode . ledger-mode))
           ("Magit" (or
                    (mode . magit-blame-mode)
                    (mode . magit-cherry-mode)
                    (mode . magit-diff-mode)
                    (mode . magit-log-mode)
                    (mode . magit-process-mode)
                    (mode . magit-status-mode)))
           ("Emacs" (or
                    (name . "^\\*Help\\*$")
                    (name . "^\\*Custom.*")
                    (name . "^\\*Org Agenda\\*$")
                    (name . "^\\*info\\*$")
                    (name . "^\\*scratch\\*$")
                    (name . "^\\*Backtrace\\*$")
                    (name . "^\\*Completions\\*$")
                    (name . "^\\*straight-process\\*$")
                    (name . "^\\*Messages\\*$"))))))
  :hook
  (ibuffer-mode . hl-line-mode)
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "Main")))
  :bind
  (([remap list-buffers] . #'ibuffer)))

;; hippie-expand
(use-feature hippie-exp
  :commands hippie-expand
  :config
  (setq hippie-expand-try-functions-list
        '(
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          ;; try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name
          ;; try-expand-all-abbrevs
          ;; try-expand-list
          ;; try-expand-line
          ))
  :bind
  ("M-/" . hippie-expand))


;;; External packages
;; OPTIMIZATION
;; Solarized themes
(use-package emacs-color-theme-solarized
  :straight (:host github :repo "sellout/emacs-color-theme-solarized")
  :init
  (setq solarized-termcolor 256)
  (setq solarized-broken-srgb t)
  (setq solarized-contrast 'normal)
  (defun solarized-light ()
      (load-theme 'solarized t)
      (set-frame-parameter nil 'background-mode 'light)
      (enable-theme 'solarized))

  (defun solarized-dark ()
      (load-theme 'solarized t)
      (set-frame-parameter nil 'background-mode 'dark)
      (enable-theme 'solarized))

  (defun solarized-switch ()
      (interactive)
      (if (string= (frame-parameter nil 'background-mode) 'light)
          (solarized-dark)
        (solarized-light)))

  (solarized-light)
  :bind
  ("<f6>" . #'solarized-switch))

;; selectrum
(use-package selectrum
  :defer 1
  :init
  (selectrum-mode +1))

;; EDITING
;; ace-jump
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))

;; bookmarks
(use-package bm
  :init
  (setq bm-restore-repository-on-load t)
  :config
  (setq bm-cycle-all-buffers t)
  (setq bm-repository-file (concat history-dir "bm-bookmarks"))
  (setq-default bm-buffer-persistence t)
  :hook
  (after-init-hook . bm-repository-load)
  (kill-buffer-hook . bm-buffer-save)
  (kill-emacs-hook . (lambda ()
                       (bm-buffer-save-all)
                       (bm-repository-save)))
  (after-save-hook . #'bm-buffer-save)
  (find-file-hooks . #'bm-buffer-restore)
  (after-revert-hook . #'bm-buffer-restore)
  (vc-before-checkin-hook . #'bm-buffer-save)
  :bind
  ("<f7>" . bm-next)
  ("S-<f7>" . bm-previous)
  ("<f8>" . bm-toggle))


;; FEATURES
;; Ledger
(use-package ledger-mode
  :mode
  ("\\.dat\\'"
   "\\.ledger\\'"))

;; magit
(use-package with-editor
  :after magit)

(use-package transient
  :after magit
  :config
  (setq transient-levels-file (concat etc-dir "transient/levels"))
  (setq transient-values-file (concat etc-dir "transient/values"))
  (setq transient-history-file (concat etc-dir "transient/history"))
  (transient-bind-q-to-quit))

(use-package magit
  :bind
  ("C-x g" . #'magit-status)
  ("C-x M-g" . #'magit-dispatch)
  ("C-c M-g" . #'magit-file-dispatch))

