;; emacs.el --- Emacs configuration file -*- lexical-binding: t -*-

;;; Optimization
;; built-in emacs package manager
(setq package-enable-at-startup nil)

;; Resize frame
(setq frame-inhibit-implied-resize t)

;; Startup screen
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)
;(fset #'display-startup-echo-area-message #'ignore)

;; Graphical elements 
(when (display-graphic-p)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (tool-bar-mode -1))

;; Dialog box
(setq use-file-dialog nil)


;;; Setting
;; Frame title
(setq frame-title-format "%b [%m]")

;; Frame resize behaviour
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

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

;; Font
(add-to-list 'default-frame-alist '(font . "Cascadia Code-10"))

;; Line number format
(setq display-line-numbers-widen t)

;;; Package manager
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

;;; Built-in packages
;; org-mode
(use-feature org
  :defer 5)

;;; Backup files
(use-feature files
  :init
  (setq auto-save-list-file-name (concat history-dir "autosave")
        backup-directory-alist `(("." . ,(concat history-dir "backup/")))
        make-backup-files nil
        backup-by-copying t
        create-lockfiles nil
        auto-save-default nil
        find-file-visit-truename t
        find-file-suppress-same-file-warnings t
        revert-without-query '(".*")
        confirm-kill-emacs #'y-or-n-p
        ))

;; History
(use-feature recentf
  :init
  (setq recentf-save-file (concat history-dir "recentf")
        recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

(use-feature savehist
  :defer 1
  :config
  (setq savehist-file (concat history-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode 1))

(use-feature saveplace
  :defer 1
  :config
  (setq save-place-file (concat history-dir "saveplace"))
  (save-place-mode 1))

(use-feature desktop
  :defer 1
  :config
  (setq desktop-dirname (concat etc-dir "desktop")
        desktop-base-file-name "autosave"
        desktop-base-lock-name "autosave-lock"))

;; Mouse wheel
(use-feature mwheel
  :init
  (setq mouse-wheel-scroll-amount
        '(1 ((shift) . 5) ((control)))
        ))

;; Prefer vertical split
(use-feature window
  :init
  (setq split-height-threshold nil))

;; Display line number
(use-feature display-line-numbers
  :init
  (setq display-line-numbers-grow-only t
        )
  :config
  (defun cicle-line-numbers-mode ()
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
  ("<f5>" . cicle-line-numbers-mode))


;; Highlight line
(use-feature hl-line
  :hook
  (prog-mode . hl-line-mode))

;; Selection
(use-feature delsel
  :init
  (delete-selection-mode +1))

;; simple.el
(use-feature simple
  :init
  (setq shift-select-mode nil
        column-number-mode 1))

;; Custom edit
(use-feature cus-edit
  :defer 5
  :config
  (setq custom-file (concat etc-dir "custom.el")))

;; Advanced command
(use-feature novice
  :init
  (setq disabled-command-function nil))

;; Autorevert
(use-feature autorevert
  :defer 2
  :config
;;  (setq auto-revert-interval 1)
  (global-auto-revert-mode +1)
  (setq global-auto-revert-non-file-buffers t))

;; Dired
(use-feature dired
  :defer 2
  :config
  (setq dired-auto-revert-buffer t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        delete-by-moving-to-trash t
        dired-listing-switches "-Alh"
        dired-dwim-target t)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)))

;;; Ibuffer
(use-feature ibuffer
  :defer 2
  :config
  (setq ibuffer-expert t
        ibuffer-use-other-window nil
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        '(("Main"
           ("Directories" (mode . dired-mode))
           ("Org" (mode . org-mode))
           ("Programming" (mode . prog-mode))
           ("Markdown" (mode . markdown-mode))
           ("Ledger" (mode. ledger-mode))
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

;; Hippie expand
(use-feature hippie-exp
  :defer 2
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

;; Matching Paranthesis
(use-feature paren
  :init
  (setq show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t
        blink-matching-paren nil)
  (show-paren-mode 1))

;; Emacs vc
(use-feature vc-hooks
  :config
  (setq vc-handled-backends nil))
;; vc-follow-symlinks t

;;; External packages
;; Solarized themes
(use-package emacs-color-theme-solarized
  :straight (:host github :repo "sellout/emacs-color-theme-solarized")
  :init
  (setq solarized-termcolor 256
        solarized-broken-srgb t
        solarized-contrast 'normal)

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

;; Ledger
(use-package ledger-mode
  :defer 5
  :mode
  ("\\.dat\\'"
   "\\.ledger\\'"))

;; magit
(use-package with-editor
  :defer 5)

(use-package transient
  :after with-editor
  :config
  (setq transient-levels-file  (concat etc-dir "transient/levels")
        transient-values-file  (concat etc-dir "transient/values")
        transient-history-file (concat etc-dir "transient/history"))
  (transient-bind-q-to-quit))

(use-package magit
  :bind
  ("C-x g" . #'magit-status)
  ("C-x M-g" . #'magit-dispatch)
  ("C-c M-g" . #'magit-file-dispatch))
