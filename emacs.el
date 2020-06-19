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
(menu-bar-mode -1)
(fset #'yes-or-no-p #'y-or-n-p)


;;; Built-in packages
;; startup
(use-feature startup
  :init
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message nil)
  (setq initial-major-mode 'fundamental-mode)
  (fset #'display-startup-echo-area-message #'ignore))

;; scroll-bar
(use-feature scroll-bar
  :config
  (scroll-bar-mode -1))

;; window
(use-feature window
  :init
  (setq split-height-threshold nil))

;; simple.el
(use-feature simple
  :config
  (setq save-interprogram-paste-before-kill t)
  (setq shift-select-mode nil)
  (setq column-number-mode 1))

;; mouse wheel
(use-feature mwheel
  :config
  (setq mouse-wheel-scroll-amount
        '(1 ((shift) . 5) ((control)))))

;; frame
(use-feature frame
  :preface
  (defun func/default-font ()
    (interactive)
    (when (member "Roboto Mono" (font-family-list))
      (set-face-attribute 'default nil :family "Roboto Mono"))
    (set-face-attribute 'default nil
                        :height 100
                        :weight 'normal))
  :config
  (func/default-font))

;; display line number
(use-feature display-line-numbers
  :preface
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
  :config
  (setq display-line-numbers-grow-only t)
  :hook
  (prog-mode . display-line-numbers-mode)
  :bind
  ("<f5>" . func/cicle-line-numbers-mode))

;; Highlight line
(use-feature hl-line
  :hook
  (prog-mode . hl-line-mode))

;; paranthesis
(use-feature paren
  :defer 1
  :config
  (setq show-paren-delay 0)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren t)
  (setq blink-matching-paren nil)
  (show-paren-mode 1))

;; selection
(use-feature delsel
  :defer 1
  :config
  (delete-selection-mode +1))

;; subword
(use-feature subword
  :hook
  (prog-mode-hook . subword-mode))

;; advanced command
(use-feature novice
  :init
  (setq disabled-command-function nil))

;; files
(use-feature files
  :defer 2
  :preface
  (defconst history-dir (concat user-emacs-directory "history/"))
  (defconst etc-dir (concat user-emacs-directory "etc/"))
  (defconst lisp-dir (concat user-emacs-directory "lisp/"))
  :config
  (setq auto-save-list-file-name (concat history-dir "autosave"))
  (setq backup-directory-alist `(("." . ,(concat history-dir "backup/"))))
  (setq backup-by-copying t)
  (setq version-control t)
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq create-lockfiles nil)
  (setq find-file-visit-truename t)
  (setq find-file-suppress-same-file-warnings t)
  (setq revert-without-query '(".*"))
  (setq confirm-kill-processes nil)
  (setq confirm-kill-emacs #'y-or-n-p))

;; recent files
(use-feature recentf
  :defer 2
  :config
  (setq recentf-save-file (concat history-dir "recentf"))
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1))

;; save history
(use-feature savehist
  :defer 2
  :config
  (setq savehist-file (concat history-dir "savehist"))
  (setq savehist-save-minibuffer-history t)
  (setq savehist-autosave-interval nil)
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode 1))

;; save places
(use-feature saveplace
  :defer 2
  :config
  (setq save-place-file (concat history-dir "saveplace"))
  (save-place-mode 1))

;; desktop history
(use-feature desktop
  :defer 2
  :config
  (setq desktop-dirname (concat etc-dir "desktop"))
  (setq desktop-base-file-name "autosave")
  (setq desktop-base-lock-name "autosave-lock"))

;; Custom edit file
(use-feature cus-edit
  :defer 2
  :config
  (setq custom-file (concat etc-dir "custom.el")))

;; org-mode
(use-feature org
  :commands org-mode)

;; dired
(use-feature dired
  :commands dired-mode
  :preface
  (defun func/dired-goto-subdirectory ()
    "Go to subdirectory without creating new buffer"
    (interactive)
    (find-alternate-file ".."))
  :config
  (setq dired-auto-revert-buffer t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-AlFhv --color=always --group-directories-first")
  (setq dired-dwim-target t)
  :bind (:map dired-mode-map
              ("z" . #'func/dired-goto-subdirectory))
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)))

;;; ibuffer
(use-feature ibuffer
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
