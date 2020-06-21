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
(setq-default display-line-numbers-widen t)
(setq locale-coding-system 'utf-8)
(setq load-prefer-newer t)
(setq x-underline-at-descent-line t)
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

;; tooltip
(use-feature tooltip
  :config
  (setq tooltip-delay 0.5)
  (setq tooltip-use-echo-area t)
  (setq x-gtk-use-system-tooltips nil))
  
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
  :defer 1
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
  :defer 1
  :config
  (setq recentf-save-file (concat history-dir "recentf"))
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1))

;; save history
(use-feature savehist
  :defer 1
  :config
  (setq savehist-file (concat history-dir "savehist"))
  (setq savehist-save-minibuffer-history t)
  (setq savehist-autosave-interval nil)
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode 1))

;; save places
(use-feature saveplace
  :defer 1
  :config
  (setq save-place-file (concat history-dir "saveplace"))
  (save-place-mode 1))

;; desktop history
(use-feature desktop
  :defer 1
  :config
  (setq desktop-dirname (concat etc-dir "desktop"))
  (setq desktop-base-file-name "autosave")
  (setq desktop-base-lock-name "autosave-lock"))

;; custom edit file
(use-feature cus-edit
  :defer 1
  :config
  (setq custom-file (concat etc-dir "custom.el")))

;; autorevert
(use-feature autorevert
  :defer 2
  :config
  (setq auto-revert-verbose t)
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

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
           ("Terminal" (mode . vterm))
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


;;; Third-party Packages
;; selectrum
(use-package selectrum
  :defer 1
  :config
  (defun func/recentf-open-files ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

  (defvar selectrum-swiper-history nil "Submission history for `selectrum-swiper'.")
  (defun func/selectrum-swiper ()
    "Search for a matching line and jump to the beginning of its text.  Obeys narrowing."
    (interactive)
    (let* ((selectrum-should-sort-p nil)
           (line-choices (cl-loop
                          with minimum-line-number = (line-number-at-pos (point-min) t)
                          with buffer-text-lines = (split-string (buffer-string) "\n")
                          with number-format = (format "L%%0%dd: " (length (number-to-string (length buffer-text-lines))))
                          for txt in buffer-text-lines
                          for num from minimum-line-number to (+ minimum-line-number
                                                                 (1- (length buffer-text-lines)))
                          unless (string-empty-p txt) ; Just skip empty lines.
                          collect (concat (format number-format num) txt)))
           ;; Get the matching line.
           (chosen-line (completing-read "Jump to matching line: " line-choices
                                         nil t nil 'selectrum-swiper-history))
           ;; Stop at the ":". It is followed by one " ".
           (line-number-prefix (seq-take-while (lambda (char)
                                                 (not (char-equal ?: char)))
                                               chosen-line))
           ;; Get the corresponding line number, skipping the "L" in line-number-prefix.
           (chosen-line-number (string-to-number (substring line-number-prefix 1)))
           ;; Get the current line number for determining the travel distance.
           (current-line-number (line-number-at-pos (point) t)))
  
      (push-mark (point) t)
      ;; Manually edit history to remove line numbers.
      (setcar selectrum-swiper-history (substring chosen-line
                                                  ;; Want after line-prefix followed by ": ".
                                                  (+ (length line-number-prefix) 2)))
      (forward-line (- chosen-line-number current-line-number))
      (beginning-of-line-text 1)))
  
  (selectrum-mode +1)
  :bind
  ("C-x C-r" . #'func/recentf-open-files)
  ("M-g s" . #'func/selectrum-swiper))

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
  ("M-<f7>" . bm-toggle))

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

;; ;; Solarized themes
;; (use-package emacs-color-theme-solarized
;;   :straight (:host github :repo "sellout/emacs-color-theme-solarized")
;;   :disabled
;;   :init
;;   (setq solarized-termcolor 256)
;;   (setq solarized-broken-srgb t)
;;   (setq solarized-contrast 'normal)
;;   (defun solarized-light ()
;;       (load-theme 'solarized t)
;;       (set-frame-parameter nil 'background-mode 'light)
;;       (enable-theme 'solarized))

;;   (defun solarized-dark ()
;;       (load-theme 'solarized t)
;;       (set-frame-parameter nil 'background-mode 'dark)
;;       (enable-theme 'solarized))

;;   (defun solarized-switch ()
;;       (interactive)
;;       (if (string= (frame-parameter nil 'background-mode) 'light)
;;           (solarized-dark)
;;         (solarized-light)))

;;   (solarized-light)
;;   :bind
;;   ("<f6>" . #'solarized-switch))

;; Ledger
(use-package ledger-mode
  :mode
  ("\\.dat\\'"
   "\\.ledger\\'"))

;; vterm
(use-package vterm)
(use-package vterm-toggle
  :straight (:host github :repo "jixiuf/vterm-toggle")
  :after vterm
  :bind
  ("<f9>" . #'vterm-toggle)
  ("M-<f9>" . #'vterm-toggle-cd))
  
;; solarized-theme 2
(use-package solarized-theme
  :straight (:host github :repo "rafiyqw/solarized-emacs")
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-less-bold t)
  (defun func/solarized-theme-switch ()
    (interactive)
    (if (eq (car custom-enabled-themes) 'solarized-dark-high-contrast)
        (load-theme 'solarized-light-high-contrast)
      (load-theme 'solarized-dark-high-contrast)))
 
;;  :hook (after-init-hook . (load-theme 'solarized-dark-high-contrast))
  :bind
  ("<f6>" . func/solarized-theme-switch))
