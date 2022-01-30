;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          Michael's Emacs stuff
;;;                          ---------------------
;;; A lot of this stuff has been grabbed from the EmacsWiki, StackOverflow,
;;; blogs, etc.
;;;
;;; One day I'll get around to making all of the indentation consistent...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Add MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Download and install packages
(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;; Ensure packages are kept up to date
(use-package auto-package-update
  :config (setq auto-package-update-delete-old-versions t
                auto-package-update-hide-results t)
          (auto-package-update-maybe))

;; Setup dirs
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Put backup files neatly away
(let ((backup-dir "~/.emacs.d/backups")
      (auto-saves-dir "~/.emacs.d/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

;; Change some default variables
(setq backup-by-copying t   ; Don't delink hardlinks
      delete-old-versions t ; Clean up the backups
      version-control t     ; Use version numbers on backups,
      kept-new-versions 5   ; keep some new versions
      kept-old-versions 2   ; and some old ones, too
      tab-width 4
      tab-stop-list (number-sequence 4 200 4)
      inhibit-startup-screen t
      ring-bell-function 'ignore
      column-number-mode t
      use-dialog-box nil    ; Put prompts in minibuffer
      use-short-answers t   ; y/n instead of yes/no
      load-prefer-newer t   ; Prefer newer elisp files
      ;x-select-enable-clipboard nil ; Keep the Emacs kill ring
      ;x-select-enable-primary nil   ; out of the system clipboard.
      compilation-read-command nil
      compilation-scroll-output 'first-error)

(setq-default indent-tabs-mode nil)
(global-set-key "\C-x\ \C-v" 'find-alternate-file) ; Quickly reload a changed buffer
(auto-fill-mode -1) ; Turn off auto-fill-mode
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(when (window-system)
  (tool-bar-mode -1)    ; No tool bar
  (scroll-bar-mode -1)  ; No scroll bar
  (tooltip-mode -1))    ; no GUI tooltips
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; Start maximized
(global-linum-mode 1)   ; line numbers on side
(set-charset-priority 'unicode) ; UTF-8 as default
(prefer-coding-system 'utf-8-unix)

;; Recent files list
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; redo+
;; Requires lisp/redo+.el
(require 'redo+)
(global-set-key (kbd "C-?") 'redo)

;; Highlight text past columns 80 and 100
;; Requires lisp/column-marker.el
(require 'column-marker)
(add-hook 'prog-mode-hook (lambda ()
  (interactive) (column-marker-1 80)))
(add-hook 'prog-mode-hook (lambda ()
  (interactive) (column-marker-2 100)))

;; Column indicator at line 120 - you don't wanna cross this bad boy
(add-hook 'prog-mode-hook (lambda ()
  (display-fill-column-indicator-mode)))
(setq-default display-fill-column-indicator-column 120)

;; guess-style
;; Requires lisp/guess-style.el
;(require 'guess-style)
;(autoload 'guess-style-set-variable "guess-style" nil t)
;(autoload 'guess-style-guess-variable "guess-style")
;(autoload 'guess-style-guess-all "guess-style" nil t)
;(add-hook 'prog-mode-hook 'guess-style-guess-all)

;;; Packages and package configuration
;;; ---------------------------------

;; A bunch of essentials
(use-package gruvbox-theme)
(use-package fzf)
(use-package flycheck
  :init (global-flycheck-mode)
  :config (use-package flycheck-inline
            :config (global-flycheck-inline-mode)))
(use-package which-key
  :config (which-key-mode))
(use-package yasnippet)
(use-package hydra)
(use-package avy)


;; Helm
;; ----
(use-package helm-xref
  :config (helm-mode)
  :bind ([remap find-file] . helm-find-files)
        ([remap execute-extended-command] . helm-M-x)
        ([remap switch-to-buffer] . helm-mini))


;; Projectile
;; ---------
(use-package projectile
  :diminish projectile-mode
  :bind (("C-c k" . #'projectile-kill-buffers)
         ("C-c m" . #'projectile-compile-project))
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom (projectile-enable-caching t)
  :config (projectile-mode)
  :init (when (file-directory-p "~/Projects")
          (setq projectile-project-search-path '("~/Projects")))
        (setq projectile-switch-project-action #'projectile-dired))


;; Magit
;; ----
(use-package magit
  :config (use-package forge)
          ;(use-package libgit
          ;  :config (use-package magit-libgit))
          (unless (boundp 'bug-reference-auto-setup-functions)
             (defvar bug-reference-auto-setup-functions '())))


;; LSP mode
;; --------
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (lsp-mode . lsp-enable-which-key-integration)
        (c-mode . lsp-deferred)
	(c++-mode . lsp-deferred)
	(cperl-mode . lsp-deferred)
  :config (setq lsp-completion-enable-additional-text-edit nil)
          (use-package lsp-ui
            :custom (lsp-ui-doc-show-with-mouse t)
                    (lsp-ui-doc-show-with-cursor nil)
                    (lsp-ui-doc-delay 1.5)
            :config (lsp-ui-doc-mode)
            :commands lsp-ui-mode)
          (use-package helm-lsp)
          (use-package lsp-treemacs
            :custom (lsp-treemacs-sync-mode 1))
          (use-package lsp-java
            :hook (java-mode-hook . lsp-deferred)
            :config (use-package lsp-java-boot
                      :hook (lsp-mode-hook . lsp-lens-mode)
                            (java-mode-hook . lsp-java-boot-lens-mode)))
          (use-package ccls
            :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp-deferred))))
  :commands lsp lsp-deferred)

          
;; Company
;; -------
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
        (after-init-hook . global-company-mode)
  :bind (:map company-active-map ("<tab>" . company-complete-selection))
        (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom (company-minimum-prefix-length 1)
          (company-idle-delay 0.0)
  :config (use-package company-box
            :hook (company-mode . company-box-mode)))
          
;; Requires PlSense perl module, abandoned
;;(use-package company-plsense)
          

;; DAP mode
;; --------
(use-package dap-mode
  :after lsp-mode
  :bind ("C-c b b" . dap-breakpoint-toggle)
	("C-c b r" . dap-debug-restart)
	("C-c b l" . dap-debug-last)
	("C-c b d" . dap-debug)
  :config (dap-mode)
          (dap-auto-configure-mode)
	  (dap-ui-mode)
	  (dap-ui-controls-mode)
          (use-package dap-java
            :ensure nil)
          (require 'dap-cpptools)
  :hook (dap-stopped-hook . (lambda (arg) (call-interactively #'dap-hydra))))


;; RealGUD
;; -------
(use-package realgud)


;; search and replace replacement
(use-package visual-regexp
  :bind (("C-c 5" . #'vr/replace)))


;; Bigass Treemacs section - copied from Treemacs github page
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   t
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 16)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;;(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;  :ensure t
;;  :config (treemacs-set-scope-type 'Perspectives))


;; Smart tabs mode
(use-package smart-tabs-mode
  :hook (c-mode-common-hook . (lambda () (setq indent-tabs-mode t)))
        (java-mode-hook . (lambda () (setq indent-tabs-mode t)))
        (cperl-mode-hook . (lambda () (setq indent-tabs-mode t)))
  :init (smart-tabs-insinuate 'c 'c++ 'cperl 'java))


;;; Language-specific stuff
;;; -----------------------

;; perl
;; ----

;; cperl-mode is preferred to perl-mode
;; "Brevity is the soul of wit" <foo at acm.org>
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4)

;; Modes for file types
;;(setq auto-mode-alist
;;    (append auto-mode-alist
;;        ("\\.java\\'" . java-mode)
;;        ("\\.md\\'" . markdown-mode))))

