;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          Michael's Emacs stuff
;;;                          ---------------------
;;; A lot of this stuff has been grabbed from the EmacsWiki, StackOverflow,
;;; blogs, etc.
;;;
;;; One day I'll get around to making all of the indentation consistent...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Add MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; setup ~/.emacs.d dirs
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

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; for quickly reloading a changed buffer
(global-set-key "\C-x\ \C-v" 'find-alternate-file)

;; turn off auto-fill-mode
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; setup tabs (tab key, not the other kind)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 200 4))
(setq cperl-indent-level 4)

;; clean up the widndow a bit
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;; fuck the startup screen off
(setq inhibit-startup-screen t)

;; maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; line numbers
(global-linum-mode 1)

;; recent files list
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; redo+
;; requires lisp/redo+.el
(require 'redo+)
(global-set-key (kbd "C-?") 'redo)

;; highlight text past columns 80 and 100
;; requires lisp/column-marker.el
(require 'column-marker)
(add-hook 'prog-mode-hook (lambda ()
  (interactive) (column-marker-1 80)))
(add-hook 'prog-mode-hook (lambda ()
  (interactive) (column-marker-2 100)))

;; column indicator at line 120 - you don't wanna cross this bad boy
(add-hook 'prog-mode-hook (lambda ()
  (display-fill-column-indicator-mode)))
(setq-default display-fill-column-indicator-column 120)

;; projectile (project manager)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; for loading the packages below
(require 'use-package)

;;; Emacs Doom themes
;;; -----------------

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; For treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


;;; Bigass Treemacs section
;;; ------------------------

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
    (treemacs-resize-icons 44)

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


;;; Language-specific stuff
;;; -----------------------


;;; perl
;;; ----

;; cperl-mode is preferred to perl-mode
;; "Brevity is the soul of wit" <foo at acm.org>
(defalias 'perl-mode 'cperl-mode)

;; auto-mode for perl
(setq auto-mode-alist
    (append auto-mode-alist
        ;; perl module or library
        '(("\\.p\[lm]\\'" . cperl-mode)
        ;; test file
        ("\\.t\\'" . cperl-mode))))


;;; Java
;;; ----

;; JDEE (Java Development Environment for Emacs) is installed via MELPA, but
;; an external server .jar is required. This is included in ~.emacs.d/jars and
;; configured below.

(custom-set-variables
    '(jdee-server-dir "~/.emacs.d/jars"))

;; auto-mode for Java
(setq auto-mode-alist
    (append auto-mode-alist
        '(("\\.java\\'" . java-mode))))


;;; Markdown
;;; --------

;; Some markdown modes are installed from MELPA, but I haven't gotten around to
;; configuring them yet. Watch this space

;; auto-mode for Markdown
(setq auto-mode-alist
    (append auto-mode-alist
        '(("\\.md\\'" . markdown-mode))))


;;; ---------------------------
;;; End language-specific stuff