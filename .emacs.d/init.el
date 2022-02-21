;; -*- lexical-binding: t -*-

;; Startup Performace
;; The default is 800 kb. measured in bytes
(setq gc-cons-threshold (* 50 1000 1000))
;; change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
	  url-history-file (expand-file-name "url/history" user-emacs-directory))


;; autosave
(setq backup-by-copying t    ; don't clobber symlinks
	  backup-directory-alist '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
	  delete-old-versions t
	  kept-new-versions 6
	  kept-old-versions 2
	  version-control t
	  create-lockfiles nil)
(setq auto-save-file-name-transforms
	  `((".*" "~/.emacs.d/saves" t)))


;; avoid silly errors
(set-default-coding-systems 'utf-8)


;; hide startup message
(setq-default inhibit-splash-screen t
			  inhibit-startup-message t
			  tab-width 4
			  c-basic-offset 4
			  compilation-scroll-output t)


;; Enable `relative` line numbers
(column-number-mode)
(global-display-line-numbers-mode)
;; (setq display-line-numbers-type 'relative)
;; Use relative numbers only in GUI
;; relative numbers cause unpleasant flickering in terminal emacs.
(if (display-graphic-p)
	(progn
	  (setq display-line-numbers-type 'relative)))

;; disable line numbers in these modes
(dolist (mode '(org-mode-hook
				term-mode-hook
				shell-mode-hook
				eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; custom modeline (time-format)
(setq display-time-format "%l:%M: %p %b %y"
	  display-time-default-load-average nil)


;; hide scrollbar, menubar and toolbar. highlight braces
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)

;; stop the annoying beep sound
(setq ring-bell-function 'ignore)

;; mouse config
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


;; maximize window by default
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; escape
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; set font
;; WARNING: You should have Fira Code font installed
;; on your system. change the font or delete the following
;; region if you dont want to deal with it
(set-face-attribute 'default nil
					:font "Fira Code-9"
					:weight 'normal)

;; keybindings emacs way
(global-unset-key "\C-l")
(defvar ctl-l-map (make-keymap)
  "Keymap for local bindings and functions, prefixed by (^L)")
(define-key global-map "\C-l" 'Control-L-prefix)
(fset 'Control-L-prefix ctl-l-map)

(define-key ctl-l-map "l"  'recenter-top-bottom)
(define-key ctl-l-map "g"  'goto-line)
(define-key ctl-l-map "r"  'replace-string)
(define-key ctl-l-map "R"  'replace-regexp)
(define-key ctl-l-map "q"  'query-replace)
(define-key ctl-l-map "Q"  'query-replace-regexp)
(define-key ctl-l-map "T"  'delete-trailing-whitespace)
(define-key ctl-l-map "k"  'kill-current-buffer)


;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("melpa-stable" . "https://stable.melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("elpa" . "https://elpa.gnu.org/packages/")))


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; load theme
(unless (package-installed-p 'solarized-theme)
  (package-install 'solarized-theme))

;; another cool theme
(unless (package-installed-p 'gruber-darker-theme)
  (package-install 'gruber-darker-theme))

;; more readable :)
(if (display-graphic-p)
	(progn
	  ;; if graphic
	  (load-theme 'solarized-dark t))
  ;; else (inside terminal?)
  (load-theme 'gruber-darker t))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package diminish :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 4.0))

(use-package undo-tree
  :ensure t
  :diminish
  :init
  (global-undo-tree-mode 1))

;; Enable evil
(use-package evil
  :ensure t
  :diminish
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)

  ;; Disable -- INSERT -- and such messages
  (setq evil-insert-state-message nil)
  (setq evil-normal-state-message nil)
  (setq evil-emacs-state-message nil)
  (setq evil-visual-state-message nil)
  (setq evil-replace-state-message nil)

  (setq evil-disable-insert-state-bindings t)

  :config
  (evil-mode 1)

  ;; (define-key evil-emacs-state-map (kbd "C-[") 'evil-normal-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
  (defadvice evil-insert-state (around emacs-state-instead-of-insert-state activate)
	(evil-emacs-state))
  )

(define-key ctl-l-map "et" 'evil-mode) ;; toggle evil-mode

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)))

(define-key ctl-l-map "dl" 'dired-find-file)
(define-key ctl-l-map "dh" 'dired-up-directory)
(define-key ctl-l-map "dd" 'dired-do-delete)
(define-key ctl-l-map "dM" 'dired-do-chmod)
(define-key ctl-l-map "dt" 'dired-do-touch)
(define-key ctl-l-map "dr" 'dired-do-rename)
(define-key ctl-l-map "dD" 'dired-create-directory)


;; some more keybindings
(global-set-key (kbd "C-M-, m") 'mark-sexp)
(global-set-key (kbd "C-M-, w") 'mark-word)


(use-package general
  :ensure t
  :config
  (general-evil-setup t)

  (general-create-definer usf/leader-kdef
	:keymaps '(normal insert visual emacs)
	:prefix "\\"
	:global-prefix "C-\\")

  (general-create-definer usf/cc-keys
	:prefix "C-c"))

(usf/leader-kdef
  "t"  '(:ignore t :which-key "toggles")
  "tw" 'whitespace-mode
  "tt" '(counsel-load-theme :which-key "choose theme"))

(use-package magit
  :ensure t
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch))

(usf/leader-kdef
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)

(use-package git-gutter
  :ensure t
  :defer t
  :diminish
  :hook ((text-mode . git-gutter-mode)
		 (prog-mode . git-gutter-mode)))

(use-package rust-mode :ensure t)
(use-package go-mode :ensure t)


(use-package lsp-mode
  :ensure t
  :defer
  ;; :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
		 ;; (python-mode . lsp-deferred)
		 ;; (c-mode . lsp)
		 ;; (c++-mode  lsp
		 ;; (rust-mode . lsp)
		 ;; (go-mode . lsp)
		 ;; if you want which-key integration
		 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; optionally
(use-package lsp-ui
  :ensure t
  :after lsp
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :after (ivy lsp)
  :commands lsp-ivy-workspace-symbol)

(use-package flycheck
  :ensure t
  :defer t
  :hook (lsp-mode . flycheck-mode))

(usf/leader-kdef
  "ae" 'flycheck-mode)

(use-package company
  :ensure t
  :diminish
  :init
  :bind
  (:map company-active-map
		("<tab>" . company-indent-or-complete-common))
  )
(global-company-mode)

(usf/leader-kdef
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "ls" 'counsel-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action)

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
		 :map ivy-minibuffer-map
		 ("TAB" . ivy-alt-done)
		 ("C-f" . ivy-alt-done)
		 ("C-l" . ivy-alt-done)
		 ("C-j" . ivy-next-line)
		 ("C-k" . ivy-previous-line)
		 :map ivy-switch-buffer-map
		 ("C-k" . ivy-previous-line)
		 ("C-l" . ivy-done)
		 ("C-d" . ivy-switch-buffer-kill)
		 :map ivy-reverse-i-search-map
		 ("C-k" . ivy-previous-line)
		 ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
		 ("C-x b" . counsel-ibuffer)
		 ("C-x C-f" . counsel-find-file)
		 ;; ("C-M-j" . counsel-switch-buffer)
		 ("C-M-l" . counsel-imenu))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package evil-nerd-commenter
  :ensure t
  :after evil)

(usf/leader-kdef
  "c <SPC>" 'evilnc-comment-or-uncomment-lines)

(defun c-mode-conf ()
  (c-set-style "linux")
  (setq c-basic-offset 4))

(add-hook 'c-mode-hook 'c-mode-conf)

(require 'compile)


(define-key ctl-l-map "cc" 'compile)
(define-key ctl-l-map "cr" 'recompile)

;; when compiling open a new buffer and switch
;; to the buffer automatically
;; without any fancy splits or anything
(setq special-display-buffer-names
	  '("*compilation*"))

(setq special-display-function
	  (lambda (buffer &optional args)
		;; (split-window)
		(switch-to-buffer buffer)
		(get-buffer-window buffer 0)))


(setq comment-auto-fill-only-comments t)
(auto-fill-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(embark marginalia vertico yaml-mode which-key use-package undo-tree typescript-mode solarized-theme rust-mode magit lsp-ui lsp-ivy gruvbox-theme gruber-darker-theme go-mode git-gutter general flycheck evil-nerd-commenter evil diminish counsel company atom-one-dark-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
