;; -*- lexical-binding: t -*-
;; Author: Youssef Hesham <m1cr0xf7>

;; avoid silly errors
(set-default-coding-systems 'utf-8)

;; change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(defvar user-cache-directory (expand-file-name "~/.cache/emacs/"))
(setq user-emacs-directory (expand-file-name "~/.emacs.d/")
      url-history-file (concat user-cache-directory "url/history")
      package-user-dir "~/.emacs.d/packages")

;; autosave
(setq backup-by-copying t    ; don't clobber symlinks
      ;; don't litter my fs tree
      ;; backup-directory-alist '(("." . "~/.emacs.d/saves"))
      auto-save-default nil
      make-backup-files nil
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      create-lockfiles nil
      package-native-compile t)

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/saves" t)))


(setq indent-tabs-mode nil
      tab-width 4
      compilation-scroll-output t
      fill-column 80
      isearch-repeat-on-direction-change t
      isearch-wrap-pause 'no-ding)

(setq comment-auto-fill-only-comments t)
(auto-fill-mode t)

;; make scrolling less painful
;; (setq scroll-margin 4
;;   scroll-conservatively 0
;;   scroll-up-aggressively 0.01
;;   scroll-down-aggressively 0.01)
;; (setq-default scroll-up-aggressively 0.01
;;   scroll-down-aggressively 0.01)


;; It lets you move point from window to window using Shift and the
;; arrow keys. This is easier to type than ‘C-x o’.
(windmove-default-keybindings)

;; this sets HTML tab to 4 spaces (2 spaces is nice, 4 is ugly)
;; (defvaralias 'sgml-basic-offset 'tab-width)

;; Enable `relative` line numbers
(column-number-mode)

;; Use relative numbers only in GUI
;; relative numbers cause unpleasant flickering in terminal emacs.
(if (display-graphic-p)
    (progn
      (setq display-line-numbers-type 'relative)))


(add-hook 'prog-mode-hook (lambda() (display-line-numbers-mode 1)))
(add-hook 'text-mode-hook (lambda() (display-line-numbers-mode 1)))

;; Emacs! act normal wtf?
(delete-selection-mode t)

;; custom modeline (time-format)
(setq display-time-format "%l:%M: %p %b %y"
      display-time-default-load-average nil)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
;; Emacs 28: Hide commands in M-x which do not work in the current mode.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; mouse config
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1 ;; keyboard scroll one line at a time
  mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; one line at a time
  mouse-wheel-progressive-speed nil	       ; don't accelerate scrolling
  mouse-wheel-follow-mouse 't		       ; scroll window under mouse
  ;; scroll-preserve-screen-position 1
  scroll-conservatively 10000)

;; (setq jit-lock-defer-time 0)
;; (setq fast-but-imprecise-scrolling t)

;; escape
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; hippie expand
;; (global-set-key [remap dabbrev-expand] 'hippie-expand)


;; editor
(defun move-region-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
	(transpose-lines arg))
      (forward-line -1)))))
(defun move-region-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-region-internal arg))
(global-set-key (kbd "M-<down>") 'move-region-down)
(defun move-region-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-region-internal (- arg)))
(global-set-key (kbd "M-<up>") 'move-region-up)
(defun duplicate-line ()
  "duplicate the current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))
(defun delete-ws-and-indent ()
  "delete trailing whitespace and indent the whole buffer"
  (interactive)
  (mark-whole-buffer)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))
(defun unhighlight-all-in-buffer ()
  "Remove all highlights made by `hi-lock' from the current buffer."
  (interactive)
  (unhighlight-regexp t))
;; highlight / unhighlight
(global-set-key (kbd "C-#") 'highlight-symbol-at-point)
(global-set-key (kbd "C-*") 'unhighlight-all-in-buffer)
(defun kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
	(kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))
(defun kill-symbol-at-point ()
  "Kill symbol at point."
  (interactive)
  (kill-thing-at-point 'symbol))
(defun kill-word-at-point ()
  "Kill word at point."
  (interactive)
  (kill-thing-at-point 'word))
(defun kill-other-buffers ()
  "Kill all other buffers. except the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(defun select-text-in-quote ()
  (interactive)
  (let ( $skipChars $p1 )
    (setq $skipChars "^\"`<>(){}[]")
    (skip-chars-backward $skipChars)
    (setq $p1 (point))
    (skip-chars-forward $skipChars)
    (set-mark $p1)))
(defun load-if-exists (f)
  "Load a file if exists"
  (if (file-exists-p (expand-file-name f))
      (load-file f)))
(defun generate-tags (extension)
  "Generate a TAGS file for all files with the given EXTENSION."
  (interactive "sEnter file extension (without dot): ")
  (let ((command (format "find . -name \"*.%s\" -print | etags -" extension)))
    (shell-command command)
    (message "TAGS file generated for *.%s files" extension)))
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))
(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
	ad-do-it
      (error (and (buffer-modified-p)
		  (not (ding))
		  (y-or-n-p "Buffer is modified, save it? ")
		  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))
(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

(load-if-exists (expand-file-name (concat user-emacs-directory "secrets.el")))

;; go to the beginning and the end of current buffer
(global-set-key (kbd "C-{") 'beginning-of-buffer)
(global-set-key (kbd "C-}") 'end-of-buffer)

;; temporary for my shitty broken keyboard
(global-set-key (kbd "<kp-delete>") (lambda () (interactive) (insert ".")))
(global-set-key (kbd "<kp-begin>") 'next-line)


;; set font
;; WARNING: You should have Fira Code font installed
;; on your system. change the font or delete the following
;; region if you dont want to deal with it
(set-face-attribute 'default nil
		    :family "Fira Code"
		    :weight 'regular
		    :height 110)

(set-fontset-font t 'arabic "Noto Sans Arabic")

;; keybindings emacs way
(global-unset-key "\C-l")
(defvar ctl-l-map (make-keymap)
  "Keymap for local bindings and functions, prefixed by (^L)")
(define-key global-map "\C-l" 'Control-L-prefix)
(fset 'Control-L-prefix ctl-l-map)


;; keybindings emacs way
(global-unset-key "\C-z")
(defvar ctl-z-map (make-keymap)
  "Keymap for local bindings and functions, prefixed by (^Z)")
(define-key global-map "\C-z" 'Control-Z-prefix)
(fset 'Control-Z-prefix ctl-z-map)

(define-key ctl-z-map "z"   'suspend-frame)
(define-key ctl-z-map "u"   'undo-redo)
(define-key ctl-z-map "r"   'undo)
(define-key ctl-z-map "m"   'mark-sexp)

(define-key ctl-z-map "ds"  'kill-symbol-at-point)
(define-key ctl-z-map "dw"  'kill-word-at-point)
(global-set-key (kbd "C-z C-d") 'kill-whole-line)


(global-unset-key "\C-\\")
(defvar ctl-backslash-map (make-keymap)
  "Keymap for local bindings and functions, prefixed by \ (backslash)")
(define-key global-map "\C-\\" 'Control-Backslash-prefix)
(fset 'Control-Backslash-prefix ctl-backslash-map)

;; Common keybindings
(define-key ctl-l-map "l"   'recenter-top-bottom)
(define-key ctl-l-map "g"   'goto-line)
(define-key ctl-l-map "R"   'replace-regexp)
(define-key ctl-l-map "Q"   'query-replace-regexp)
(define-key ctl-l-map "T"   'delete-trailing-whitespace)
(define-key ctl-l-map "k"   'kill-current-buffer)
(define-key ctl-l-map "fr"  'fill-region)
(define-key ctl-l-map "ee"  'async-shell-command)
(define-key ctl-l-map "er"  'shell-command-on-region)
(define-key ctl-l-map "sq"  'select-text-in-quote)


(define-key ctl-backslash-map "m" 'man)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External Packages (use-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; add themes directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; load the theme
;; (load-theme 'less t)
;; (load-theme 'modus-vivendi t)
;; (load-theme 'leyl t)

;; ensure you have doom-themes package
;; (use-package doom-themes :ensure t)
(load-theme 'doom-flatwhite t)

(use-package diminish :ensure t)


;; Languages
(use-package markdown-mode :ensure t)
(use-package js2-mode :ensure t)
(use-package lua-mode :ensure t)
(use-package rust-mode :ensure t)
(use-package go-mode :ensure t)
(use-package dockerfile-mode :ensure t)


(setq c-default-style "bsd")

(use-package c-ts-mode
  :ensure nil
  :init
  (add-hook 'c-ts-mode-hook #'hide-ifdef-mode)
  (add-hook 'c-ts-mode-hook
            (lambda ()
              (setq-local c-ts-mode-indent-style 'bsd)
              (setq-local c-ts-mode-indent-offset 4))))



(use-package dumb-jump
  :ensure t
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :config
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq dumb-jump-prefer-searcher 'ag)

  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g b" . dumb-jump-back)))

(use-package yasnippet
  :ensure t
  :diminish t
  ;; :defer t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))  ;; personal snippets
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (diminish 'yas-minor-mode)
  (defun yasnippet-snippets--fixed-indent ()
    "Set `yas-indent-line' to `fixed'."
    (set (make-local-variable 'yas-indent-line) 'fixed)))

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
  (global-undo-tree-mode 1)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :diminish
  :defer t
  :init

  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(define-key ctl-backslash-map "dl" 'dired-find-file)
(define-key ctl-backslash-map "dh" 'dired-up-directory)
(define-key ctl-backslash-map "dd" 'dired-do-delete)
(define-key ctl-backslash-map "dM" 'dired-do-chmod)
(define-key ctl-backslash-map "dt" 'dired-do-touch)
(define-key ctl-backslash-map "dr" 'dired-do-rename)

(global-set-key (kbd "C-,") 'duplicate-line)

;; easier way to navigate
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; emacs isearch stuff
(global-set-key (kbd "M-s s") 'isearch-forward)
(global-set-key (kbd "M-s r") 'isearch-query-replace)
(global-set-key (kbd "M-s R") 'isearch-query-replace-regexp)


;; configure modeline
(use-package moody
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (setq moody-mode-line-height 18)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil))

;; Example configuration for Consult
(use-package consult
  :ensure t
  :bind (
	 ;; C-x bindings (ctl-x-map)
	 ("C-x b" . consult-buffer)		   ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x p b" . consult-project-buffer)	   ;; orig. project-switch-to-buffer
	 ;; M-g bindings (goto-map)
	 ("M-g g" . consult-goto-line)		   ;; orig. goto-line
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings (search-map)
	 ("M-s g" . consult-grep)
	 ("C-s" . consult-line)
	 ("M-s L" . consult-line-multi))
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(define-key ctl-backslash-map "tw"  'whitespace-mode)
(define-key ctl-backslash-map "tt"  'consult-theme)

(use-package diff-hl
  :ensure t
  :defer
  :init
  (global-diff-hl-mode))

(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-get-current-branch))

(define-key ctl-backslash-map "gs"  'magit-status)
(define-key ctl-backslash-map "gd"  'magit-diff-unstaged)
(define-key ctl-backslash-map "glc" 'magit-log-current)
(define-key ctl-backslash-map "glf" 'magit-log-buffer-file)
(define-key ctl-backslash-map "gb"  'magit-branch)

(use-package flycheck
  :ensure t
  :defer t
  :hook (lsp-mode . flycheck-mode))

(define-key ctl-backslash-map "ae"  'global-flycheck-mode)
(define-key ctl-backslash-map "aE"  'list-flycheck-errors)



(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package company
  :ensure t
  :diminish
  :init

  (global-company-mode)
  (define-key ctl-z-map "n" 'company-complete))

(use-package gptel
  :ensure t
  :defer
  :init

  (setq gptel-backend (gptel-make-gemini "Gemini"
                   :key (my-auth 'gemini-key)
                   :stream t))

  (gptel-make-ollama "Ollama"             ;Any name of your choosing
    :host "localhost:11434"               ;Where it's running
    :stream t                             ;Stream responses
    :models '(gemma3:1b qwen2.5:0.5b deepseek-r1:1.5b))          ;List of models

  (define-key ctl-backslash-map (kbd "RET")  'gptel-send))


(use-package pyvenv :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package parrot
  :ensure t
  :defer
  :config
  (setq parrot-rotate-dict
	'(
          (:rot ("yes" "no") :caps t :upcase t)
          (:rot ("on" "off") :caps t :upcase t)
          (:rot ("get" "set") :caps t :upcase t)

          (:rot ("true" "false") :caps t :upcase t)
          (:rot ("&&" "||"))
          (:rot ("==" "!="))
	  (:rot ("&" "|"))
          (:rot (">" "<" "=" ">=" "<="))

          (:rot ("." "->"))
          (:rot ("let" "const" "var"))
          (:rot ("if" "else" "else if" "elif"))
          (:rot ("ifdef" "ifndef"))

          (:rot ("int8_t" "int16_t" "int32_t" "int64_t"))
          (:rot ("uint8_t" "uint16_t" "uint32_t" "uint64_t"))

	  (:rot ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
          (:rot ("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th"))))

  (define-key ctl-l-map "p" 'parrot-rotate-next-word-at-point)
  (define-key ctl-l-map "n" 'parrot-rotate-prev-word-at-point))

(define-key ctl-backslash-map "ld" 'xref-find-definitions)
(define-key ctl-backslash-map "lr" 'xref-find-references)
(define-key ctl-backslash-map "ls" 'consult-imenu)

;; comments
(define-key ctl-backslash-map "c " 'comment-line)
(define-key ctl-backslash-map "cb" 'comment-box)
(define-key ctl-backslash-map "ca" 'comment-dwim)


;; Highlight "#if 0" as comments
(use-package hideif
  :ensure nil
  :hook ((c-mode c++-mode c-ts-mode) . hide-ifdef-mode)
  :config
  (when (eq system-type 'gnu/linux)
    (add-to-list 'hide-ifdef-env '(__linux__ . 1))
    (add-to-list 'hide-ifdef-env '(__GNUC__ . 11)))
  (when (eq system-type 'darwin)
    (add-to-list 'hide-ifdef-env '(__APPLE__ . 1))
    (add-to-list 'hide-ifdef-env '(__clang__ . 1))
    (add-to-list 'hide-ifdef-env '(__llvm__ . 1)))
  :custom
  ;; Use hide-ifdefs/show-ifdefs manually.
  (hide-ifdef-initially nil)
  (hide-ifdef-shadow t))


;;; EMACS-SOLO-HIGHLIGHT-KEYWORDS-MODE
;;
;;  Highlights a list of words like TODO, FIXME...
;;  Code borrowed from `alternateved'
;;
(use-package emacs-solo-highlight-keywords-mode
  :ensure nil
  :no-require t
  :defer t
  :init
  (defcustom +highlight-keywords-faces
    '(("TODO" . error)
      ("FIXME" . error)
      ("BUG" . error)
      ("HACK" . warning)
      ("WONTFIX" . warning)
      ("NOTE" . compilation-info)
      ("HERE" . compilation-info)
      ("REFERENCE" . compilation-info))
    "Alist of keywords to highlight and their face."
    :group '+highlight-keywords
    :type '(alist :key-type (string :tag "Keyword")
                  :value-type (symbol :tag "Face"))
    :set (lambda (sym val)
           (dolist (face (mapcar #'cdr val))
             (unless (facep face)
               (error "Invalid face: %s" face)))
           (set-default sym val)))

  (defvar +highlight-keywords--keywords
    (when +highlight-keywords-faces
      (let ((keywords (mapcar #'car +highlight-keywords-faces)))
        `((,(regexp-opt keywords 'words)
           (0 (when (nth 8 (syntax-ppss))
                (cdr (assoc (match-string 0) +highlight-keywords-faces)))
              prepend)))))
    "Keywords and corresponding faces for `emacs-solo/highlight-keywords-mode'.")

  (defun emacs-solo/highlight-keywords-mode-on ()
    (font-lock-add-keywords nil +highlight-keywords--keywords t)
    (font-lock-flush))

  (defun emacs-solo/highlight-keywords-mode-off ()
    (font-lock-remove-keywords nil +highlight-keywords--keywords)
    (font-lock-flush))

  (define-minor-mode emacs-solo/highlight-keywords-mode
    "Highlight TODO and similar keywords in comments and strings."
    :lighter " +HL"
    :group '+highlight-keywords
    (if emacs-solo/highlight-keywords-mode
        (emacs-solo/highlight-keywords-mode-on)
      (emacs-solo/highlight-keywords-mode-off)))

  :hook
  (prog-mode . (lambda () (run-at-time "1 sec" nil #'emacs-solo/highlight-keywords-mode-on))))

;; ;; Whitespace style
(setq whitespace-style '(face
			 tabs
			 spaces
			 trailing
			 space-mark
			 ;; newline
			 indentation
			 tab-mark))

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

(auto-fill-mode)

;; TRAMP
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)


(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))
