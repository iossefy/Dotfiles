;; -*- lexical-binding: t -*-

;; Org Mode
(defun usf/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  (diminish org-indent-mode)
  (load-theme 'doom-solarized-dark t)
  ;; Improve org mode look;; s
  (setq org-startup-indented t
		org-pretty-entities t
		org-hide-emphasis-markers t
		org-startup-with-inline-images t
		org-image-actual-width '(300)
		org-hide-emphasis-markers t
		org-src-fontify-natively t
		org-fontify-quote-and-verse-blocks t
		org-src-tab-acts-natively t
		org-edit-src-content-indentation 2
		org-hide-block-startup nil
		org-src-preserve-indentation nil
		org-startup-folded 'content
		org-cycle-separator-lines 2))

(use-package org
  :defer t
  :hook (org-mode . usf/org-mode-setup)
  :config
  (setq rg-ellipsis " ▾"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))
