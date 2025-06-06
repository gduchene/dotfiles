;; General Configuration  -*- lexical-binding: t -*-

(setopt frame-resize-pixelwise t
        mac-right-command-modifier 'meta
        mac-right-option-modifier 'none)

(when (executable-find "gls")
  (setq insert-directory-program "gls"))


;; Org

(use-package org-roam
  :defer t
  :custom (org-roam-directory "~/Documents/ZK/"))


;; Server

(use-package server
  :config
  (defun my/server-start ()
    (when (and (display-graphic-p) (not (server-running-p)))
      (server-start)))
  :hook (after-init . my/server-start))


;; Theme Management

(add-hook 'after-init-hook #'my/set-frame-themes)


;; “Window” Management

(keymap-global-set "C-c l" #'my/resize-frame)

(dolist (k '(("C-s-<left>" . "C-c l i") ("C-s-<right>" . "C-c l o")))
  (if (keymap-global-lookup (car k))
      (message "`%s' is already bound, skipping." (car k))
    (keymap-global-set (car k) (cdr k))))
