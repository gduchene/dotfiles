;; General Configuration

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

(when (or window-system (daemonp))
  (my/with-add-hook 'after-init-hook
    (when my/day-theme (load-theme my/day-theme :no-confirm :no-enable))
    (when my/night-theme (load-theme my/night-theme :no-confirm :no-enable))
    (run-at-time nil (* 15 60) #'my/maybe-switch-theme my/day-theme
                 my/night-theme #'my/macos-dark-p)))


;; “Window” Management

(keymap-global-set "C-c l" #'my/resize-frame)

(dolist (k '(("C-s-<left>" . "C-c l i") ("C-s-<right>" . "C-c l o")))
  (if (keymap-global-lookup (car k))
      (message "`%s' is already bound, skipping." (car k))
    (keymap-global-set (car k) (cdr k))))
