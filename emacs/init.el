;; Path Configuration

(setopt abbrev-file-name (my/cache-file-name "abbrev_defs")
        auto-save-list-file-prefix (my/cache-file-name "auto-save-list/")
        backup-directory-alist `(("." . ,(my/cache-file-name "backups")))
        bookmark-default-file (my/data-file-name "bookmarks")
        custom-file (my/cache-file-name "custom.el")
        delete-by-moving-to-trash t
        transient-history-file (my/cache-file-name "transient/history.el"))


;; General Configuration

(setopt backup-by-copying t
        confirm-kill-emacs #'y-or-n-p
        delete-old-versions t
        display-time-24hr-format t
        fill-column 72
        inhibit-startup-screen t
        recenter-positions '(3 middle top bottom)
        require-final-newline t
        ring-bell-function #'ignore
        sentence-end-double-space nil
        show-paren-delay 0
        uniquify-buffer-name-style 'post-forward-angle-brackets
        vc-follow-symlinks nil
        version-control t)

(setq-default indent-tabs-mode nil)

(add-to-list 'completion-styles 'flex)

(put 'list-timers 'disabled nil)

(add-hook 'after-init-hook #'my/display-startup-time)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(auto-save-mode -1)
(auto-save-visited-mode 1)
(blink-cursor-mode -1)
(column-number-mode 1)
(display-time-mode (if (getenv "TMUX") -1 1))
(electric-indent-mode -1)
(global-auto-revert-mode 1)

(keymap-global-set "C-c j" #'join-line)
(keymap-global-set "C-c q" #'electric-quote-local-mode)

(use-package marginalia :config (marginalia-mode 1) :ensure t)


;; Buffer Management

(setopt ibuffer-saved-filter-groups
        '(("default"
           ("Emacs" (or (mode . completion-list-mode)
                        (mode . debugger-mode)
                        (mode . help-mode)
                        (mode . messages-buffer-mode)
                        (name . "^\\*scratch\\*$")))
           ("Magit" (name . "^magit"))
           ("Dired" (mode . dired-mode))
           ("Shells" (mode . term-mode))
           ("Manuals" (or (mode . Info-mode)
                          (mode . Man-mode)))))
        ibuffer-show-empty-filter-groups nil)

(my/with-add-hook 'ibuffer-mode-hook
  (ibuffer-switch-to-saved-filter-groups "default"))

(keymap-global-set "C-x C-b" #'ibuffer)


;; Search and Completion Management

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-c s" . consult-ripgrep))
  :ensure t)

(use-package vertico :config (vertico-mode 1) :ensure t)

(setq completion-ignore-case t
      completion-in-region-function #'consult-completion-in-region)

(dolist (fn '(isearch-forward isearch-backward))
  (advice-add fn :after #'my/isearch-region))


;; Theme Management

(defvar my/day-theme nil "Theme to use during the day.")

(defvar my/night-theme nil "Theme to use during the night.")


;; “Window” Management

(keymap-global-set "C-TAB" #'other-window)
(keymap-global-set "C-M-TAB" #'other-frame)
(keymap-global-set "C-c f" #'toggle-frame-maximized)

(windmove-default-keybindings 'super)

(dolist (fn '(delete-window split-window-horizontally split-window-vertically))
  (advice-add fn :after #'(lambda (&rest _args) (balance-windows))))

(advice-add #'fill-paragraph :around
            #'(lambda (orig &rest args)
                (let* ((offset (save-excursion
                                 (back-to-indentation)
                                 (current-column)))
                       (fill-column (+ offset fill-column)))
                  (apply orig args))))

(if (daemonp) (add-hook 'before-make-frame-hook #'my/disable-frame-modes)
  (my/disable-frame-modes))


;; Bazel

(use-package bazel
  :custom (bazel-buildifier-before-save t)
  :defer t
  :ensure t)


;; C++

(use-package cc-mode
  :config
  (defun my/c++-hooks ()
    (setopt flycheck-clang-language-standard "c++20"))

  :hook
  ((c-mode-common . google-set-c-style)
   (c++-mode . flycheck-mode)
   (c++-mode . my/c++-hooks))

  :bind (:map c-mode-base-map ("C-c d" . clang-format))
  :custom (clang-format-style "google"))

(use-package clang-format :defer t :ensure t)
(use-package flycheck :defer t :ensure t)
(use-package google-c-style :defer t :ensure t)


;; Dired

(use-package dired
  :bind (:map dired-mode-map ("b" . dired-up-directory))
  :defer t
  :custom (dired-listing-switches (concat dired-listing-switches " -h")))

(put 'dired-find-alternate-file 'disabled nil)

(my/with-add-hook 'dired-mode-hook
  (setq-local mouse-1-click-follows-link (- mouse-1-click-follows-link))
  (keymap-local-set "<mouse-1>" #'dired-find-file))


;; Go

(use-package go-mode
  :custom ((gofmt-args '("-local" "go.awhk.org"))
           (gofmt-command "goimports")
           (tab-width 2))

  :config
  (defun my/go-add-hooks ()
    (add-hook 'before-save-hook #'my/eglot-organize-imports nil :local)
    (add-hook 'before-save-hook #'eglot-format-buffer nil :local))

  :hook ((go-mode . eglot-ensure)
         (go-mode . my/go-add-hooks)))


;; Git

(use-package git-commit
  :custom (git-commit-summary-max-length 50)
  :hook ((git-commit-setup . electric-quote-local-mode)))

(use-package jinx :ensure t :hook git-commit-setup)

(use-package magit :bind ("C-c k" . magit-status) :ensure t)


;; Org

(use-package org :custom (org-startup-folded 'showall) :defer t)


;; Shell Scripts

(use-package sh-script
  :custom (sh-basic-offset 2)
  :magic ("#compdef .+" . sh-mode)
  :mode ("/PKGBUILD\\'" . sh-mode))


;; Swift

(use-package eglot
  :config (add-to-list 'eglot-server-programs
                       '(swift-mode . ("xcrun" "sourcekit-lsp")))
  :defer t)

(use-package swift-mode
  :init
  (defun my/swift-format ()
    "Format Swift code."
    (interactive)
    (save-buffer)
    (shell-command (format "swift format -i %s" (buffer-file-name))))
  :hook (swift-mode . eglot-ensure)
  :bind (:map swift-mode-map ("C-c d" . my/swift-format))
  :ensure t)


;; systemd

(use-package conf-mode
  :mode ("\\.service\\'" "\\.socket\\'" "\\.timer\\'"))


;; Further Customization

(my/load-file-variations "emacs/init")
