;; Path Configuration  -*- lexical-binding: t -*-

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
        package-gnupghome-dir (expand-file-name "gnupg" package-user-dir)
        recenter-positions '(3 middle top bottom)
        require-final-newline t
        ring-bell-function #'ignore
        sentence-end-double-space nil
        show-paren-delay 0
        uniquify-buffer-name-style 'post-forward-angle-brackets
        vc-follow-symlinks nil
        version-control t)

(setq-default indent-tabs-mode nil)

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

(keymap-global-set "C-c DEL" #'bury-buffer)
(keymap-global-set "C-c j" #'join-line)
(keymap-global-set "C-c q" #'electric-quote-local-mode)

(use-package marginalia :config (marginalia-mode 1) :ensure t)


;; Buffer Management

(use-package ibuffer
  :config
  (defun my/ibuffer-hooks ()
    (ibuffer-switch-to-saved-filter-groups "default"))
  :hook (ibuffer-mode . my/ibuffer-hooks)
  :bind ("C-x C-b" . ibuffer)
  :custom
  ((ibuffer-saved-filter-groups
    `(("default"
       ("Emacs" (or (mode . completion-list-mode)
                    (mode . debugger-mode)
                    (mode . help-mode)
                    (mode . messages-buffer-mode)
                    (name . "^\\*scratch\\*$")))
       ("dotfiles" (filename . ,(concat my/dotfiles-directory ".*")))
       ("Magit" (name . "^magit"))
       ("Dired" (mode . dired-mode))
       ("Shells" (mode . term-mode))
       ("Manuals" (or (mode . Info-mode)
                      (mode . Man-mode))))))
   (ibuffer-show-empty-filter-groups nil)))


;; Project Management

(use-package project
  :custom (project-list-file (my/cache-file-name "projects"))
  :defer t)


;; Search and Completion Management

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-c s" . my/consult-ripgrep))
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-styles '(orderless basic)))

(use-package vertico :config (vertico-mode 1) :ensure t)

(setq completion-ignore-case t
      completion-in-region-function #'consult-completion-in-region)

(setopt read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)

(dolist (fn '(isearch-forward isearch-backward))
  (advice-add fn :after #'my/isearch-region))


;; Theme Management

(defconst my/fonts
  '((:family "Iosevka Fixed Slab" :height 140)
    (:family "Iosevka SS04" :height 140)
    (:family "Iosevka" :height 140)
    (:family "Menlo" :height 120))
  "Fonts to try as default.")

(defconst my/themes
  `((day . (adwaita
            leuven
            modus-operandi
            modus-operandi-tinted
            tango
            ,nil))
    (night . (tango-dark
              misterioso
              modus-vivendi
              modus-vivendi-tinted
              wombat)))
  "Themes to try.")

(let ((hook (cond ((daemonp) 'server-after-make-frame-hook)
                  (window-system 'after-init-hook))))
  (when hook (add-hook hook #'my/set-frame-font)))


;; “Window” Management

(keymap-global-set "C-c f" #'toggle-frame-maximized)

(windmove-default-keybindings 'super)
(windmove-swap-states-default-keybindings)

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
  :config
  (defun my/dired-hooks ()
    (setq-local mouse-1-click-follows-link (- mouse-1-click-follows-link)))
  (put 'dired-find-alternate-file 'disabled nil)
  :bind
  (:map dired-mode-map
        ("b" . dired-up-directory)
        ("<mouse-1>" . dired-find-file))
  :hook (dired-mode . my/dired-hooks)
  :custom (dired-listing-switches (concat dired-listing-switches " -h")))


;; Dockerfile

(use-package dockerfile-mode :defer t :ensure t)


;; Eglot

(use-package eglot
  :defer t
  :custom
  ((eglot-autoshutdown t)
   (eglot-extend-to-xref t)
   (eglot-ignored-server-capabilities '(:inlayHintProvider))))


;; Eshell

(use-package esh-mode
  :config
  (defun my/eshell-clear ()
    (interactive)
    (if eshell-foreground-command
        (eshell/clear-scrollback)
      (let ((input (eshell-get-old-input)))
        (eshell/clear-scrollback)
        (eshell-emit-prompt)
        (insert input))))
  :bind (("C-c e" . eshell)
         :map eshell-mode-map
         ("s-l" . my/eshell-clear))
  :custom ((eshell-cmpl-ignore-case t)
           (eshell-aliases-file (expand-file-name "alias" user-emacs-directory))
           (eshell-directory-name (my/cache-file-name "eshell"))
           (eshell-scroll-show-maximum-output nil)))


;; Flymake

(use-package flymake
  :bind (:map flymake-mode-map
              ("s-[" . flymake-goto-prev-error)
              ("s-]" . flymake-goto-next-error)))


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
         (go-mode . my/go-add-hooks))

  :ensure t)


;; Git

(use-package git-commit
  :custom (git-commit-summary-max-length 50)
  :hook ((git-commit-setup . electric-quote-local-mode)))

(use-package jinx :ensure t :hook git-commit-setup)

(use-package magit :bind ("C-c k" . magit-status) :ensure t)


;; Org

(use-package org
  :custom
  ((org-hide-emphasis-markers t)
   (org-startup-folded 'showall))
  :defer t)

(use-package org-roam
  :config (org-roam-db-autosync-enable)
  :bind ("C-c n f" . org-roam-node-find)
  :custom
  ((org-id-locations-file (my/data-file-name "org-id-locations"))
   (org-roam-db-location (my/data-file-name "org-roam.db")))
  :ensure t)


;; Rust

(use-package rust-mode
  :hook (rust-mode . eglot-ensure)
  :ensure t)


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


;; Tramp

(use-package tramp-cache
  :after tramp
  :custom (tramp-persistency-file-name (my/data-file-name "tramp")))


;; Further Customization

(my/load-file-variations "emacs/init")
