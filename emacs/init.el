(let ((cache-dir (substitute-in-file-name "${XDG_CACHE_HOME}/emacs/"))
      (data-dir (substitute-in-file-name "${XDG_DATA_HOME}/emacs/")))
  (setq abbrev-file-name (concat cache-dir "abbrev-def")
        auto-save-list-file-prefix (concat cache-dir "auto-save/")
        backup-directory-alist `(("." . ,(concat cache-dir "backups")))
        custom-file (concat cache-dir "custom.el")
        package-user-dir (concat data-dir "elpa")
        transient-history-file (concat cache-dir "transient/history.el")))

(defun source-if-exists (filename)
  (load (concat (file-truename user-emacs-directory) "../"
                (substitute-in-file-name filename)) t))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq-default indent-tabs-mode nil
              fill-column 72)
(setq backup-by-copying t
      display-time-24hr-format t
      inhibit-startup-screen t
      recenter-positions '(3 middle top bottom)
      require-final-newline t
      sentence-end-double-space nil
      show-paren-delay 0
      uniquify-buffer-name-style 'post-forward-angle-brackets
      vc-follow-symlinks nil)

(auto-save-mode -1)
(auto-save-visited-mode 1)
(column-number-mode 1)
(display-time-mode 1)
(electric-indent-mode -1)
(global-auto-revert-mode 1)
(menu-bar-mode -1)
(show-paren-mode 1)
(tool-bar-mode -1)
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("/PKGBUILD\\'" . shell-script-mode))
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(left . 0.5))
(add-to-list 'default-frame-alist '(top . 0.5))
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'magic-mode-alist '("#compdef .+" . sh-mode))

(when (display-graphic-p)
  (blink-cursor-mode -1)
  (scroll-bar-mode -1))

(defvar awhk-day-theme nil "Theme to use during the day.")

(defvar awhk-night-theme nil "Theme to use during the night.")

(use-package bazel
  :config (setq bazel-buildifier-before-save t)
  :ensure t)

(use-package cc-mode :bind (:map c++-mode-map ("C-c f" . clang-format)))

(use-package clang-format
  :init (setq clang-format-style "google")
  :ensure t)

(use-package diminish :ensure t)

(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (require 'dired-x))

(use-package doom-themes
  :config
  (setq awhk-day-theme 'doom-one-light
        awhk-night-theme 'doom-one
        doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (add-to-list 'doom-themes-base-faces
               '(nobreak-space :inherit 'default :underline builtin)
               t)
  (load-theme 'doom-one 'no-confirm 'no-enable)
  (load-theme 'doom-one-light 'no-confirm 'no-enable)
  :ensure t)

(use-package exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
  (exec-path-from-shell-initialize)
  :ensure t)

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config (evil-mode)
  :ensure t)

(use-package evil-collection
  :config
  (evil-collection-init '(dired ibuffer magit))
  (diminish 'evil-collection-unimpaired-mode)
  :after (evil diminish)
  :ensure t)

(use-package flycheck
  :config
  (setq flycheck-global-modes '(c++-mode go-mode))
  (global-flycheck-mode)
  :hook (c++-mode . (lambda () (setq flycheck-clang-language-standard "c++17")))
  :after go-mode
  :ensure t)

(use-package go-mode
  :init
  (when (executable-find "goimports")
    (setq gofmt-args '("-local" "go.awhk.org")
          gofmt-command "goimports"))
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 2)
              (add-hook 'before-save-hook 'gofmt-before-save nil t)))
  :ensure t)

(use-package google-c-style
  :hook (c++-mode . google-set-c-style)
  :ensure t)

(use-package ibuffer
  :init
  (defvar awhk-ibuffer-filters "default" "Default Ibuffer filter group.")
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups awhk-ibuffer-filters)))
  :config
  (setq ibuffer-saved-filter-groups
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
        ibuffer-show-empty-filter-groups nil))

(use-package ivy
  :config (ivy-mode 1)
  :bind (:map ivy-minibuffer-map ("C-w" . ivy-backward-delete-char))
  :demand t
  :diminish
  :ensure t)

(use-package magit
  :init (setq git-commit-summary-max-length 50)
  :bind (("C-c k" . magit-status))
  :ensure t)

(use-package org :init (setq org-startup-folded "showall"))

(use-package sh-script :config (setq sh-basic-offset 2))

(use-package term
  :bind
  (("C-c t" . (lambda ()
                (interactive)
                (set-buffer (make-term "terminal" shell-file-name))
                (term-mode)
                (term-char-mode)
                (switch-to-buffer "*terminal*")
                (evil-emacs-state)))))

(use-package timer-list :config (put 'list-timers 'disabled nil))

(defun center-frame (&optional frame)
  "Center FRAME."
  (interactive)
  (modify-frame-parameters frame '((left . 0.5) (top . 0.5))))

(defun focus-frame (&optional frame)
  "Focus FRAME."
  (interactive)
  (delete-other-windows)
  (modify-frame-parameters frame '((fullscreen . fullheight)
                                   (left . 0.5)
                                   (width . 100))))

(defun maybe-switch-theme (light-theme dark-theme enable-dark-theme-p)
  "Switch between themes.

If ENABLE-DARK-THEME-P returns a non-nil value, then DARK-THEME
is enabled and LIGHT-THEME is disabled, unless DARK-THEME is
already enabled. The opposite happens if ENABLE-DARK-THEME-P
returns nil."
  (if (funcall enable-dark-theme-p)
      (unless (member dark-theme custom-enabled-themes)
        (enable-theme dark-theme)
        (disable-theme light-theme))
    (unless (member light-theme custom-enabled-themes)
      (enable-theme light-theme)
      (disable-theme dark-theme))))

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-M-tab>") 'other-frame)
(global-set-key (kbd "C-c l") 'focus-frame)
(global-set-key (kbd "C-c s") 'toggle-frame-maximized)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x 0")
                (lambda ()
                  (interactive)
                  (delete-window)
                  (balance-windows)))
(global-set-key (kbd "C-x 2")
                (lambda ()
                  (interactive)
                  (split-window-vertically)
                  (balance-windows)))
(global-set-key (kbd "C-x 3")
                (lambda ()
                  (interactive)
                  (split-window-horizontally)
                  (balance-windows)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'server-switch-hook
          (lambda ()
            (menu-bar-mode -1)
            (scroll-bar-mode -1)))

(source-if-exists "dotfiles-${UNAME}/emacs/init.el")
(source-if-exists "dotfiles-${DOMAIN}/emacs/init.el")
(source-if-exists "dotfiles-${HOST}/emacs/init.el")
