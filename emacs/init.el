;; Path Configuration

(setq abbrev-file-name (my/cache-file-name "abbrev-def")
      auto-save-list-file-prefix (my/cache-file-name "auto-save/")
      backup-directory-alist `(("." . ,(my/cache-file-name "backups")))
      custom-file (my/cache-file-name "custom.el")
      transient-history-file (my/cache-file-name "transient/history.el"))


;; General Configuration

(setq-default indent-tabs-mode nil
              fill-column 72)

(setq backup-by-copying t
      confirm-kill-emacs #'y-or-n-p
      display-time-24hr-format t
      inhibit-startup-screen t
      recenter-positions '(3 middle top bottom)
      require-final-newline t
      ring-bell-function #'ignore
      sentence-end-double-space nil
      show-paren-delay 0
      uniquify-buffer-name-style 'post-forward-angle-brackets
      vc-follow-symlinks nil)

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

(global-set-key (kbd "C-c q") #'electric-quote-local-mode)

(use-package marginalia :config (marginalia-mode 1) :ensure t)


;; Buffer Management

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
      ibuffer-show-empty-filter-groups nil)

(my/with-add-hook 'ibuffer-mode-hook
  (ibuffer-switch-to-saved-filter-groups "default"))

(global-set-key (kbd "C-x C-b") #'ibuffer)


;; Search and Completion Management

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-c s" . consult-ripgrep)))

(use-package vertico :config (vertico-mode 1) :ensure t)

(setq completion-ignore-case t
      completion-in-region-function #'consult-completion-in-region)

(dolist (fn '(isearch-forward isearch-backward))
  (advice-add fn :after #'my/isearch-region))


;; Theme Management

(defvar my/day-theme nil "Theme to use during the day.")

(defvar my/night-theme nil "Theme to use during the night.")


;; “Window” Management

(global-set-key (kbd "<C-tab>") #'other-window)
(global-set-key (kbd "<C-M-tab>") #'other-frame)
(global-set-key (kbd "C-c l") #'my/focus-frame)
(global-set-key (kbd "C-c f") #'toggle-frame-maximized)

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

(setq clang-format-style "google")

(my/with-add-hook 'c-initialization-hook
  (local-set-key (kbd "C-c d") #'clang-format))
(add-hook 'c-mode-common-hook #'google-set-c-style)
(add-hook 'c++-mode-hook #'flycheck-mode)


;; Dired

(put 'dired-find-alternate-file 'disabled nil)

(my/with-add-hook 'dired-mode-hook
  (setq-local mouse-1-click-follows-link (- mouse-1-click-follows-link))
  (local-set-key [mouse-1] #'dired-find-file))


;; Go

(use-package go-mode
  :custom ((gofmt-args '("-local" "go.awhk.org"))
           (gofmt-command "goimports")
           (tab-width 2))

  :config
  (defun my/go-add-hooks ()
    (add-hook 'before-save-hook #'my/eglot-organize-imports nil :local)
    (add-hook 'before-save-hook #'eglot-format-buffer nil :local))

  :defer t
  :hook ((go-mode . eglot-ensure)
         (go-mode . my/go-add-hooks)))


;; Flycheck

(setq flycheck-clang-language-standard "c++20")


;; Git

(use-package git-commit
  :custom (git-commit-summary-max-length 50)
  :hook ((git-commit-setup . electric-quote-local-mode)
         (git-commit-setup . git-commit-turn-on-flyspell)))

(use-package magit :bind ("C-c k" . magit-status) :ensure t)


;; Org

(use-package org :custom (org-startup-folded 'showall) :defer t)


;; Shell Scripts

(use-package sh-script
  :custom (sh-basic-offset 2)
  :magic ("#compdef .+" . sh-mode)
  :mode ("/PKGBUILD\\'" . sh-mode))


;; systemd

(use-package conf-mode
  :mode ("\\.service\\'" "\\.socket\\'" "\\.timer\\'"))


;; Further Customization

(my/load-file-variations "emacs/init")
