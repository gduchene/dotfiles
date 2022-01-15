;; Path Configuration

(setq abbrev-file-name (my/cache-file-name "abbrev-def")
      auto-save-list-file-prefix (my/cache-file-name "auto-save/")
      backup-directory-alist `(("." . ,(my/cache-file-name "backups")))
      custom-file (my/cache-file-name "custom.el")
      transient-history-file (my/cache-file-name "transient/history.el"))


;; Package Management

(setq package-archives '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
                         ("MELPA"    . "https://melpa.org/packages/"))
      package-user-dir (my/data-file-name "elpa"))

(my/load-file "emacs/package-list")
(my/load-file-variations "emacs/package-list")

(unless (file-directory-p package-user-dir)
  (package-refresh-contents)
  (package-install-selected-packages))

(package-initialize)


;; General Configuration

(setq-default indent-tabs-mode nil
              fill-column 72)

(setq backup-by-copying t
      display-time-24hr-format t
      inhibit-startup-screen t
      recenter-positions '(3 middle top bottom)
      require-final-newline t
      ring-bell-function 'ignore
      sentence-end-double-space nil
      show-paren-delay 0
      uniquify-buffer-name-style 'post-forward-angle-brackets
      vc-follow-symlinks nil)

(put 'list-timers 'disabled nil)

(add-hook 'after-init-hook #'my/display-startup-time)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(my/with-add-hook 'server-switch-hook
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(auto-save-mode -1)
(auto-save-visited-mode 1)
(blink-cursor-mode -1)
(column-number-mode 1)
(display-time-mode (if (getenv "TMUX") -1 1))
(electric-indent-mode -1)
(global-auto-revert-mode 1)
(show-paren-mode 1)

(global-set-key (kbd "C-c q") #'electric-quote-local-mode)


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


;; Ivy

(ivy-mode 1)
(diminish 'ivy-mode)

(counsel-mode 1)
(diminish 'counsel-mode)

(global-set-key (kbd "C-c s") #'swiper)
(global-set-key (kbd "C-c x") #'swiper-all)


;; Theme Management

(defvar my/day-theme nil "Theme to use during the day.")

(defvar my/night-theme nil "Theme to use during the night.")

(when (or window-system (daemonp))
  (require 'doom-themes)

  (setq my/day-theme 'doom-one-light
        my/night-theme 'doom-one
        doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (add-to-list 'doom-themes-base-faces
               '(nobreak-space :inherit 'default :underline builtin)
               :append))


;; “Window” Management

(global-set-key (kbd "<C-tab>") #'other-window)
(global-set-key (kbd "<C-M-tab>") #'other-frame)
(global-set-key (kbd "C-c l") #'my/focus-frame)
(global-set-key (kbd "C-c f") #'toggle-frame-maximized)

(dolist (fn '(delete-window split-window-horizontally split-window-vertically))
  (advice-add fn :after #'(lambda (&rest _args) (balance-windows))))


;; Bazel

(setq bazel-buildifier-before-save t)


;; C++

(setq clang-format-style "google")

(my/with-add-hook 'c-initialization-hook
  (local-set-key (kbd "C-c f") #'clang-format))
(add-hook 'c-mode-common-hook #'google-set-c-style)
(add-hook 'c++-mode-hook #'flycheck-mode)


;; Dired

(put 'dired-find-alternate-file 'disabled nil)

(my/with-add-hook 'dired-mode-hook
  (setq-local mouse-1-click-follows-link (- mouse-1-click-follows-link)))

(define-key dired-mode-map [mouse-1] #'dired-find-file)


;; Go

(when (executable-find "goimports")
  (setq gofmt-args '("-local" "go.awhk.org")
        gofmt-command "goimports"))

(my/with-add-hook 'go-mode-hook
  (setq tab-width 2)
  (add-hook 'before-save-hook #'gofmt-before-save nil :local)
  (flycheck-mode))


;; Flycheck

(setq flycheck-clang-language-standard "c++17")


;; Git

(setq git-commit-summary-max-length 50)

(add-hook 'git-commit-setup-hook #'electric-quote-local-mode)
(add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)

(global-set-key (kbd "C-c k") #'magit-status)


;; Org

(setq org-startup-folded "showall")


;; Shell Scripts

(setq sh-basic-offset 2)

(push '("/PKGBUILD\\'" . sh-mode) auto-mode-alist)
(push '("#compdef .+" . sh-mode) magic-mode-alist)


;; systemd

(dolist (regexp '("\\.service\\'" "\\.socket\\'" "\\.timer\\'"))
  (push `(,regexp . conf-mode) auto-mode-alist))


;; Further Customization

(my/load-file-variations "emacs/init")
