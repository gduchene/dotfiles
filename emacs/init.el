(let ((cache-dir (substitute-in-file-name "${XDG_CACHE_HOME}/emacs/"))
      (data-dir (substitute-in-file-name "${XDG_DATA_HOME}/emacs/")))
  (setq abbrev-file-name (concat cache-dir "abbrev-def")
        auto-save-list-file-prefix (concat cache-dir "auto-save/")
        backup-directory-alist `(("." . ,(concat cache-dir "backups")))
        custom-file (concat cache-dir "custom.el")
        ido-save-directory-list-file (concat cache-dir "ido.last")
        package-user-dir (concat data-dir "elpa")))

(package-initialize)

(setq-default indent-tabs-mode nil
              fill-column 72)
(setq backup-by-copying t
      inhibit-startup-screen t
      recenter-positions '(3 middle top bottom)
      require-final-newline t
      show-paren-delay 0
      uniquify-buffer-name-style 'post-forward-angle-brackets
      vc-follow-symlinks nil)

(auto-save-mode -1)
(auto-save-visited-mode 1)
(column-number-mode 1)
(display-time-mode 1)
(electric-indent-mode -1)
(evil-mode 1)
(global-auto-revert-mode 1)
(ido-mode 1)
(menu-bar-mode -1)
(show-paren-mode 1)
(tool-bar-mode -1)
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-mode))
(add-to-list 'ido-ignore-files ".DS_Store")
(add-to-list 'magic-mode-alist '("#compdef .+" . sh-mode))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-M-tab>") 'other-frame)
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
(add-hook 'c++-mode-hook (lambda () (google-set-c-style)))
(add-hook 'go-mode-hook
          (lambda ()
            (setq-local tab-width 2)
            (add-hook 'before-save-hook 'gofmt-before-save nil t)))
(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-switch-to-saved-filter-groups "default")))
(add-hook 'sh-mode-hook (lambda () (setq-local sh-basic-offset 2)))
