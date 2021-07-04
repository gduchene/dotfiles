(setq mac-right-option-modifier 'none)

(when (executable-find "gls")
  (setq insert-directory-program "gls"))

(defun macos-dark-p ()
  (interactive)
  (string= (shell-command-to-string "defaults read -g AppleInterfaceStyle")
           "Dark\n"))

(add-hook 'after-init-hook
          (lambda ()
            (run-at-time nil (* 15 60) 'maybe-switch-theme awhk-day-theme
                         awhk-night-theme 'macos-dark-p)))
