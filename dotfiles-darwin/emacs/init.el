(setq mac-right-option-modifier 'none)

(when (executable-find "gls")
  (setq insert-directory-program "gls"))

(defun is-macos-dark ()
  (interactive)
  (string= (shell-command-to-string "defaults read -g AppleInterfaceStyle")
           "Dark\n"))

(run-at-time nil (* 15 60) 'maybe-switch-theme 'doom-one-light 'doom-one
             'is-macos-dark)
