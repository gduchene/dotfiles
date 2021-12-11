;; General Configuration

(setq mac-right-command-modifier 'meta
      mac-right-option-modifier 'none)

(when (executable-find "gls")
  (setq insert-directory-program "gls"))


;; Theme Management

(when window-system
  (my/with-add-hook 'after-init-hook
    (run-at-time nil (* 15 60) #'my/maybe-switch-theme my/day-theme
                 my/night-theme #'my/macos-dark-p)))
