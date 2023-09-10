;; General Configuration

(setopt mac-right-command-modifier 'meta
        mac-right-option-modifier 'none)

(when (executable-find "gls")
  (setq insert-directory-program "gls"))


;; Theme Management

(when (or window-system (daemonp))
  (my/with-add-hook 'after-init-hook
    (when my/day-theme (load-theme my/day-theme :no-confirm :no-enable))
    (when my/night-theme (load-theme my/night-theme :no-confirm :no-enable))
    (run-at-time nil (* 15 60) #'my/maybe-switch-theme my/day-theme
                 my/night-theme #'my/macos-dark-p)))
