;; Path Functions

(require 'xdg)

(defun my/cache-file-name (name)
  (expand-file-name name (concat (xdg-cache-home) "/emacs")))

(defun my/data-file-name (name)
  (expand-file-name name (concat (xdg-data-home) "/emacs")))


;; Search Helpers

(defun my/isearch-region (&rest _args)
  "Pull the contents of the region into the search string if it
is active."
  (when (use-region-p)
    (deactivate-mark)
    (isearch-yank-string (buffer-substring-no-properties
                          (region-beginning) (region-end)))))


;; Theme Functions

(defun my/macos-dark-p ()
  "Returns non-nil if macOS is currently in dark mode, nil otherwise."
  (interactive)
  (string= (shell-command-to-string "defaults read -g AppleInterfaceStyle")
           "Dark\n"))

(defun my/maybe-switch-theme (light-theme dark-theme enable-dark-theme-p)
  "Switch between themes.

If ENABLE-DARK-THEME-P returns a non-nil value, then DARK-THEME
is enabled and LIGHT-THEME is disabled, unless DARK-THEME is
already enabled, undefined, or nil. The opposite happens if
ENABLE-DARK-THEME-P returns nil, unless if LIGHT-THEME is nil, in
which case only DARK-THEME is disabled, and nothing else is
enabled."
  (if (funcall enable-dark-theme-p)
      (when (and (custom-theme-p dark-theme)
                 (not (custom-theme-enabled-p dark-theme)))
        (enable-theme dark-theme)
        (disable-theme light-theme))
    (if (and (not light-theme) (custom-theme-enabled-p dark-theme))
        (disable-theme dark-theme)
      (when (and (custom-theme-p light-theme)
                 (not (custom-theme-enabled-p light-theme)))
        (enable-theme light-theme)
        (disable-theme dark-theme)))))


;; “Window Management” Functions

(defun my/center-frame (&optional frame)
  "Center FRAME."
  (interactive)
  (modify-frame-parameters frame '((left . 0.5) (top . 0.5))))

(defun my/disable-frame-modes ()
  "Disable select frame modes where appropriate."
  (dolist (mode '(menu-bar-mode scroll-bar-mode tool-bar-mode))
    (when (and (fboundp mode) (symbol-value mode)) (funcall mode -1))))

(defun my/focus-frame (&optional frame)
  "Focus FRAME."
  (interactive)
  (delete-other-windows)
  (modify-frame-parameters frame '((height . 1.0) (width . 0.5)
                                   (left . 0.5) (top . 1.0)
                                   (user-position . t))))


;; Misc. Stuff

(defun my/display-startup-time ()
  "Displays a message saying how long it took Emacs to load."
  (message "Emacs loaded in %.2f seconds with %d garbage collections done."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(defun my/eglot-organize-imports ()
  "Interactively call ‘eglot-code-action-organize-imports’."
  (interactive)
  (call-interactively #'eglot-code-action-organize-imports))

(defmacro my/with-add-hook (hook &rest body)
  (declare (indent 1) (debug t))
  `(add-hook ,hook #'(lambda () ,@body)))
