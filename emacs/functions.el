;; Path Functions

(require 'xdg)

(defun my/cache-file-name (name)
  (expand-file-name name (concat (xdg-cache-home) "/emacs")))

(defun my/data-file-name (name)
  (expand-file-name name (concat (xdg-data-home) "/emacs")))


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
already enabled. The opposite happens if ENABLE-DARK-THEME-P
returns nil."
  (if (funcall enable-dark-theme-p)
      (unless (member dark-theme custom-enabled-themes)
        (load-theme dark-theme :no-confirm)
        (disable-theme light-theme))
    (unless (member light-theme custom-enabled-themes)
      (load-theme light-theme :no-confirm)
      (disable-theme dark-theme))))


;; “Window Management” Functions

(defun my/center-frame (&optional frame)
  "Center FRAME."
  (interactive)
  (modify-frame-parameters frame '((left . 0.5) (top . 0.5))))

(defun my/focus-frame (&optional frame)
  "Focus FRAME."
  (interactive)
  (delete-other-windows)
  (modify-frame-parameters frame '((fullscreen . fullheight)
                                   (left . 0.5)
                                   (width . 100))))


;; Misc. Stuff

(defun my/display-startup-time ()
  "Displays a message saying how long it took Emacs to load."
  (message "Emacs loaded in %.2f seconds with %d garbage collections done."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(defmacro my/with-add-hook (hook &rest body)
  (declare (indent 1) (debug t))
  `(add-hook ,hook #'(lambda () ,@body)))
