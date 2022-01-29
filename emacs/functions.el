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

(defun my/swiper (&optional swiper-all)
  "Call ‘swiper’ (‘swiper-all’ when prefixed) with the contents
of the region if it is active."
  (interactive "P")
  (if (use-region-p)
      (let ((initial-input (buffer-substring-no-properties
                            (region-beginning) (region-end))))
        (deactivate-mark)
        (if swiper-all (swiper-all initial-input) (swiper initial-input)))
    (if swiper-all (swiper-all) (swiper))))


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

(defun my/disable-frame-modes ()
  "Disable select frame modes where appropriate."
  (dolist (mode '(menu-bar-mode scroll-bar-mode tool-bar-mode))
    (when (and (fboundp mode) (symbol-value mode)) (funcall mode -1))))

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
