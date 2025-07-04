;; -*- lexical-binding: t -*-

(defun my/macos-dark-p ()
  "Returns non-nil if macOS is currently in dark mode, nil otherwise."
  (interactive)
  (equal (process-lines-ignore-status "defaults" "read" "-g"
                                      "AppleInterfaceStyle")
         '("Dark")))

(defun my/modify-frame-parameters (parameters &optional frame)
  (when (frame-parameter frame 'fullscreen)
    (toggle-frame-maximized))
  (modify-frame-parameters frame (append parameters
                                         '((height . 1.0) (top . 1.0)
                                           (left . 0.0) (user-position t)))))

(defun my/resize-frame (&optional frame)
  "Interactively resize FRAME."
  (interactive)
  (let ((echo-keystrokes nil))
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (keymap-set map "f" #'toggle-frame-maximized)
       (dolist (elem `(("i" . ((width . 0.5)))
                       ("j" . ((width . ,(/ 2.0 3))))
                       ("k" . ((left . 1.0) (width . ,(/ 2.0 3))))
                       ("l" . ((left . 0.5) (width . 0.5)))
                       ("o" . ((left . 1.0) (width . 0.5)))
                       ("p" . ((left . 1.0) (width . ,(/ 1.0 3))))
                       ("u" . ((width . ,(/ 1.0 3))))))
         (keymap-set map (car elem) (lambda ()
                                      (interactive)
                                      (my/modify-frame-parameters (cdr elem)))))
       map)
     t nil
     "Use %k to resize the frame")))

(defun my/set-frame-themes ()
  "Configure themes, including theme switching."
  (let ((day-theme (seq-random-elt (alist-get 'day my/themes)))
        (night-theme (seq-random-elt (alist-get 'night my/themes))))
    (when day-theme
      (load-theme day-theme :no-confirm :no-enable))
    (when night-theme
      (load-theme night-theme :no-confirm :no-enable))
    (run-at-time nil (* 15 60) #'my/maybe-switch-theme day-theme night-theme
                 #'my/macos-dark-p)))
