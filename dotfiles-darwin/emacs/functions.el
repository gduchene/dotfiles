;; -*- lexical-binding: t -*-

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
       (dolist (elem `(("j" . ((width . ,(/ 2.0 3))))
                       ("k" . ((left . 1.0) (width . ,(/ 2.0 3))))
                       ("l" . ((left . 0.5) (width . 0.5)))))
         (keymap-set map (car elem) (lambda ()
                                      (interactive)
                                      (my/modify-frame-parameters (cdr elem)))))
       map)
     t nil
     "Use %k to resize the frame")))
