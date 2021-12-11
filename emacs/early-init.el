;; Helper Loading Functions

(defun my/load-file (filename)
  "Load FILENAME if it exists within the dotfiles directory."
  (load (concat (file-truename user-emacs-directory) "../"
                (substitute-in-file-name filename)) :no-error))

(defun my/load-file-variations (filename)
  "Load variations of FILENAME that may exist in other dotfiles
directories."
  (dolist (elt (mapcar (lambda (elt) (format "dotfiles-%s/%s" elt filename))
                       '("${UNAME}" "${DOMAIN}" "${HOST}")))
    (my/load-file elt)))

(my/load-file "emacs/functions")
(my/load-file-variations "emacs/functions")


;; Early Customization

(setq default-frame-alist '((height . 40) (left . 0.5)
                            (top . 0.3) (width . 160))
      gc-cons-threshold (* 50 1024 1024))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


;; Further Early Customization

(my/load-file-variations "emacs/early-init")
