(defun my/source-if-exists (filename)
  "Load FILENAME if it exists within the dotfiles hierarchy."
  (load (concat (file-truename user-emacs-directory) "../"
                (substitute-in-file-name filename)) :no-error))

(setq default-frame-alist '((height . 40) (left . 0.5) (top . 0.2) (width . 160))
      gc-cons-threshold (* 50 1024 1024))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(tool-bar-mode -1)

(my/source-if-exists "dotfiles-${UNAME}/emacs/early-init")
(my/source-if-exists "dotfiles-${DOMAIN}/emacs/early-init")
(my/source-if-exists "dotfiles-${HOST}/emacs/early-init")
