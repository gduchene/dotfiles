(defconst my/dotfiles-directory
  (file-truename (concat user-emacs-directory "../"))
  "Directory containing all configuration files.")


;; Helper Loading Functions

(defun my/load-file (filename)
  "Load FILENAME if it exists within the dotfiles directory."
  (load (concat my/dotfiles-directory
                (substitute-in-file-name filename)) :no-error))

(defun my/load-file-variations (filename)
  "Load variations of FILENAME that may exist in other dotfiles
directories."
  (dolist (elt '("${UNAME}" "${DOMAIN}" "${HOST_SHORT}" "${HOST}" "local"))
    (my/load-file (format "dotfiles-%s/%s" elt filename))))

(my/load-file "emacs/functions")
(my/load-file-variations "emacs/functions")


;; Early Customization

(setopt default-frame-alist '((height . 40) (left . 0.5)
                              (top . 0.3) (width . 160))
        gc-cons-threshold (* 50 1024 1024)
        package-archives '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
                           ("MELPA"    . "https://melpa.org/packages/"))
        package-user-dir (my/data-file-name "elpa")
        treesit-extra-load-path `(,(my/data-file-name "tree-sitter")))


;; Further Early Customization

(my/load-file-variations "emacs/early-init")
