(setq mac-right-option-modifier 'none)

(let ((gnu-ls "/usr/local/bin/gls"))
  (when (file-exists-p gnu-ls)
    (setq insert-directory-program gnu-ls)))
