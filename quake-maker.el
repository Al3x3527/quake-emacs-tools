;;; lisp/quake-maker/quake-maker.el -*- lexical-binding: t; -*-

(defun quake-make()
"update progs.dat files."
  (interactive)
  (setq progslist '(
                    "~/GitHub/Project-E/qwprogs.dat"
                    "~/GitHub/Project-E/menu.dat"
                    "~/GitHub/Project-E/csprogs.dat"))
  (while progslist
      (copy-file (car progslist) "~/Quake/FTEQW/enoch/" t)
      (setq progslist (cdr progslist)))
  (with-help-window "qm-out"
  (with-current-buffer(get-buffer-create "qm-out")
    (print "progs copied.")
   (eshell-command "cd ~/Quake/FTEQW/ && ./fteqw64 +map test_biped > #<buffer qm-out>")
   (pop-to-buffer "qm-out"'((display-buffer-at-bottom)
                             (inhibit-same-window . t)
   ))
  )))
;; if this doesn't work, try opening an eshell buffer manually, then close it.
