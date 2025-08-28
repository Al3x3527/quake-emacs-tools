;;; lisp/quake-maker/quake-maker.el -*- lexical-binding: t; -*-

(defun quake-make()
"update progs.dat files."
  (interactive)
  (copy-file "~/GitHub/Project-E/qwprogs.dat" "~/Quake/FTEQW/enoch/" t)
  (with-help-window "qm-out"
  (with-current-buffer(get-buffer-create "qm-out")
   (eshell-command "cd ~/Quake/FTEQW/ && ./fteqw64 > #<buffer qm-out>")
   (pop-to-buffer "qm-out"'((display-buffer-at-bottom)
                             (inhibit-same-window . t)
   ))
  )))
