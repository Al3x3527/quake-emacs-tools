;;; quake-maker.el -*- lexical-binding: t; -*-

;;TODO make sure we can load quake config
(load-file "/home/alex/GitHub/Project-E/quake.el")
(require 'quake)

(defun quake-copy-progs()
  "update progs.dat files."
  (interactive)

;;; copy mod files
  (let* (target-dir (expand-file-name quake-moddirpath quake-gamedirpath)))
  (dolist (file quake-progslist)
    (let ((src (expand-file-name file quake-project))
          (dst (expand-file-name file target-dir))
          (copy-file src dst t)
          (with-current-buffer "qm-out"
            (insert file " copied."))))))
(provide 'quake-copy-progs)

;;; run game
(defun quake-run-engine ()
  "run Quake engine from cmd line."
  (interactive)
    (let ((default-directory quake-gamedirpath))
      (eshell-command
       (concat "./" quake-engine " " quake-launch-args)
       "qm-out")))
; FIXME: eshell output inserted at point in current buffer
(provide 'quake-run-engine)

;;; test game
(defun quake-test ()
  "update progs and run Quake engine."
  (interactive)
  (with-current-buffer "qm-out"
    (erase-buffer))
  (quake-copy-progs)
  (quake-run-engine)
  (pop-to-buffer "qm-out" '((display-buffer-at-bottom))))
(provide 'quake-test)

;;; quake-maker.el ends here
