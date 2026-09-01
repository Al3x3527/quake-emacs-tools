;;; quake-maker.el -*- lexical-binding: t; -*-

;;(load-file "/home/alex/Projects/Project-E/quake.el")
(require 'quakec-mode)

(defun quake-output-buffer ()
  (get-buffer-create "*quake-output*"))

(defun quake-copy-progs ()
  "Copy Quake progs files."
  (interactive)

  (let ((target-dir
         (expand-file-name
          quake-moddirpath
          quake-gamedirpath)))

    (dolist (file quake-progslist)

      (let ((src (expand-file-name file quake-project))
            (dst (expand-file-name file target-dir)))

        (copy-file src dst t)

        (with-current-buffer (quake-output-buffer)
          (goto-char (point-max))
          (insert (format "%s copied\n" file)))))))

(defun quake-run-engine ()
  "Run Quake engine."
  (interactive)

  (let* ((default-directory quake-gamedirpath)
         (buffer (quake-output-buffer))
         (exe (expand-file-name
               quake-engine
               quake-gamedirpath)))

    (apply #'start-process
           "quake-engine"
           buffer
           exe
           quake-launch-args)

    (pop-to-buffer buffer)));

;;; test game
(defun quake-test ()
  "update progs and run Quake engine."
  (interactive)
  (with-current-buffer (quake-output-buffer)
    (erase-buffer))
  (quake-copy-progs)
  (quake-run-engine)
  (pop-to-buffer "*quake-output*" '((display-buffer-at-bottom))))
(provide 'quake-test)

;;; quake-maker.el ends here
