;;; fgd-gen.el --- Generate .fgd from program .dat files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Alex
;; Version: 0.0
;; URL: https://github.com/Al3x3527/quake-emacs-tools/tree/main/quake-maker
;; Package-Requires: ((emacs "24.4"))
;;
;;; Commentary:
;;
;; Extracts @fgd blocks from QuakeC program .dat files based on progs.src.
;;
;;; Code:

(require 'cl-lib)

(defun fgd-gen--read-file-as-bytes (file)
  "Return FILE contents as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun fgd-gen--detect-progs-src-type (file)
  "Return (type . payload).

TYPE is 1 or 2.
If type 2, PAYLOAD is list of #pragma sourcefile filenames.
If type 1, PAYLOAD is the path from the first line."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((first-line (string-trim (or (thing-at-point 'line t) ""))))
      (cond
       ;; type 2 example: "#pragma sourcefile sv_progs.src"
       ((string-match "^#pragma[ \t]+sourcefile[ \t]+\\(.+\\)$" first-line)
        (let (files)
          ;; Scan for all sourcefile pragmas
          (goto-char (point-min))
          (while (re-search-forward "^#pragma[ \t]+sourcefile[ \t]+\\(.+\\)$" nil t)
            (push (match-string 1) files))
          (cons 2 (nreverse files))))
       ;; type 1 example: "../qwprogs.dat"
       (t
        (cons 1 first-line))))))

(defun fgd-gen--sourcefiles-to-program-files (type payload base-dir)
  "Convert TYPE/PAYLOAD from progs.src into list of program (.dat) file paths.

BASE-DIR is the directory of the active file."
  (cl-case type
    (2 ;; payload is list of .src files
     (cl-loop for src in payload append
              (let* ((src-path (expand-file-name src base-dir)))
                (unless (file-exists-p src-path)
                  (error "Sourcefile not found: %s" src-path))
                ;; Each .src file contains a list of program .dat file paths.
                (with-temp-buffer
                  (insert-file-contents src-path)
                  (cl-loop while (not (eobp))
                           for line = (string-trim (thing-at-point 'line t))
                           do (forward-line 1)
                           if (and (not (string-empty-p line))
                                   (not (string-prefix-p "#" line)))
                           collect (expand-file-name line (file-name-directory src-path)))))))
    (1 ;; payload is a single .dat file path
     (let ((dat (expand-file-name payload base-dir)))
       (unless (file-exists-p dat)
         (error "Program file not found: %s" dat))
       (list dat)))))

(defun fgd-gen--extract-fgd-blocks (program-file)
  "Return list of fgd strings extracted from PROGRAM-FILE.

Searches for literal \"@fgd\" followed by a space (optional) and
copies text up to the next null byte ^@ (0)."
  (let* ((raw (fgd-gen--read-file-as-bytes program-file))
         (len (length raw))
         (start 0)
         results)
    (while (and start (< start len))
      (setq start (string-match "@fgd ?" raw start))
      (when start
        (let* ((after (match-end 0))
               (nil-pos (string-match "\0" raw after)))
          (when nil-pos
            (push (substring raw after nil-pos) results)
            (setq start (1+ nil-pos))))))
    (nreverse results)))

(defun fgd-gen--locate-fmf (active-file)
  "Return the .fmf path associated with ACTIVE-FILE.

Searches ACTIVE-FILE's directory, else ../<name>.fmf."
  (let* ((dir (file-name-directory active-file))
         (name (file-name-base active-file))
         (same-dir (expand-file-name (concat name ".fmf") dir))
         (up-dir (expand-file-name (concat "../" name ".fmf") dir)))
    (cond
     ((file-exists-p same-dir) same-dir)
     ((file-exists-p up-dir) up-dir)
     (t (error "No .fmf file found for %s" name)))))

;;;###autoload
(defun fgd-gen-generate ()
  "Generate <name>.fgd from the .dat files referenced by progs.src."
  (interactive)
  (unless buffer-file-name
    (error "This command must be run from a visiting file buffer"))

  (let* ((active-file buffer-file-name)
         (base-dir (file-name-directory active-file))
         ;; Locate the .fmf file
         (fmf (fgd-gen--locate-fmf active-file))
         (fgd (concat (file-name-sans-extension fmf) ".fgd"))
         ;; Find progs.src in same directory
         (progs-src (expand-file-name "progs.src" base-dir)))
    (unless (file-exists-p progs-src)
      (error "progs.src not found in %s" base-dir))

    ;; Determine syntax type (1 or 2)
    (pcase-let* ((`(,type . ,payload) (fgd-gen--detect-progs-src-type progs-src))
                 (program-files (fgd-gen--sourcefiles-to-program-files type payload base-dir)))

      ;; Extract fgd fragments from all program .dat files
      (let (blocks)
        (dolist (pf program-files)
          (setq blocks (append blocks (fgd-gen--extract-fgd-blocks pf))))

        ;; Write output
        (with-temp-file fgd
          (set-buffer-multibyte t)
          (dolist (blk blocks)
            (insert blk)))           ; Insert raw without newlines.

        ;; Display result in help window
        (with-help-window "*FGD Output*"
          (with-current-buffer standard-output
            (insert-file-contents fgd)
            (goto-char (point-min))))
        (message "FGD written to: %s" fgd)))))

(provide 'fgd-gen)
;;; fgd-gen.el ends here
