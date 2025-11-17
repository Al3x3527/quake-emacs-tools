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

;;; ---------------------------------------------------------------------------
;;;  New FMF lookup logic
;;; ---------------------------------------------------------------------------

(defun fgd-gen--find-fmf-file (active-file)
  "Return the single .fmf file found in active dir or one dir up.
Error if zero or more than one found."
  (let* ((dir (file-name-directory active-file))
         (up (expand-file-name ".." dir))
         (candidates (append (directory-files dir t "\\.fmf\\'")
                             (directory-files up t "\\.fmf\\'"))))
    (setq candidates (cl-delete-if-not #'file-regular-p candidates))
    (cond
     ((null candidates)
      (error "No .fmf file found in %s or %s" dir up))
     ((> (length candidates) 1)
      (error "Multiple .fmf files found; expected only one: %S" candidates))
     (t (car candidates)))))

;;; ---------------------------------------------------------------------------
;;;  New progs.src fallback logic
;;; ---------------------------------------------------------------------------

(defun fgd-gen--find-progs-src (active-file)
  "Return the path to progs.src.
1. Look in active directory.
2. If not found, search one directory down (non-recursively)."
  (let* ((base-dir (file-name-directory active-file))
         (primary (expand-file-name "progs.src" base-dir)))
    (cond
     ((file-exists-p primary)
      primary)
     (t
      ;; Search subdirectories one level down
      (let* ((subs (cl-remove-if-not #'file-directory-p
                                     (directory-files base-dir t "^[^.]" t)))
             (found
              (cl-loop for d in subs
                       for candidate = (expand-file-name "progs.src" d)
                       if (file-exists-p candidate)
                       collect candidate)))
        (cond
         ((null found)
          (error "progs.src not found in %s or its immediate subdirectories"
                 base-dir))
         ((> (length found) 1)
          (error "Multiple progs.src found: %S" found))
         (t (car found))))))))

;;; ---------------------------------------------------------------------------
;;;  progs.src type detection
;;; ---------------------------------------------------------------------------

(defun fgd-gen--detect-progs-src-type (file)
  "Return (type . payload). TYPE = 1 or 2.

Type 2: `#pragma sourcefile FILE.src` lines.
Type 1: first line is a direct program file path."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((first-line (string-trim (or (thing-at-point 'line t) ""))))
      (cond
       ((string-match "^#pragma[ \t]+sourcefile[ \t]+\\(.+\\)$" first-line)
        (let (files)
          (goto-char (point-min))
          (while (re-search-forward
                  "^#pragma[ \t]+sourcefile[ \t]+\\(.+\\)$" nil t)
            (push (match-string 1) files))
          (cons 2 (nreverse files))))
       (t
        (cons 1 first-line))))))

(defun fgd-gen--sourcefiles-to-program-files (type payload base-dir)
  "Convert TYPE/PAYLOAD from progs.src into absolute .dat paths."
  (cl-case type
    (2
     (cl-loop for src in payload append
              (let* ((src-path (expand-file-name src base-dir)))
                (unless (file-exists-p src-path)
                  (error "Sourcefile not found: %s" src-path))
                (with-temp-buffer
                  (insert-file-contents src-path)
                  (cl-loop while (not (eobp))
                           for line = (string-trim (thing-at-point 'line t))
                           do (forward-line 1)
                           if (and (not (string-empty-p line))
                                   (not (string-prefix-p "#" line)))
                           collect (expand-file-name line (file-name-directory src-path)))))))
    (1
     (let ((dat (expand-file-name payload base-dir)))
       (unless (file-exists-p dat)
         (error "Program file not found: %s" dat))
       (list dat)))))

;;; ---------------------------------------------------------------------------
;;;  Hardened FGD block extraction (no infinite loops)
;;; ---------------------------------------------------------------------------

(defun fgd-gen--extract-fgd-blocks (program-file)
  "Return list of @fgd blocks from PROGRAM-FILE.

Searches for '@fgd' with optional space, extracts until the next
null byte. Loop is designed to always terminate even if the file
is malformed or missing null bytes."
  (let* ((raw (fgd-gen--read-file-as-bytes program-file))
         (len (length raw))
         (start 0)
         results)
    (while (and start (< start len))
      ;; Look for @fgd
      (let ((pos (string-match "@fgd ?" raw start)))
        (if (not pos)
            (setq start nil) ;; stop looping
          (let* ((after (match-end 0))
                 (nil-pos (string-match "\0" raw after)))

            ;; If there is no null byte after '@fgd', stop scanning.
            ;; (Cannot extract incomplete block.)
            (if (not nil-pos)
                (setq start nil)
              ;; Extract block
              (push (substring raw after nil-pos) results)
              ;; Ensure strict forward progress
              (setq start (max (1+ nil-pos) (1+ pos))))))))
    (nreverse results)))

;;; ---------------------------------------------------------------------------
;;;  Main interactive entry point
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun fgd-gen-generate ()
  "Generate a .fgd file from all @fgd blocks in referenced .dat files."
  (interactive)
  (unless buffer-file-name
    (error "Must run from a visiting file buffer"))

  (let* ((active-file buffer-file-name)
         (base-dir (file-name-directory active-file))
         (fmf (fgd-gen--find-fmf-file active-file))
         (fgd (concat (file-name-sans-extension fmf) ".fgd"))
         (progs-src (fgd-gen--find-progs-src active-file)))
    (pcase-let* ((`(,type . ,payload)
                   (fgd-gen--detect-progs-src-type progs-src))
                 (program-files
                  (fgd-gen--sourcefiles-to-program-files type payload base-dir)))
      ;; Gather all @fgd blocks
      (let (blocks)
        (dolist (pf program-files)
          (setq blocks (append blocks (fgd-gen--extract-fgd-blocks pf))))

        ;; Write .fgd
        (with-temp-file fgd
          (set-buffer-multibyte t)
          (dolist (blk blocks)
            (insert blk)))

        ;; Show output
        (with-help-window "*FGD Output*"
          (with-current-buffer standard-output
            (insert-file-contents fgd)
            (goto-char (point-min))))

        (message "FGD written to: %s" fgd)))))

(provide 'fgd-gen)
;;; fgd-gen.el ends here
