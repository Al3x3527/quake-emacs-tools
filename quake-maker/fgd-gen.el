;;; fgd-gen.el --- Generate .fgd from program .dat files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Alex
;; Version: 0.0
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;;
;;; Commentary:
;;
;; Extracts @fgd blocks from program .dat files referenced by progs.src,
;; writes a .fgd (using the sole .fmf in the active dir or parent dir),
;; then improves class syntax by moving top-matter from bodies into headers,
;; normalizing whitespace, indenting bodies, and ensuring balanced brackets.
;;
;;; Code:

(require 'cl-lib)

;; -------------------------------------------------------------------
;; low-level helpers
;; -------------------------------------------------------------------

(defun fgd-gen--read-file-as-bytes (file)
  "Return FILE contents as a unibyte string (literal bytes)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun fgd-gen--write-text-file (file text)
  "Write TEXT to FILE using a multibyte buffer (normal text file)."
  (with-temp-file file
    (set-buffer-multibyte t)
    (insert text)))

(defun fgd-gen--escaped-p (string pos)
  "Return t if character at POS in STRING is escaped by odd number of backslashes."
  (let ((i (1- pos))
        (count 0))
    (while (and (>= i 0) (eq (aref string i) ?\\))
      (setq count (1+ count))
      (setq i (1- i)))
    (= (mod count 2) 1)))

(defun fgd-gen--find-first-unquoted (str char start)
  "Return index of CHAR in STR starting at START, ignoring text in double quotes.
Return nil if no unquoted CHAR is found."
  (cl-block find
    (let ((len (length str))
          (i start)
          (in-string nil))
      (while (< i len)
        (let ((c (aref str i)))
          (cond
           ((= c ?\")        ;; toggle string flag
            (setq in-string (not in-string)))

           ((and (not in-string)
                 (= c char))
            (cl-return-from find i))))
        (setq i (1+ i)))
      nil)))  ;; default return value

;; -------------------------------------------------------------------
;; locate files (fmf / progs.src)
;; -------------------------------------------------------------------

(defun fgd-gen--find-fmf-file (active-file)
  "Return the single .fmf file found in ACTIVE-FILE's directory or its parent.
Signal an error if none found or if more than one is found."
  (let* ((dir (file-name-directory active-file))
         (parent (expand-file-name ".." dir))
         (candidates (append (directory-files dir t "\\.fmf\\'")
                             (directory-files parent t "\\.fmf\\'"))))
    (setq candidates (cl-remove-if-not #'file-regular-p candidates))
    (cond
     ((null candidates) (error "No .fmf file found in %s or %s" dir parent))
     ((> (length candidates) 1) (error "Multiple .fmf files found; expected one: %S" candidates))
     (t (car candidates)))))

(defun fgd-gen--find-progs-src (active-file)
  "Return path to progs.src.
1) Look in active-file's directory.
2) If not found, search one level down (non-recursively) for a single progs.src.
Signal an error if none or multiple candidates."
  (let* ((base-dir (file-name-directory active-file))
         (primary (expand-file-name "progs.src" base-dir)))
    (cond
     ((file-exists-p primary) primary)
     (t
      (let* ((subs (cl-remove-if-not
                    (lambda (d)
                      (and (file-directory-p d)
                           (not (member (file-name-nondirectory d) '("." "..")))))
                    (directory-files base-dir t)))
             (found
              (cl-loop for d in subs
                       for candidate = (expand-file-name "progs.src" d)
                       if (file-exists-p candidate)
                       collect candidate)))
        (cond
         ((null found) (error "progs.src not found in %s or its immediate subdirectories" base-dir))
         ((> (length found) 1) (error "Multiple progs.src found: %S" found))
         (t (car found))))))))

;; -------------------------------------------------------------------
;; progs.src parsing
;; -------------------------------------------------------------------

(defun fgd-gen--detect-progs-src-type (file)
  "Detect progs.src FILE type.

Return a cons (TYPE . PAYLOAD):
- TYPE 2: PAYLOAD is list of sourcefile names discovered by `#pragma sourcefile`.
- TYPE 1: PAYLOAD is the first-line path string."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((first-line (string-trim (or (thing-at-point 'line t) ""))))
      (if (string-match "^#pragma[ \t]+sourcefile[ \t]+\\(.+\\)$" first-line)
          (let (files)
            (goto-char (point-min))
            (while (re-search-forward "^#pragma[ \t]+sourcefile[ \t]+\\(.+\\)$" nil t)
              (push (match-string 1) files))
            (cons 2 (nreverse files)))
        (cons 1 first-line)))))

(defun fgd-gen--sourcefiles-to-program-files (type payload base-dir)
  "Convert progs.src TYPE/PAYLOAD to list of absolute program file paths (.dat).

- If TYPE is 2, PAYLOAD is list of .src files which themselves contain program file paths (one per line).
- If TYPE is 1, PAYLOAD is single program file path."
  (cl-case type
    (2
     (cl-loop for src in payload
              append
              (let ((src-path (expand-file-name src base-dir)))
                (unless (file-exists-p src-path)
                  (error "Sourcefile not found: %s" src-path))
                (with-temp-buffer
                  (insert-file-contents src-path)
                  (let (collected)
                    (goto-char (point-min))
                    (while (not (eobp))
                      (let ((line (string-trim (or (thing-at-point 'line t) ""))))
                        (forward-line 1)
                        (unless (or (string-empty-p line) (string-prefix-p "#" line))
                          (push (expand-file-name line (file-name-directory src-path)) collected))))
                    (nreverse collected))))))
    (1
     (let ((dat (expand-file-name payload base-dir)))
       (unless (file-exists-p dat)
         (error "Program file not found: %s" dat))
       (list dat)))))

;; -------------------------------------------------------------------
;; extract @fgd blocks from program .dat files
;; -------------------------------------------------------------------

(defun fgd-gen--extract-fgd-blocks (program-file)
  "Return a list of strings extracted from PROGRAM-FILE.

Each string corresponds to the data between an \"@fgd\" marker and the following NUL (\\0).
The function is defensive and always advances; if a block is incomplete (no NUL), it stops scanning that file."
  (let* ((raw (fgd-gen--read-file-as-bytes program-file))
         (len (length raw))
         (start 0)
         (results '()))
    (while (and start (< start len))
      (let ((pos (string-match "@fgd ?" raw start)))
        (if (not pos)
            (setq start nil)
          (let* ((after (match-end 0))
                 (nil-pos (string-match "\0" raw after)))
            (if (not nil-pos)
                ;; incomplete block, stop scanning this file
                (setq start nil)
              (push (substring raw after nil-pos) results)
              ;; make strict forward progress
              (setq start (max (1+ nil-pos) (1+ pos))))))))
    (nreverse results)))

;; -------------------------------------------------------------------
;; class processing: move top-matter, format body, fix brackets
;; -------------------------------------------------------------------

(defun fgd-gen--normalize-space (s)
  "Trim S and collapse runs of whitespace within to a single space."
  (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim s)))

(defun fgd-gen--find-matching-paren (text pos)
  "Return index of closing parenthesis matching opening at POS in TEXT.
POS should point at the opening '('. Returns nil if not found."
  (let ((depth 1)
        (i (1+ pos))
        (len (length text))
        (in-string nil))
    (while (and (< i len) (> depth 0))
      (let ((c (aref text i)))
        (cond
         ((= c ?\") (setq in-string (not in-string)))
         ((and (not in-string) (= c ?\()) (setq depth (1+ depth)))
         ((and (not in-string) (= c ?\))) (setq depth (1- depth)))))
      (setq i (1+ i)))
    (if (= depth 0) (1- i) nil)))

(defun fgd-gen--process-single-class (class-text)
  "Process CLASS-TEXT, normalize header, move top-matter from body to header,
indent body, and ensure exactly one pair of brackets."
  (let* ((wanted '("model" "base" "color" "size"))
         ;; find first unquoted '['
         (bracket-pos (fgd-gen--find-first-unquoted class-text ?\[ 0))
         (header (if bracket-pos
                     (string-trim-right (substring class-text 0 bracket-pos))
                   (string-trim-right class-text)))
         (body (if bracket-pos
                   (substring class-text (1+ bracket-pos))
                 "")))

    ;; collect existing header tokens
    (let ((header-has
           (cl-remove-if-not
            #'identity
            (cl-mapcar (lambda (name)
                         (when (string-match (concat "\\_<" (regexp-quote name) "\\_>\\s-*(") header)
                           name))
                       wanted)))
          (top-tokens '()))

      ;; extract first occurrences of wanted tokens from body
      (when (not (string-empty-p (string-trim body)))
        (with-temp-buffer
          (insert body)
          (goto-char (point-min))
          (while (re-search-forward
                  "\\_<\\(model\\|base\\|color\\|size\\)\\_>\\s-*\\((\\(?:.\\|\n\\)*?\\))"
                  nil t)
            (let* ((name (match-string 1))
                   (token (string-trim (match-string 0))))
              ;; only first occurrence per name and not in header
              (unless (or (member name header-has) (member name top-tokens))
                (push token top-tokens))
              ;; delete the matched token
              (delete-region (match-beginning 0) (match-end 0))
              (delete-horizontal-space)))
          (setq body (string-trim (buffer-string)))))

      (setq top-tokens (nreverse top-tokens))

      ;; insert tokens into header before first '='
      (let ((new-header header))
        (when top-tokens
          (let ((tokens-str (mapconcat #'identity top-tokens " "))
                (eq-pos (fgd-gen--find-first-unquoted new-header ?= 0)))
            (if eq-pos
                ;; insert before '='
                (setq new-header (concat (substring new-header 0 eq-pos)
                                         " "
                                         tokens-str
                                         " "
                                         (substring new-header eq-pos)))
              ;; append if no '='
              (setq new-header (concat new-header " " tokens-str)))))

        ;; normalize spaces in header but preserve newlines
        (setq new-header (replace-regexp-in-string "[ \t]+" " " new-header))
        (setq new-header (replace-regexp-in-string " \\(\n\\)" "\\1" new-header))
        (setq new-header (replace-regexp-in-string "\\(\n\\) " "\\1" new-header))

        ;; format body
        (let* ((body-lines (if (string-empty-p body)
                               nil
                             (split-string body "\n" t)))
               (indented-body (if body-lines
                                  (mapconcat (lambda (ln) (concat "  " (string-trim-right ln)))
                                             body-lines "\n")
                                "")))
          ;; assemble final class
          (concat
           new-header
           (if (string-empty-p indented-body)
               " [ ]"
             (concat "\n  [\n"
                     indented-body
                     "\n  ]"))))))))

(defun fgd-gen--improve-fgd-syntax (fgd-file)
  "Read FGD-FILE, process each class region, overwrite file, and return new content.

Processes each class independently, moves top-matter from body into header,
normalizes whitespace, indents body lines, and ensures balanced brackets."
  (let* ((text (with-temp-buffer (insert-file-contents fgd-file)
                 (buffer-string)))
         ;; match @ClassType at start of line (or after newline)
         (class-re "^[ \t]*@[ \t]*\\(BaseClass\\|PointClass\\|SolidClass\\)\\_>")
         (pos 0)
         (matches '()))
    ;; Collect start indices of all classes
    (while (string-match class-re text pos)
      (push (match-beginning 0) matches)
      ;; advance past this match to avoid overlap
      (setq pos (match-end 0)))
    (setq matches (nreverse matches))

    ;; Build regions: each region starts at a class header, ends at next header or eof
    (let ((regions '())
          (n (length matches)))
      (dotimes (i n)
        (let ((s (nth i matches))
              (e (if (< i (1- n))
                     (nth (1+ i) matches)
                   (length text))))
          (push (cons s e) regions)))
      (setq regions (nreverse regions))

      ;; Assemble output: preserve text between classes
      (let ((out "")
            (cursor 0))
        (dolist (reg regions)
          (let ((s (car reg)) (e (cdr reg)))
            ;; append any text between previous cursor and this class
            (when (< cursor s)
              (setq out (concat out (substring text cursor s))))
            ;; process this class
            (let ((class-text (substring text s e)))
              (setq out (concat out (fgd-gen--process-single-class class-text) "\n")))
            (setq cursor e)))
        ;; append any remaining text after last class
        (when (< cursor (length text))
          (setq out (concat out (substring text cursor))))
        ;; write back to file and return
        (fgd-gen--write-text-file fgd-file out)
        out))))


;; -------------------------------------------------------------------
;; Main interactive entry point
;; -------------------------------------------------------------------

;;;###autoload
(defun fgd-gen-generate ()
  "Generate a .fgd file from program files referenced by progs.src and improve its syntax.
Run this from a buffer visiting a file inside the project."
  (interactive)
  (unless buffer-file-name
    (user-error "Must run from a visiting file buffer"))
  (let* ((active-file buffer-file-name)
         (base-dir (file-name-directory active-file))
         (fmf (fgd-gen--find-fmf-file active-file))
         (fgd (concat (file-name-sans-extension fmf) ".fgd"))
         (progs-src (fgd-gen--find-progs-src active-file)))
    (pcase-let* ((`(,type . ,payload) (fgd-gen--detect-progs-src-type progs-src))
                 (program-files (fgd-gen--sourcefiles-to-program-files type payload base-dir)))
      (let (blocks)
        (dolist (pf program-files)
          (setq blocks (append blocks (fgd-gen--extract-fgd-blocks pf))))
        ;; write raw fgd (concatenate blocks without adding newlines)
        (fgd-gen--write-text-file fgd (mapconcat #'identity blocks ""))
        ;; improve syntax in-place
        (let ((new-content (fgd-gen--improve-fgd-syntax fgd)))
          (with-help-window "*FGD Output*"
            (with-current-buffer standard-output
              (insert new-content)
              (goto-char (point-min))))
          (message "FGD written and improved: %s" fgd))))))

(provide 'fgd-gen)
;;; fgd-gen.el ends here
