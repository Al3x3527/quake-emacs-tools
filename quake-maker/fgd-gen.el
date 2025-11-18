;;; fgd-gen.el --- Generate .fgd from program .dat files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Alex
;; Version: 0.1
;; URL: https://github.com/Al3x3527/quake-emacs-tools/tree/main/quake-maker
;; Package-Requires: ((emacs "24.4"))
;;
;;; Commentary:
;;
;; Extracts @fgd blocks from QuakeC program .dat files based on progs.src,
;; then improves class syntax by moving top-matter from class bodies into
;; headers, normalizing whitespace, and fixing missing closing brackets.
;;
;;; Code:

(require 'cl-lib)

;; -------------------------------------------------------------------
;; low-level helpers
;; -------------------------------------------------------------------

(defun fgd-gen--read-file-as-bytes (file)
  "Return FILE contents as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun fgd-gen--write-text-file (file text)
  "Write TEXT to FILE (multibyte buffer)."
  (with-temp-file file
    (set-buffer-multibyte t)
    (insert text)))

;; return t if char at POS in STRING is escaped by an odd number of backslashes
(defun fgd-gen--escaped-p (string pos)
  "Return t if character at POS in STRING is preceded by odd number of backslashes."
  (let ((count 0)
        (i (1- pos)))
    (while (and (>= i 0) (eq (aref string i) ?\\))
      (cl-incf count)
      (cl-decf i))
    (oddp count)))

;; find first occurrence of CHAR (character) in STRING at or after START that is not inside double quotes.
(defun fgd-gen--find-first-unquoted (string char &optional start)
  "Find index of CHAR in STRING at or after START that is not inside quotes.
Returns nil if none found."
  (let ((len (length string))
        (i (or start 0))
        (in-quote nil))
    (while (and (< i len) (not (and (not in-quote) (eq (aref string i) char))))
      (let ((c (aref string i)))
        (cond
         ((eq c ?\") (unless (fgd-gen--escaped-p string i)
                        (setq in-quote (not in-quote))))
         (t nil)))
      (setq i (1+ i)))
    (when (and (< i len) (not in-quote) (eq (aref string i) char))
      i)))

;; -------------------------------------------------------------------
;; locate files (fmf / progs.src)
;; -------------------------------------------------------------------

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
      ;; Search subdirectories one level down (non-recursive)
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
         ((null found)
          (error "progs.src not found in %s or its immediate subdirectories" base-dir))
         ((> (length found) 1)
          (error "Multiple progs.src found: %S" found))
         (t (car found))))))))

;; -------------------------------------------------------------------
;; progs.src parsing
;; -------------------------------------------------------------------

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
          (while (re-search-forward "^#pragma[ \t]+sourcefile[ \t]+\\(.+\\)$" nil t)
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

;; -------------------------------------------------------------------
;; extract @fgd blocks from program .dat files
;; -------------------------------------------------------------------

(defun fgd-gen--extract-fgd-blocks (program-file)
  "Return list of fgd strings extracted from PROGRAM-FILE.

Searches for literal \"@fgd\" followed by optional space, extracts until the next
null byte ^@ (0). The loop is robust to malformed files and always progresses."
  (let* ((raw (fgd-gen--read-file-as-bytes program-file))
         (len (length raw))
         (start 0)
         results)
    (while (and start (< start len))
      (let ((pos (string-match "@fgd ?" raw start)))
        (if (not pos)
            (setq start nil)
          (let* ((after (match-end 0))
                 (nil-pos (string-match "\0" raw after)))
            (if (not nil-pos)
                ;; incomplete block: stop scanning further in this file
                (setq start nil)
              (push (substring raw after nil-pos) results)
              ;; ensure forward progress
              (setq start (max (1+ nil-pos) (1+ pos))))))))
    (nreverse results)))

;; -------------------------------------------------------------------
;; FGD syntax improvement: move top matter into header, indent, fix brackets
;; -------------------------------------------------------------------

(defun fgd-gen--normalize-whitespace-token (token)
  "Return TOKEN trimmed with single space normalization."
  (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim token)))

(defun fgd-gen--process-single-class (class-text)
  "Process CLASS-TEXT and return transformed class text.

Moves first occurrences of model(...), base(...), color(...), size(...)
from the body into the header (if header doesn't already have them),
preserving multi-line header, normalizing spacing, indenting body,
and ensuring closing bracket exists."
  (let* ((orig class-text)
         (len (length class-text))
         ;; find first unquoted '[' to determine body start (if any)
         (bracket-pos (fgd-gen--find-first-unquoted class-text ?\[ 0))
         (header-end-pos (if bracket-pos bracket-pos len)) ; header is up to bracket or whole class
         (header (string-trim-right (substring class-text 0 header-end-pos)))
         (body (if bracket-pos
                   (substring class-text (1+ bracket-pos) (length class-text)) ; after '[' to end of class region
                 "" ))
         (top-tokens '())
         (wanted '("model" "base" "color" "size")))

    ;; ---------------------------
    ;; Find top-matter already present in header
    ;; ---------------------------
    (let (header-has)
      (dolist (name wanted)
        (when (string-match (format "\\_<%s\\_>\\s-*(" name) header)
          (push name header-has)))
      (setq header-has (nreverse header-has))
      ;; We'll not re-add these names
      )

    ;; ---------------------------
    ;; Extract tokens from body (first occurrences only)
    ;; operate in a temp buffer so we can delete safely
    ;; ---------------------------
    (when (> (length body) 0)
      (with-temp-buffer
        (insert body)
        (goto-char (point-min))
        (while  (re-search-forward "\\_<\\(model\\|base\\|color\\|size\\)\\_>\\s-*(\\([^)]*\\))" nil t)
         (let* ((name (match-string 1))
                 (full (match-string 0))
                 (start (match-beginning 0))
                 (end (match-end 0)))
            ;; only take first occurrence per name and only if not already in header
            (unless (or (member name top-tokens) (string-match (format "\\_<%s\\_>\\s-*(" name) header))
              (push full top-tokens))
            ;; remove the matched text and any trailing whitespace/newline
            (save-excursion
              (goto-char start)
              ;; delete any leading whitespace on the same line (so we don't leave an empty indent)
              (when (looking-back "[ \t]+" (line-beginning-position))
                (backward-delete-char (length (match-string 0))))
              ;; delete the region (start..end)
              (delete-region start end)
              ;; remove trailing spaces at point
              (delete-horizontal-space)
              ;; if remainder of line empty, delete the newline
              (when (looking-at "^[ \t]*$")
                (let ((line-start (line-beginning-position))
                      (line-end (line-end-position)))
                  ;; delete the empty line and its newline if any
                  (when (< line-end (point-max))
                    (delete-region line-start (1+ line-end))))))
            ;; continue from start (safe, because we deleted region)
            (goto-char (max 1 start))))
        (setq body (buffer-string))))
    (setq top-tokens (nreverse top-tokens)) ; maintain original found order

    ;; ---------------------------
    ;; Prepare header insertion point: find the '=' (first unquoted '=')
    ;; We'll insert tokens BEFORE that '=' and ensure single-space separation.
    ;; If '=' is on its own line, we append tokens to the previous non-empty header line.
    ;; ---------------------------
    (let ((eq-pos (string-match "=" header)))
      (when eq-pos
        ;; find line containing '='
        (let* ((eq-line-start (save-match-data
                                (with-temp-buffer
                                  (insert header)
                                  (goto-char (1+ eq-pos))
                                  (forward-line 0)
                                  (line-beginning-position))))
               ;; we need index relative to header: compute by searching for the '=' location in header buffer
               )
          ;; nothing else — we'll compute insertion by string ops below
          )))

    ;; Build new header: place tokens before '='
    (let ((new-header header))
      (when top-tokens
        (let* ((tokens-str (mapconcat #'fgd-gen--normalize-whitespace-token top-tokens " "))
               ;; find position of first unquoted '=' in header (searching left to right)
               (eq-index (string-match "=" new-header))
               (insert-pos nil))
          (if (not eq-index)
              ;; no '=', append tokens to end of header (with a space)
              (setq new-header (concat (string-trim-right new-header) " " tokens-str))
            ;; if '=' exists, we want to put tokens before '='; but preserve multi-line header
            ;; strategy: find the start of the line that contains '='.
            (let* ((line-start (save-match-data
                                 (let ((pos eq-index)
                                       (s 0))
                                   ;; find start index of line containing eq-index
                                   (while (and (> pos 0)
                                               (not (eq (aref new-header (1- pos)) ?\n)))
                                     (setq pos (1- pos)))
                                   pos))))
              ;; if '=' is at beginning of line (ignoring whitespace), then we should append tokens to the previous non-empty header line
              (let ((line-text (substring new-header line-start (or (string-match "\n" new-header line-start) (length new-header)))))
                (if (string-match-p "^\\s-*=" line-text)
                    ;; find previous non-empty line
                    (let ((prev-line-end (1- line-start))
                          (prev-line-start nil))
                      (when (>= prev-line-end 0)
                        (setq prev-line-start
                              (let ((p prev-line-end))
                                (while (and (>= p 0)
                                            (not (eq (aref new-header p) ?\n)))
                                  (setq p (1- p)))
                                (1+ p))))
                      (if (and prev-line-start (< prev-line-start line-start))
                          (progn
                            ;; insert tokens at end of previous line
                            (let ((before (substring new-header 0 (string-match "\n" new-header prev-line-start))))
                              ;; fallback simpler approach: insert just before eq-index if not found prev line
                              (setq new-header (concat (substring new-header 0 prev-line-start)
                                                       (string-trim-right (substring new-header prev-line-start line-start))
                                                       " " tokens-str
                                                       (substring new-header line-start)))))
                        ;; can't find previous line safely: fall back to inserting before '=' inline
                        (setq new-header (concat (substring new-header 0 eq-index) " " tokens-str (substring new-header eq-index))))
                  ;; '=' is inline (same line with other content): insert tokens before '=' in that line
                  (setq new-header (concat (substring new-header 0 eq-index) " " tokens-str (substring new-header eq-index)))))))))

      ;; Now normalize whitespace in header: compress repeated spaces/newlines to preserve multi-line but normalize internal spacing:
      (setq new-header (replace-regexp-in-string "[ \t]+" " " new-header))
      ;; But keep newlines: replace " \n" -> "\n", and "\n " -> "\n"
      (setq new-header (replace-regexp-in-string " \\(\n\\)" "\\1" new-header))
      (setq new-header (replace-regexp-in-string "\\(\n\\) " "\\1" new-header))

      ;; ---------------------------
      ;; Prepare body formatting:
      ;; - indent body lines by two spaces
      ;; - ensure opening bracket is on its own indented line ("  [")
      ;; - ensure closing bracket exists on its own indented line ("  ]")
      ;; - remove leading/trailing blank lines
      ;; ---------------------------
      (let* ((body-lines (if (string-empty-p (string-trim body))
                             nil
                           (split-string body "\n" t))) ; t = omit-empty
             (indented-body (mapconcat (lambda (ln) (concat "  " (string-trim-right ln))) body-lines "\n"))
             (has-close (and (string-match-p "\\]" class-text)))
             (new-body-text (if indented-body indented-body "")))
        ;; assemble
        (let ((assembled
               (if (string-empty-p new-body-text)
                   ;; no body found: keep header only (no brackets)
                   (string-trim-right new-header)
                 ;; body exists -> ensure opening bracket line preceded by newline
                 (concat
                  (string-trim-right new-header)
                  "\n  [\n"
                  new-body-text
                  ;; ensure closing bracket
                  (if (string-match-p "\\]" (substring class-text (or bracket-pos 0) (length class-text)))
                      ;; there is some closing bracket in original class region: preserve it as a final line
                      (if (string-match-p "\\]\\s-*\\'" class-text)
                          "\n  ]"
                        ;; there was a ']' somewhere; just append closing bracket for safety
                        "\n  ]")
                    ;; no original ']' -> add one
                    "\n  ]"))))
          assembled)))))

(defun fgd-gen--improve-fgd-syntax (fgd-file)
  "Read FGD-FILE, improve class syntax, and overwrite it.
Returns the new content string."
  (let* ((text (with-temp-buffer
                 (insert-file-contents fgd-file)
                 (buffer-string)))
         (out "")
         (pos 0)
         (len (length text))
         (class-re "^\\s-*@[ \t]*\\(BaseClass\\|PointClass\\|SolidClass\\)\\_>")
         matches)

    ;; Find all class start positions
    (while (and (< pos len)
                (string-match class-re text pos))
      (let ((start (match-beginning 0)))
        (push start matches)
        (setq pos (match-end 0))))
    (setq matches (nreverse matches))

    ;; No classes -> nothing to change
    (unless matches
      (fgd-gen--write-text-file fgd-file text)
      (message "No classes found; no syntax changes applied.")
      (with-temp-buffer (insert text) (buffer-string))
      (return-from fgd-gen--improve-fgd-syntax text))

    ;; Process each class region
    (let ((regions '()))
      (dotimes (i (length matches))
        (let* ((s (nth i matches))
               (e (if (< i (1- (length matches)))
                      (nth (1+ i) matches)
                    len)))
          (push (cons s e) regions)))
      (setq regions (nreverse regions))

      ;; build output by copying non-class, then processed class, in sequence
      (let ((cursor 0))
        (dolist (reg regions)
          (let ((s (car reg)) (e (cdr reg)))
            ;; copy anything before this class unchanged
            (setq out (concat out (substring text cursor s)))
            (let ((class-text (substring text s e)))
              (let ((new-class (fgd-gen--process-single-class class-text)))
                (setq out (concat out new-class))))
            (setq cursor e)))
        ;; append rest
        (setq out (concat out (substring text (or (cadr (last regions)) 0) len))))

      ;; write back
      (fgd-gen--write-text-file fgd-file out)
      out)))

;; -------------------------------------------------------------------
;; Main interactive entry point
;; -------------------------------------------------------------------

;;;###autoload
(defun fgd-gen-generate ()
  "Generate a .fgd file from all @fgd blocks in referenced .dat files and improve syntax."
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

        ;; Write .fgd (raw concatenation)
        (fgd-gen--write-text-file fgd (mapconcat #'identity blocks ""))

        ;; Improve syntax in-place (will overwrite fgd)
        (let ((new-content (fgd-gen--improve-fgd-syntax fgd)))
          ;; Display result in help window
          (with-help-window "*FGD Output*"
            (with-current-buffer standard-output
              (insert new-content)
              (goto-char (point-min))))
          (message "FGD written and improved: %s" fgd))))))

(provide 'fgd-gen)
;;; fgd-gen.el ends here
