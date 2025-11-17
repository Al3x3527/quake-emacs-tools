;;; fgd-gen.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Alex
;;
;; Created: November 17, 2025
;; Modified: November 17, 2025
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;interactive
;;get active file
;;get directory of active file
;;find the .fmf file at ../<name>.fmf or in active file directory
;;get name of .fmf file
;;create <name>.fgd if it does not exist
;;we will overwrite fgd file if it already exists
;;
;;find file named progs.src in active file directory
;;determine if progs.src is type 2 or type 1
;;
;;NOTE type 2 syntax: first lines "#pragma sourcefile <filename>.src"
;;ex. "#pragma sourcefile sv_progs.src"
;;type 1 syntax: first line "<path to program file>"
;;ex "../qwprogs.dat"
;;
;;if progs.src is type 2, use list of sourcefiles
;;else progs.src is only sourcefile
;;create list of program files from source files
;;
;;NOTE program files plain text format so-long-mode. with .dat file extension.
;;
;;for each program file
;;stream file
;;until eof
;;for find instance of "@fgd"
;;find next nil char "^@"
;;write subsequence between "@fgd " and nil char to .fgd file
;;NOTE write subsequence ignoring space after "@fgd" if present.
;;print contents of .fgd file to a help window
;;
;;NOTE this function should write every subsequence occuring between "@fgd " and nil char
;;from every program file to the fgd file without adding new line characters.
;;some program files will have no instances of "@fgd" and this is not an error.
;;
(provide 'fgd-gen)
;;; fgd-gen.el ends here
