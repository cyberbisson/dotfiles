;;; visual-basic-mode.el 
;; This is free software.

;; A mode for editing Visual Basic programs.
;; Modified version of Fred White's visual-basic-mode.el

;; Copyright (C) 1996 Fred White <fwhite@alum.mit.edu>
;; Copyright (C) 1998 Free Software Foundation, Inc.
;;   (additions by Dave Love)

;; Author: Fred White <fwhite@alum.mit.edu>
;; Adapted-by: Dave Love <d.love@dl.ac.uk>
;;           : Kevin Whitefoot <kevin.whitefoot@nopow.abb.no>
;; Version: 1.3 (May 1, 1996)
;; Keywords: languages, basic, Evil


;; (Old) LCD Archive Entry:
;; basic-mode|Fred White|fwhite@alum.mit.edu|
;; A mode for editing Visual Basic programs.|
;; 18-Apr-96|1.0|~/modes/basic-mode.el.Z|

;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;;; Commentary:

;; Purpose of this package:
;;  This is a mode for editing programs written in The World's Most
;;  Successful Programming Language.  It features automatic
;;  indentation, font locking, keyword capitalization, and some minor
;;  convenience functions.

;; Installation instructions
;;  Put basic-mode.el somewhere in your path, compile it, and add the
;;  following to your init file:

;;  (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
;;  (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
;;                                  visual-basic-mode)) auto-mode-alist))

;; Of course, under Windows 3.1, you'll have to name this file
;; something shorter than visual-basic-mode.el

;; Revisions:
;; 1.0 18-Apr-96  Initial version
;; 1.1 Accomodate emacs 19.29+ font-lock-defaults
;;     Simon Marshall <Simon.Marshall@esrin.esa.it>
;  1.2 Rename to visual-basic-mode
;; 1.3 Fix some indentation bugs.
;; 1.3+ Changes by Dave Love: [No attempt at compatibility with
;;      anything other than Emacs 20, sorry, but little attempt to
;;      sanitize for Emacs 20 specifically.]
;;      Change `_' syntax only for font-lock and imenu, not generally;
;;      provide levels of font-locking in the current fashion;
;;      font-lock case-insensitively; use regexp-opt with the font-lok
;;      keywords; imenu support; `visual-basic-split-line', bound to
;;      C-M-j; account for single-statement `if' in indentation; add
;;      keyword "Global"; use local-write-file-hooks, not
;;      write-file-hooks.
;; 1.4 September 1998
;; 1.4 KJW Add begin..end, add extra keywords
;;     Add customisation for single line if.  Disallow by default.
;;     Fix if regexp to require whitespace after if and require then.
;;     Add more VB keywords.  Make begin..end work as if..endif so
;;     that forms are formatted correctly.
;; 1.4.1 KJW Merged Dave Love and KJW versions.
;;     Added keywords suggested by Mickey Ferguson
;;     <MFerguson@peinc.com>
;;     Fixed imenu variable to find private variables and enums

;;     Changed syntax class of =, <, > to punctuation to allow dynamic
;;     abbreviations to pick up only the word at point rather than the
;;     whole expression.

;;     Fixed bug introduced by KJW adding suport for begin...end in
;;     forms whereby a single end outdented.

;;     Partially fixed failure to recognise if statements with
;;     continuations (still fails on 'single line' if with
;;     continuation, ugh).

;;
;; Notes:
;; Dave Love
;; BTW, here's a script for making tags tables that I (Dave Love) have
;; used with reasonable success.  It assumes a hacked version of etags
;; with support for case-folded regexps.  I think this is now in the
;; development version at <URL:ftp://fly.cnuce.cnr.it/pub/> and should
;; make it into Emacs after 20.4.

;; #! /bin/sh

;; # etags-vb: (so-called) Visual (so-called) Basic TAGS generation.
;; # Dave Love <d.love@dl.ac.uk>.  Public domain.
;; # 1997-11-21

;; if [ $# -lt 1 ]; then
;;     echo "Usage: `basename $0` [etags options] VBfile ... [etags options] " 1>&2
;;     exit 1
;; fi

;; if [ $1 = "--help" ] || [ $1 = "-h" ]; then
;;     echo "Usage: `basename $0` [etags options] VBfile ... [etags options]

;; "
;;     etags --help
;; fi

;; exec etags --lang=none -c '/\(global\|public\)[ \t]+\(\(const\|type\)[ \t]+\)*\([a-z_0-9]+\)/\4/' \
;;     -c '/public[ \t]+\(sub\|function\)[ \t]+\([a-z_0-9]+\)/\2/' \
;;   "$@"

;; End Notes Dave Love


;; Known bugs:
;;  Doesn't know about ":" separated stmts
;;  Doesn't recognize single line if statements if these are broken by
;;  line continuation characters


;; todo:
;;  fwd/back-compound-statement
;;  completion over OCX methods and properties.
;;  IDE integration
;;  Change behaviour of ESC-q to recognise words used as paragraph
;;  titles and prevent them being dragged into the previous
;;  paragraph.
;;  etc.


;;; Code:

(provide 'visual-basic-mode)

(defvar visual-basic-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))
(defvar visual-basic-winemacs-p (string-match "Win-Emacs" (emacs-version)))
(defvar visual-basic-win32-p (eq window-system 'w32))

;; Variables you may want to customize.
(defvar visual-basic-mode-indent 8 "*Default indentation per nesting level.")
(defvar visual-basic-fontify-p t "*Whether to fontify Basic buffers.")
(defvar visual-basic-capitalize-keywords-p t
  "*Whether to capitalize BASIC keywords.")
(defvar visual-basic-wild-files "*.frm *.bas *.cls"
  "*Wildcard pattern for BASIC source files.")
(defvar visual-basic-ide-pathname nil
  "*The full pathname of your Visual Basic exe file, if any.")
;; KJW Provide for my preference in if statements
(defvar visual-basic-allow-single-line-if nil
  "*Whether to allow single line if")


(defvar visual-basic-defn-templates
  (list "Public Sub ()\nEnd Sub\n\n"
        "Class\nEnd Class\n\n"
        "Public Function () As Variant\nEnd Function\n\n"
        "Public Property Get ()\nEnd Property\n\n")
  "*List of function templates though which visual-basic-new-sub cycles.")

(defvar visual-basic-imenu-generic-expression
   '((nil "^\\s-*\\(public\\|private\\)*\\s-+\\(declare\\s-+\\)*\\(sub\\|function\\)\\s-+\\(\\sw+\\>\\)"
         4)
    ("Constants"
     "^\\s-*\\(private\\|public\\|global\\)*\\s-*\\(const\\s-+\\)\\(\\sw+\\>\\s-*=\\s-*.+\\)$\\|'"
     3)
    ("Variables"
     "^\\(private\\|public\\|global\\|dim\\)+\\s-+\\(\\sw+\\>\\s-+as\\s-+\\sw+\\>\\)"
     2)
    ("Types" "^\\(public\\s-+\\)*type\\s-+\\(\\sw+\\)" 2)))



(defvar visual-basic-mode-syntax-table nil)
(if visual-basic-mode-syntax-table
    ()
  (setq visual-basic-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\<" visual-basic-mode-syntax-table) ; Comment starter
  (modify-syntax-entry ?\n ">" visual-basic-mode-syntax-table)
  (modify-syntax-entry ?\\ "w" visual-basic-mode-syntax-table)
  (modify-syntax-entry ?\= "." visual-basic-mode-syntax-table)
  (modify-syntax-entry ?\< "." visual-basic-mode-syntax-table)
  (modify-syntax-entry ?\> "." visual-basic-mode-syntax-table)) ; Make =, etc., punctuation so that dynamic abbreviations work properly


(defvar visual-basic-mode-map nil)
(if visual-basic-mode-map
    ()
  (setq visual-basic-mode-map (make-sparse-keymap))
  (define-key visual-basic-mode-map "\t" 'visual-basic-indent-line)
  (define-key visual-basic-mode-map "\r" 'visual-basic-newline-and-indent)
  (define-key visual-basic-mode-map "\M-\C-a" 'visual-basic-beginning-of-defun)
  (define-key visual-basic-mode-map "\M-\C-e" 'visual-basic-end-of-defun)
  (define-key visual-basic-mode-map "\M-\C-h" 'visual-basic-mark-defun)
  (define-key visual-basic-mode-map "\M-\C-\\" 'visual-basic-indent-region)
  (define-key visual-basic-mode-map "\M-q" 'visual-basic-fill-or-indent)
  (define-key visual-basic-mode-map "\M-\C-j" 'visual-basic-split-line)
   (cond (visual-basic-winemacs-p
         (define-key visual-basic-mode-map '(control C) 'visual-basic-start-ide))
        (visual-basic-win32-p
         (define-key visual-basic-mode-map (read "[?\\S-\\C-c]") 'visual-basic-start-ide)))
  (if visual-basic-xemacs-p
      (progn
        (define-key visual-basic-mode-map "\M-G" 'visual-basic-grep)
        (define-key visual-basic-mode-map '(meta backspace) 'backward-kill-word)
        (define-key visual-basic-mode-map '(control meta /) 'visual-basic-new-sub))))


;; These abbrevs are valid only in a code context.
(defvar visual-basic-mode-abbrev-table nil)

(defvar visual-basic-mode-hook ())


;; Is there a way to case-fold all regexp matches?
;; Change KJW Add enum, , change matching from 0 or more to zero or one for public etc.
(eval-and-compile
  (defconst visual-basic-defun-start-regexp
    (concat
     "^[ \t]*\\([Pp]ublic \\|[Pp]rivate \\|[Ss]tatic\\|[Ff]riend \\)?"
     "\\([Cc]lass\\|[Ss]ub\\|[Ff]unction\\|[Pp]roperty +[GgSsLl]et\\|[Tt]ype\\|[Ee]num\\)"
     "[ \t]+\\(\\w+\\)[ \t]*(?")))


(defconst visual-basic-defun-end-regexp
  "^[ \t]*[Ee]nd \\([Cc]lass\\|[Ss]ub\\|[Ff]unction\\|[Pp]roperty\\|[Tt]ype\\|[Ee]num\\)")


;; Includes the compile-time #if variation.
;; KJW fixed if to require a whitespace so as to avoid matching, for
;; instance, iFileName and to require then.

;; Two versions; one recognizes single line if just as though it were
;; a multi-line and the other does not.  Modified again to remove the
;; requirement for then so as to allow it to match if statements that
;; have continuations.
;;(defconst visual-basic-if-regexp
;;   "^[ \t]*#?[Ii]f[ \t]+.*[ \t]+[Tt]hen[ \t]*.*\\('\\|$\\)")
(defconst visual-basic-if-regexp
   "^[ \t]*#?[Ii]f[ \t]+.*[ \t_]+")

(defconst visual-basic-ifthen-regexp "^[ \t]*#?[Ii]f.+\\<[Tt]hen\\>\\s-\\S-+")

(defconst visual-basic-else-regexp "^[ \t]*#?[Ee]lse\\([Ii]f\\)?")
(defconst visual-basic-endif-regexp "[ \t]*#?[Ee]nd[ \t]*[Ii]f")

(defconst visual-basic-continuation-regexp "^.*\_[ \t]*$")
(eval-and-compile
  (defconst visual-basic-label-regexp "^[ \t]*[a-zA-Z0-9_]+:$"))

(defconst visual-basic-select-regexp "^[ \t]*[Ss]elect[ \t]+[Cc]ase")
(defconst visual-basic-case-regexp "^[ \t]*[Cc]ase")
(defconst visual-basic-select-end-regexp "^[ \t]*[Ee]nd[ \t]+[Ss]elect")


(defconst visual-basic-for-regexp "^[ \t]*[Ff]or\\b")
(defconst visual-basic-next-regexp "^[ \t]*[Nn]ext\\b")

(defconst visual-basic-do-regexp "^[ \t]*[Dd]o\\b")
(defconst visual-basic-loop-regexp "^[ \t]*[Ll]oop\\b")

(defconst visual-basic-while-regexp "^[ \t]*[Ww]hile\\b")
(defconst visual-basic-wend-regexp "^[ \t]*[Ww]end\\b")

;; Added KJW Begin..end for forms
(defconst visual-basic-begin-regexp "^[ \t]*[Bb]egin)?")
;; This has created a bug.  End on its own in code should not outdent.
;; How can we fix this?  They are used in separate Lisp expressions so
;; add another one.
(defconst visual-basic-end-begin-regexp "^[ \t]*[Ee]nd")

(defconst visual-basic-with-regexp "^[ \t]*[Ww]ith\\b")
(defconst visual-basic-end-with-regexp "^[ \t]*[Ee]nd[ \t]+[Ww]ith\\b")

(defconst visual-basic-blank-regexp "^[ \t]*$")
(defconst visual-basic-comment-regexp "^[ \t]*\\s<.*$")


;; This is some approximation of the set of reserved words in Visual Basic.
(eval-and-compile
  (defvar visual-basic-all-keywords
  '("Add" "Aggregate" "And" "App" "AppActivate" "Application" "Array" "As"
    "Asc" "AscB" "Atn" "Attribute"
    "Beep" "Begin" "BeginTrans" "Boolean" "ByVal" "ByRef"
    "CBool" "CByte" "CCur"
    "CDate" "CDbl" "CInt" "CLng" "CSng" "CStr" "CVErr" "CVar" "Call"
    "Case" "ChDir" "ChDrive" "Character" "Choose" "Chr" "ChrB"
    "Class" "ClassModule" "Clipboard" "Close" "Collection" "Column" "Columns"
    "Command" "CommitTrans" "CompactDatabase" "Component" "Components"
    "Const" "Container" "Containers" "Cos" "CreateDatabase" "CreateObject"
    "CurDir" "Currency"
    "DBEngine" "DDB" "Data" "Database" "Databases"
    "Date" "DateAdd" "DateDiff" "DatePart" "DateSerial" "DateValue" "Day"
    "Debug" "Declare" "Deftype" "DeleteSetting" "Dim" "Dir" "Do"
    "DoEvents" "Domain"
    "Double" "Dynaset" "EOF" "Each" "Else" "End" "EndProperty"
    "Enum" "Environ" "Erase" "Err" "Error" "Exit" "Exp" "FV" "False" "Field"
    "Fields" "FileAttr" "FileCopy" "FileDateTime" "FileLen" "Fix" "Font" "For"
    "Form" "FormTemplate" "Format" "Forms" "FreeFile" "FreeLocks" "Friend"
    "Function"
    "Get" "GetAllSettings" "GetAttr" "GetObject" "GetSetting" "Global" "GoSub"
    "GoTo" "Group" "Groups" "Hex" "Hour" "IIf" "IMEStatus" "IPmt" "IRR"
    "If" "Implements" "Inherits" "InStr" "Input" "Int" "Integer" "Is" "IsArray" "IsDate"
    "IsEmpty" "IsError" "IsMissing" "IsNull" "IsNumeric" "IsObject" "Kill"
    "LBound" "LCase" "LOF" "LSet" "LTrim" "Left" "Len" "Let" "Like" "Line"
    "Load" "LoadPicture" "LoadResData" "LoadResPicture" "LoadResString" "Loc"
    "Lock" "Log" "Long" "Loop" "MDIForm" "MIRR" "Me" "MenuItems"
    "MenuLine" "Mid" "Minute" "MkDir" "Month" "MsgBox" "NPV" "NPer" "Name"
    "New" "Next" "Not" "Now" "Nothing" "Object" "Oct" "On" "Open"
    "OpenDatabase"
    "Operator" "Option" "Optional"
    "Or" "PPmt" "PV" "Parameter" "Parameters" "Partition"
    "Picture" "Pmt" "Print" "Printer" "Printers" "Private" "ProjectTemplate"
    "Property"
    "Properties" "Public" "Put" "QBColor" "QueryDef" "QueryDefs"
    "RSet" "RTrim" "Randomize" "Rate" "ReDim" "Recordset" "Recordsets"
    "RegisterDatabase" "Relation" "Relations" "Rem" "RepairDatabase"
    "Reset" "Resume" "Return" "Right" "RmDir" "Rnd" "Rollback" "RowBuffer"
    "SLN" "SYD" "SavePicture" "SaveSetting" "Screen" "Second" "Seek"
    "SelBookmarks" "Select" "SelectedComponents" "SendKeys" "Set"
    "SetAttr" "SetDataAccessOption" "SetDefaultWorkspace" "Sgn" "Shell"
    "Sin" "Single" "Snapshot" "Space" "Spc" "Sqr" "Static" "Step" "Stop" "Str"
    "StrComp" "StrConv" "String" "Sub" "SubMenu" "Switch" "Tab" "Table"
    "TableDef" "TableDefs" "Tan" "Then" "Time" "TimeSerial" "TimeValue"
    "Timer" "To" "Trim" "True" "Type" "TypeName" "UBound" "UCase" "Unload"
    "Unlock" "Val" "Variant" "VarType" "Verb" "Weekday" "Wend"
    "While" "Width" "With" "Workspace" "Workspaces" "Write" "Year")))

(defvar visual-basic-font-lock-keywords-1
  (eval-when-compile
    (list
     ;; Names of functions.
     (list visual-basic-defun-start-regexp
           '(1 font-lock-keyword-face nil t)
           '(2 font-lock-keyword-face nil t)
           '(3 font-lock-function-name-face))

     ;; Statement labels
     (cons visual-basic-label-regexp 'font-lock-keyword-face)

     ;; Case values
     ;; String-valued cases get font-lock-string-face regardless.
     (list "^[ \t]*case[ \t]+\\([^'\n]+\\)" 1 'font-lock-keyword-face t)

     ;; Any keywords you like.
     (list (concat "\\<" (regexp-opt
                          '("Dim" "If" "Then" "Else" "ElseIf" "End If") t)
                   "\\>")
           1 'font-lock-keyword-face))))

(defvar visual-basic-font-lock-keywords-2
  (append visual-basic-font-lock-keywords-1
          (eval-when-compile
            `((,(concat "\\<" (regexp-opt visual-basic-all-keywords t) "\\>")
                   1 font-lock-keyword-face)))))

(defvar visual-basic-font-lock-keywords visual-basic-font-lock-keywords-1)


(put 'visual-basic-mode 'font-lock-keywords 'visual-basic-font-lock-keywords)

(defun visual-basic-mode ()
  "A mode for editing Microsoft Visual Basic programs.
Features automatic indentation, font locking, keyword capitalization,
and some minor convenience functions.
Commands:
\\{visual-basic-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map visual-basic-mode-map)
  (setq major-mode 'visual-basic-mode)
  (setq mode-name "Visual Basic")
  (set-syntax-table visual-basic-mode-syntax-table)

  (add-hook 'local-write-file-hooks 'visual-basic-untabify)

  (setq local-abbrev-table visual-basic-mode-abbrev-table)
  (if visual-basic-capitalize-keywords-p
      (progn
        (make-local-variable 'pre-abbrev-expand-hook)
        (add-hook 'pre-abbrev-expand-hook 'visual-basic-pre-abbrev-expand-hook)
        (abbrev-mode 1)))

  (make-local-variable 'comment-start)
  (setq comment-start "' ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "'+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'visual-basic-indent-line)

  (if visual-basic-fontify-p
      (visual-basic-enable-font-lock))

  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression visual-basic-imenu-generic-expression)

  (set (make-local-variable 'imenu-syntax-alist) `((,(string-to-char "_") . "w")))
  (set (make-local-variable 'imenu-case-fold-search) t)

  ;;(make-local-variable 'visual-basic-associated-files)
  ;; doing this here means we need not check to see if it is bound later.
  (add-hook 'find-file-hooks 'visual-basic-load-associated-files)

  (run-hooks 'visual-basic-mode-hook))


(defun visual-basic-enable-font-lock ()
  ;; Emacs 19.29 requires a window-system else font-lock-mode errs out.
  (cond ((or visual-basic-xemacs-p window-system)

         ;; In win-emacs this sets font-lock-keywords back to nil!
         (if visual-basic-winemacs-p
             (font-lock-mode 1))

         ;; Accomodate emacs 19.29+
         ;; From: Simon Marshall <Simon.Marshall@esrin.esa.it>
         (cond ((boundp 'font-lock-defaults)
                (make-local-variable 'font-lock-defaults)
                (setq font-lock-defaults
                      `((visual-basic-font-lock-keywords
                         visual-basic-font-lock-keywords-1
                         visual-basic-font-lock-keywords-2)
                        nil t ((,(string-to-char "_") . "w")))))
               (t
                (make-local-variable 'font-lock-keywords)
                (setq font-lock-keywords visual-basic-font-lock-keywords)))

         (if visual-basic-winemacs-p
             (font-lock-fontify-buffer)
           (font-lock-mode 1)))))

;; KJW should add some odds and bobs here to cover "end if" one way
;; could be to create the abbreviations by removing whitespace then we
;; could put "end if", "end with" and so on in the keyword table
;; Another idea would be to make it intelligent enough to substitute
;; the correct end for the construct (with, select, if)
;; Is this what the abbrev table hook entry is for?
(defun visual-basic-construct-keyword-abbrev-table ()
  (if visual-basic-mode-abbrev-table
      nil
    (let ((words visual-basic-all-keywords)
          (word nil)
          (list nil))
      (while words
        (setq word (car words)
              words (cdr words))
        (setq list (cons (list (downcase word) word) list)))

      (define-abbrev-table 'visual-basic-mode-abbrev-table list))))

;; Would like to do this at compile-time.
(visual-basic-construct-keyword-abbrev-table)


(defun visual-basic-in-code-context-p ()
  (if (fboundp 'buffer-syntactic-context) ; XEmacs function.
      (null (buffer-syntactic-context))
    ;; Attempt to simulate buffer-syntactic-context
    ;; I don't know how reliable this is.
    (let* ((beg (save-excursion
                  (beginning-of-line)
                  (point)))
           (list
            (parse-partial-sexp beg (point))))
      (and (null (nth 3 list))          ; inside string.
           (null (nth 4 list))))))      ; inside comment


(defun visual-basic-pre-abbrev-expand-hook ()
  ;; Allow our abbrevs only in a code context.
  (setq local-abbrev-table
        (if (visual-basic-in-code-context-p)
            visual-basic-mode-abbrev-table)))


(defun visual-basic-newline-and-indent (&optional count)
  "Insert a newline, updating indentation."
  (interactive)
  (expand-abbrev)
  (save-excursion
    (visual-basic-indent-line))
  (call-interactively 'newline-and-indent))

(defun visual-basic-beginning-of-defun ()
  (interactive)
  (re-search-backward visual-basic-defun-start-regexp))

(defun visual-basic-end-of-defun ()
  (interactive)
  (re-search-forward visual-basic-defun-end-regexp))

(defun visual-basic-mark-defun ()
  (interactive)
  (beginning-of-line)
  (visual-basic-end-of-defun)
  (set-mark (point))
  (visual-basic-beginning-of-defun)
  (if visual-basic-xemacs-p
      (zmacs-activate-region)))

(defun visual-basic-indent-defun ()
  (interactive)
  (save-excursion
    (visual-basic-mark-defun)
    (call-interactively 'visual-basic-indent-region)))


(defun visual-basic-fill-long-comment ()
  "Fills block of comment lines around point."
  ;; Derived from code in ilisp-ext.el.
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((comment-re "^[ \t]*\\s<+[ \t]*"))
      (if (looking-at comment-re)
          (let ((fill-prefix
                 (buffer-substring
                  (progn (beginning-of-line) (point))
                  (match-end 0))))

            (while (and (not (bobp))
                        (looking-at visual-basic-comment-regexp))
              (forward-line -1))
            (if (not (bobp)) (forward-line 1))

            (let ((start (point)))

              ;; Make all the line prefixes the same.
              (while (and (not (eobp))
                          (looking-at comment-re))
                (replace-match fill-prefix)
                (forward-line 1))

              (if (not (eobp))
                  (beginning-of-line))

              ;; Fill using fill-prefix
              (fill-region-as-paragraph start (point))))))))


(defun visual-basic-fill-or-indent ()
  "Fill long comment around point, if any, else indent current definition."
  (interactive)
  (cond ((save-excursion
           (beginning-of-line)
           (looking-at visual-basic-comment-regexp))
         (visual-basic-fill-long-comment))
        (t
         (visual-basic-indent-defun))))


(defun visual-basic-new-sub ()
  "Insert template for a new subroutine.  Repeat to cycle through alternatives."
  (interactive)
  (beginning-of-line)
  (let ((templates (cons visual-basic-blank-regexp
                         visual-basic-defn-templates))
        (tem nil)
        (bound (point)))
    (while templates
      (setq tem (car templates)
            templates (cdr templates))
      (cond ((looking-at tem)
             (replace-match (or (car templates)
                                ""))
             (setq templates nil))))

    (search-backward "()" bound t)))


(defun visual-basic-untabify ()
  "Do not allow any tabs into the file."
  (if (eq major-mode 'visual-basic-mode)
      (untabify (point-min) (point-max)))
  nil)

(defun visual-basic-default-tag ()
  (if (and (not (bobp))
           (save-excursion
             (backward-sexp)
             (looking-at "\\w")))
      (backward-word 1))
  (let ((s (point))
        (e (save-excursion
             (forward-sexp)
             (point))))
    (buffer-substring s e)))

(defun visual-basic-grep (tag)
  "Search BASIC source files in current directory for TAG."
  (interactive
   (list (let* ((def (visual-basic-default-tag))
                (tag (read-string
                      (format "Grep for [%s]: " def))))
           (if (string= tag "") def tag))))
  (grep (format "grep -n %s %s" tag visual-basic-wild-files)))


;;; IDE Connection.

(defun visual-basic-buffer-project-file ()
  "Return a guess as to the project file associated with the current buffer."
  (car (directory-files (file-name-directory (buffer-file-name)) t "\\.vbp")))

(defun visual-basic-start-ide ()
  "Start Visual Basic (or your favorite IDE, (after Emacs, of course))
on the first project file in the current directory.
Note: it's not a good idea to leave Visual Basic running while you
are editing in Emacs, since Visual Basic has no provision for reloading
changed files."
  (interactive)
  (let (file)
    (cond ((null visual-basic-ide-pathname)
           (error "No pathname set for Visual Basic.  See visual-basic-ide-pathname"))
          ((null (setq file (visual-basic-buffer-project-file)))
           (error "No project file found"))
          ((fboundp 'win-exec)
           (iconify-emacs)
           (win-exec visual-basic-ide-pathname 'win-show-normal file))
          ((fboundp 'start-process)
           (iconify-frame (selected-frame))
           (start-process "*VisualBasic*" nil visual-basic-ide-pathname file))
          (t
           (error "No way to spawn process!")))))



;;; Indentation-related stuff.

(defun visual-basic-indent-region (start end)
  "Perform visual-basic-indent-line on each line in region."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (and (not (eobp))
                (< (point) end))
      (if (not (looking-at visual-basic-blank-regexp))
          (visual-basic-indent-line))
      (forward-line 1)))

  (cond ((fboundp 'zmacs-deactivate-region)
         (zmacs-deactivate-region))
        ((fboundp 'deactivate-mark)
         (deactivate-mark))))



(defun visual-basic-previous-line-of-code ()
  (if (not (bobp))
      (forward-line -1))        ; previous-line depends on goal column
  (while (and (not (bobp))
              (or (looking-at visual-basic-blank-regexp)
                  (looking-at visual-basic-comment-regexp)))
    (forward-line -1)))


(defun visual-basic-find-original-statement ()
  "If the current line is a continuation, move back to the original stmt."
  (let ((here (point)))
    (visual-basic-previous-line-of-code)
    (while (and (not (bobp))
                (looking-at visual-basic-continuation-regexp))
      (setq here (point))
      (visual-basic-previous-line-of-code))
    (goto-char here)))

(defun visual-basic-find-matching-stmt (open-regexp close-regexp)
  ;; Searching backwards
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (visual-basic-previous-line-of-code)
      (visual-basic-find-original-statement)
      (cond ((looking-at close-regexp)
             (setq level (+ level 1)))
            ((looking-at open-regexp)
             (setq level (- level 1)))))))

(defun visual-basic-find-matching-if ()
  (visual-basic-find-matching-stmt visual-basic-if-regexp
                                   visual-basic-endif-regexp))

(defun visual-basic-find-matching-select ()
  (visual-basic-find-matching-stmt visual-basic-select-regexp
                                   visual-basic-select-end-regexp))

(defun visual-basic-find-matching-for ()
  (visual-basic-find-matching-stmt visual-basic-for-regexp
                                   visual-basic-next-regexp))

(defun visual-basic-find-matching-do ()
  (visual-basic-find-matching-stmt visual-basic-do-regexp
                                   visual-basic-loop-regexp))

(defun visual-basic-find-matching-while ()
  (visual-basic-find-matching-stmt visual-basic-while-regexp
                                   visual-basic-wend-regexp))

(defun visual-basic-find-matching-with ()
  (visual-basic-find-matching-stmt visual-basic-with-regexp
                                   visual-basic-end-with-regexp))

;;; If this fails it must return the indent of the line preceding the
;;; end not the first line because end without matching begin is a
;;; normal simple statement
(defun visual-basic-find-matching-begin ()
  (let ((original-point (point)))
    (visual-basic-find-matching-stmt visual-basic-begin-regexp
                                     visual-basic-end-begin-regexp)
    (if (bobp) ;failed to find a matching begin so assume that it is
               ;an end statement instead and use the indent of the
               ;preceding line.
        (progn (goto-char original-point)
               (visual-basic-previous-line-of-code)))))


(defun visual-basic-calculate-indent ()
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      ;; Some cases depend only on where we are now.
      (cond ((or (looking-at visual-basic-defun-start-regexp)
                 (looking-at visual-basic-label-regexp)
                 (looking-at visual-basic-defun-end-regexp))
             0)
;             ((looking-at visual-basic-label-regexp) 0)

;             ((looking-at visual-basic-defun-end-regexp)
;              (- (current-indentation) visual-basic-mode-indent))

            ;; The outdenting stmts, which simply match their original.
            ((or (looking-at visual-basic-else-regexp)
                 (looking-at visual-basic-endif-regexp))
             (visual-basic-find-matching-if)
             (current-indentation))

            ;; All the other matching pairs act alike.
            ((looking-at visual-basic-next-regexp) ; for/next
             (visual-basic-find-matching-for)
             (current-indentation))

            ((looking-at visual-basic-loop-regexp) ; do/loop
             (visual-basic-find-matching-do)
             (current-indentation))

            ((looking-at visual-basic-wend-regexp) ; while/wend
             (visual-basic-find-matching-while)
             (current-indentation))

            ((looking-at visual-basic-end-with-regexp) ; with/end with
             (visual-basic-find-matching-with)
             (current-indentation))

            ((looking-at visual-basic-select-end-regexp) ; select case/end select
             (visual-basic-find-matching-select)
             (current-indentation))

            ;; A case of a select is somewhat special.
            ((looking-at visual-basic-case-regexp)
             (visual-basic-find-matching-select)
             (+ (current-indentation) visual-basic-mode-indent))

            ;; Added KJW: Make sure that this comes after the cases
            ;; for if..endif, end select because end-regexp will also
            ;; match "end select" etc.
            ((looking-at visual-basic-end-begin-regexp) ; begin/end
             (visual-basic-find-matching-begin)
             (current-indentation))

            (t
             ;; Other cases which depend on the previous line.
             (visual-basic-previous-line-of-code)

             ;; Skip over label lines, which always have 0 indent.
             (while (looking-at visual-basic-label-regexp)
               (visual-basic-previous-line-of-code))

             (cond
              ((looking-at visual-basic-continuation-regexp)
               (visual-basic-find-original-statement)
               ;; Indent continuation line under matching open paren,
               ;; or else one word in.
               (let* ((orig-stmt (point))
                      (matching-open-paren
                       (condition-case ()
                           (save-excursion
                             (goto-char original-point)
                             (beginning-of-line)
                             (backward-up-list 1)
                             ;; Only if point is now w/in cont. block.
                             (if (<= orig-stmt (point))
                                 (current-column)))
                         (error nil))))
                 (cond (matching-open-paren
                        (1+ matching-open-paren))
                       (t
                        ;; Else, after first word on original line.
                        (back-to-indentation)
                        (forward-word 1)
                        (while (looking-at "[ \t]")
                          (forward-char 1))
                        (current-column)))))
              (t
               (visual-basic-find-original-statement)

               (let ((indent (current-indentation)))
                 ;; All the various +indent regexps.
                 (cond ((looking-at visual-basic-defun-start-regexp)
                        (+ indent visual-basic-mode-indent))

                       ((and (or (looking-at visual-basic-if-regexp)
                                 (looking-at visual-basic-else-regexp))
                             (not (and visual-basic-allow-single-line-if
                                       (looking-at visual-basic-ifthen-regexp))))
                        (+ indent visual-basic-mode-indent))

                       ((or (looking-at visual-basic-select-regexp)
                            (looking-at visual-basic-case-regexp))
                        (+ indent visual-basic-mode-indent))

                       ((or (looking-at visual-basic-do-regexp)
                            (looking-at visual-basic-for-regexp)
                            (looking-at visual-basic-while-regexp)
                            (looking-at visual-basic-with-regexp)
                            (looking-at visual-basic-begin-regexp))
                        (+ indent visual-basic-mode-indent))

                       (t
                        ;; By default, just copy indent from prev line.
                        indent))))))))))

(defun visual-basic-indent-to-column (col)
  (let* ((bol (save-excursion
                (beginning-of-line)
                (point)))
         (point-in-whitespace
          (<= (point) (+ bol (current-indentation))))
         (blank-line-p
          (save-excursion
            (beginning-of-line)
            (looking-at visual-basic-blank-regexp))))

    (cond ((/= col (current-indentation))
           (save-excursion
             (beginning-of-line)
             (back-to-indentation)
             (delete-region bol (point))
             (indent-to col))))

    ;; If point was in the whitespace, move back-to-indentation.
    (cond (blank-line-p
           (end-of-line))
          (point-in-whitespace
           (back-to-indentation)))))


(defun visual-basic-indent-line ()
  "Indent current line for BASIC."
  (interactive)
  (visual-basic-indent-to-column (visual-basic-calculate-indent)))


(defun visual-basic-split-line ()
  "Split line at point, adding continuation character or continuing a comment.
In Abbrev mode, any abbrev before point will be expanded."
  (interactive)
  (let ((pps-list (parse-partial-sexp (save-excursion
                                        (beginning-of-line)
                                        (point))
                                      (point))))
    ;; Dispatch on syntax at this position.
    (cond ((equal t (nth 4 pps-list))  ; in comment
           (indent-new-comment-line))
          ((equal t (nth 4 pps-list))   ; in string
           (error "Can't break line inside a string"))
          (t (just-one-space)           ; leading space on next line
                                        ; doesn't count, sigh
             (insert "_")
             (visual-basic-newline-and-indent)))))

(provide 'visual-basic-mode)


;;; Some experimental functions

;;; Load associated files listed in the file local variables block
(defun visual-basic-load-associated-files ()
  "Load files that are useful to have around when editing the source of the file that has just been loaded.
The file must have a local variable that lists the files to be loaded.
If the file name is relative it is relative to the directory
containing the current buffer.  If the file is already loaded nothing
happens, this prevents circular references causing trouble.  After an
associated file is loaded its associated files list will be
processed."
  (if (boundp 'visual-basic-associated-files)
      (let ((files visual-basic-associated-files)
            (file nil))
        (while files
          (setq file (car files)
                files (cdr files))
          (message "Load associated file: %s" file)
          (visual-basic-load-file-ifnotloaded file default-directory)))))



(defun visual-basic-load-file-ifnotloaded (file default-directory)
  "Load file if not already loaded.
If file is relative then default-directory provides the path"
  (let((file-absolute (expand-file-name file default-directory)))
    (if (get-file-buffer file-absolute); don't do anything if the buffer is already loaded
        ()
      (find-file-noselect file-absolute ))))



;;; visual-basic-mode.el ends here
