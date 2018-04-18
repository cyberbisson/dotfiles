;; emacs.el
;; This is my Emacs initialization script.  It works with Emacs 19-25
;; and greater.
;;
;; Features:
;; - Fully compatible with Emacs v19 to current, for X, Windows, and shell.
;; - Aims to maintains basic text editing behavior of Emacs since v19.
;; - Minimal startup fuss (e.g., no text in *scratch*, no start-up message).
;; - More modeline info (e.g., day of week, battery %)
;; - --merge and --diff command-line options (suitable for Perforce, et al.).
;; - Automatic load of desktop when $EMACS_SERVER_FILE is defined in the env.
;; - Versioned backup away from local directory when ~/.emacs.bak exists.
;; - --instance-id command line gives emacs a name (good for keeping track).
;; - Different customized syntax coloration based on dark or light background.
;;
;; Matt Bisson	4/13/2017

;; -----------------------------------------------------------------------------
;; Top-level configuration routines:
;; -----------------------------------------------------------------------------

(defun custom-configure-emacs ()
  "This is the full configuration/customization function for Emacs."

  (provide-customized-features)

  (set-key-bindings)

  ;; Add some file types to interpret
  (associate-file-types)

  ;; Set some configuration variables
  (setq
   ;; Some C specific modes
   c-backslash-column           45
   c-default-style              "bsd"
   c-basic-offset               4
   c-tab-always-indent          nil
   compilation-scroll-output    t

   ;; I don't like that I can delete the prompt in the shell.
   comint-prompt-read-only      t

   ;; By default, I'll want to wrap my lines at 80 columns
   default-fill-column          80

   ;; Longer selections tend to make the mode-line too long (with battery)
;  display-time-24hr-format     t
;  display-time-day-and-date    t
;  display-time-format          "%a-%Y/%m/%d-%H:%M"
   display-time-format          "%a/%H:%M"

   ;; I've already read it
   inhibit-startup-message      t

   ;; Don't like the initial stuff in my scratch buffer
   initial-scratch-message      nil

   ;; I'm not sure why anybody thought this was a good idea...
   isearch-lax-whitespace       nil
   isearch-regexp-lax-whitespace nil

   ;; Always making backups
   make-backup-files t

   ;; Only Enter and C-g exit the search
   search-exit-option           nil

   ;; It's stupid that this is not the default behavior
   server-name                  (getenv "EMACS_SERVER_FILE")

   ;; Get rid of shift moving the mark around...
   shift-select-mode            nil)

  ;; If we have a backup drop-zone, customize it to use versions, etc...
  (let* ((backup-dir   "~/.emacs.bak")
         (attributes   (file-attributes backup-dir))
         (file-exists  (not (null attributes)))
         (is-directory (car attributes)))
    (if (and (not (eq system-type 'ms-dos))
             file-exists
             is-directory)
        (custom-configure-backups backup-dir)))

  ;; Clear out the "current" line when GDB quits
  (add-hook 'kill-buffer-hook 'gud-kill-buffer)

  ;; If and when we use Google Go, the language relies on TAB indentation by
  ;; default.  Set the tab stop to something reasonable.
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))

  ;; There is not a great way to paste into the term-mode buffer by default.
  (add-hook 'term-mode-hook
   (lambda ()
     (require 'term)
     (define-key term-raw-map (kbd "C-c C-y") 'yank)
     (define-key term-raw-map (kbd "C-c M-y") 'yank-pop)))

  ;; Provide some nice GUI tools from the Emacs command-line for diff and merge
  (add-to-list 'command-switch-alist '("--diff"  . command-line-diff))
  (add-to-list 'command-switch-alist '("--merge" . command-line-merge))

  ;; Allow us to "name" our Emacs instance from the command line
  (add-to-list 'command-switch-alist
               '("--instance-id" . command-line-instance-id))

  ;; Show me the time, so I can tell how bored I am.  Have to update the time
  ;; _after_ changing settings.
  (display-time)

  ;; I want to know what column and line I am in
  (column-number-mode 1)
  (line-number-mode   1)

  ;; Show selected marked area
  (transient-mark-mode -1)

  ;; I really don't get what all the hubub is about...
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)

  ;; Some additional major customizations based on display capabilities:
  (if (not terminal-frame)
      (custom-configure-for-xwindows)
      (custom-configure-for-terminal))

  (if (< 19 emacs-major-version) (custom-configure-emacs-20))

  ;; Set everything up for us to use a desktop (saved session) if asked.
  ;;
  ;; This should be the last thing the init-file does so that all modes are
  ;; fully configured prior to resurrecting any stateful buffers from the
  ;; desktop.  Otherwise, we get things like whitespace-mode with all the
  ;; settings we don't want to have enabled.
  (let ((env-dt-dir (getenv "EMACS_DESKTOP_DIR")))
    (if (and env-dt-dir
             (file-exists-p env-dt-dir)
             (not (member "--no-desktop" command-line-args)))
        (desktop-change-dir env-dt-dir))))

(defun custom-configure-emacs-20 ()
  "Customizations that are only applicable to Emacs 20 and above."

  (show-paren-mode 1)

  ;; Using buffer-local variables to set defaults.  We could use setq-default
  ;; instead of custom-set-variables if we cared...
  (if (getenv "USE_HOLLY")
      (custom-set-variables '(tab-width        4))
      (custom-set-variables '(indent-tabs-mode nil)))

  (if (< 20 emacs-major-version)
      (progn (eval-when-compile (declare-function display-battery ()))
             (custom-configure-emacs-21))
      (display-battery)))

(defun custom-configure-emacs-21 ()
  "Customizations that are only applicable to Emacs 21 and above."

  ;; Restore functionality s.t. down adds lines to the end of the file
  (setq next-line-add-newlines t)

  (blink-cursor-mode -1)
  (if (not terminal-frame) (tool-bar-mode -1))

  (condition-case nil
      (display-battery-mode 1)
    (error nil)) ; Ignore errors if AC powered!

  (if (< 21 emacs-major-version) (custom-configure-emacs-22)))

(defun custom-configure-emacs-22 ()
  "Customizations that are only applicable to Emacs 22 and above."

  ;; Org-mode only exists in version 22 and above.
  (setq
   auto-mode-alist       (append '(("\\.org$" . org-mode)) auto-mode-alist)
   org-startup-truncated nil)

  ;; Show me a small set of extraneous bits of whitespace.
  (global-whitespace-mode 1)
  (setq whitespace-style '(face trailing table lines empty tab-mark)))

;; -----------------------------------------------------------------------------
;; Font coloring configuration:
;; -----------------------------------------------------------------------------

(defconst bg-dark-faces
  ;; face                         fg              bg  st  b   i   u
  '((ebrowse-root-class           "Violet"        nil nil nil nil nil)
    (font-lock-builtin-face       "LightSalmon"   nil nil t   nil nil)
    (font-lock-comment-face       "PaleTurquoise" nil nil nil t   nil)
    (font-lock-constant-face      "Coral"         nil nil nil nil nil)
    (font-lock-doc-face           "LightBlue"     nil nil nil t   nil)
    (font-lock-function-name-face "Aquamarine"    nil nil nil t   nil)
    (font-lock-keyword-face       "IndianRed"     nil nil t   nil nil)
    (font-lock-string-face        "LightSkyBlue"  nil nil nil nil nil)
    (font-lock-type-face          "Violet"        nil nil nil nil nil)
    (font-lock-variable-name-face "Turquoise"     nil nil nil nil nil)
    (minibuffer-prompt            "DodgerBlue"    nil nil t   nil nil)
    (sh-heredoc                   "Chocolate"     nil nil nil nil nil)

    ;; Changing the background here:
    (gdb-selection                nil "MidnightBlue"  nil nil nil nil)
    (highlight                    nil "CadetBlue"     nil nil nil nil)
    (region                       nil "Firebrick"     nil nil nil nil))
  "The complete set of `font-lock-mode' faces for Emacs used when the background
is dark.")

(defconst bg-light-faces
  ;; face                         fg                 bg  st  b   i   u
  '((ebrowse-root-class           "Maroon"           nil nil nil nil nil)
    (font-lock-builtin-face       "DodgerBlue4"      nil nil t   nil nil)
    (font-lock-comment-face       "DarkGreen"        nil nil nil t   nil)
    (font-lock-constant-face      "Burlywood4"       nil nil nil nil nil)
    (font-lock-doc-face           "DarkGreen"        nil nil nil t   nil)
    (font-lock-function-name-face "OrangeRed2"       nil nil nil t   nil)
    (font-lock-keyword-face       "FireBrick"        nil nil t   nil nil)
    (font-lock-string-face        "Chocolate"        nil nil nil nil nil)
    (font-lock-type-face          "Maroon"           nil nil nil nil nil)
    (font-lock-variable-name-face "SteelBlue"        nil nil nil nil nil)
    (minibuffer-prompt            "DodgerBlue4"      nil nil t   nil nil)
    (sh-heredoc                   "Chocolate"        nil nil nil nil nil)
    (whitespace-line              "Red1"             nil nil   t nil nil)

    ;; Changing the background here:
    (gdb-selection                 nil "DarkSeaGreen3"   nil nil nil nil)
    (highlight                     nil "CadetBlue"       nil nil nil nil)
    (region                        nil "LightSteelBlue3" nil nil nil nil))
  "The complete set of `font-lock-mode' faces for Emacs used when the background
is light.")

(defconst faces-all-version
  '(font-lock-comment-face
    font-lock-keyword-face
    font-lock-type-face
    font-lock-variable-name-face)
  "Faces known to all versions of Emacs with `font-lock-mode'.")

(defconst faces-version-20
  '(font-lock-builtin-face
    font-lock-function-name-face
    font-lock-string-face
    gdb-selection
    highlight
    region)
  "Faces introduced in Emacs v20.")

(defconst faces-version-20-2 '(font-lock-constant-face sh-heredoc)
  "Faces that only exist from Emacs 20.0 and were removed in 20.2.")

(defconst faces-version-21 '(font-lock-doc-face minibuffer-prompt)
  "Faces introduced in Emacs v21.")

(defconst faces-version-25 '(ebrowse-root-class whitespace-line)
  "Faces introduced in Emacs v25.")

(defun modify-font-lock-faces (faces-alist faces-to-modify)
  "Runs `modify-faces' on all FACES-TO-MODIFY using the FACES-ALIST.  It is safe
for a key to be specified in FACES-TO-MODIFY that is not present in the
FACES-ALIST.  In this case, the function ignores the key."

  (mapcar (lambda (face-to-modify)
            (let ((params-for-modify-face (assq face-to-modify faces-alist)))
              (unless (null params-for-modify-face)
                (apply 'modify-face params-for-modify-face))))
          faces-to-modify))

(defun update-emacs-font-lock-faces (faces-alist)
  "Given a color scheme defined by FACES-ALIST (see `bg-dark-faces' and
`bg-light-faces' for examples), update the colors used across Emacs."

  (modify-font-lock-faces faces-alist faces-all-version)

  (when (< 19 emacs-major-version)
    (modify-font-lock-faces faces-alist faces-version-20)
    (if (and (= 20 emacs-major-version) (< 2 emacs-minor-version))
        (modify-font-lock-faces faces-alist faces-version-20-2))
    (when (< 20 emacs-major-version)
      (modify-font-lock-faces faces-alist faces-version-21)
      (when (< 24 emacs-major-version)
        ;; Face will not take effect w/o loading the mode first.
        (require 'whitespace)
        (modify-font-lock-faces faces-alist faces-version-25)))))

;; -----------------------------------------------------------------------------
;; Font coloring for light Emacs backgrounds:
;; -----------------------------------------------------------------------------


;; -----------------------------------------------------------------------------
;; Functions used for command-line control:
;; -----------------------------------------------------------------------------

;; TODO: POP DOESN'T EXIST IN OLDER EMACS!!

(defun command-line-diff (switch)
  "Enter a graphical ediff from the command-line"

  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(defun command-line-instance-id (switch)
  "Allows us to 'name' our Emacs instance.

  This is not strict, nor does it need to be unique.  The main   purpose of
  this is for when I have more than one Emacs   environment running.  This way,
  I can put a friendly name in the title for easy identification."

  (let ((emacs-instance-id (pop command-line-args-left)))
    (set-emacs-title-format (if emacs-instance-id
                                (concat "(" emacs-instance-id ") %b")
                                "%b"))))

(defun command-line-merge (switch)
  "Enter a graphical ediff merge (with ancestor) from the command line"

  (let ((base (pop command-line-args-left))
        (sccs (pop command-line-args-left))
        (mine (pop command-line-args-left))
        (merg (pop command-line-args-left)))
    (ediff-merge-with-ancestor sccs mine base () merg)))

;; -----------------------------------------------------------------------------
;; Location-specific development environment configurations:
;; -----------------------------------------------------------------------------

(defun configure-ccs-dev-env ()
  "Look for Northeastern CCS Emacs environment and configure accordingly."

  ;; The CCS .emacs file defines this, but Emacs whines when byte-compiling.
  (eval-when-compile (defvar ps-printer-name))

  (load-file "/ccs/etc/dotfiles/.emacs")
  (setq inferior-lisp-program "acl")
  (setq printer-name          "escher")
  (setq ps-printer-name       "escher")

  ;; Set up my mail preferences
  (setq rmail-file-name       "~/mail/mbisson.ccs.neu.edu"))

(defun configure-palm-dev-env ()
  "Configure Emacs to develop for Palm Hollywood (Foleo)."

  (require 'gud)

  ;; xcscope-mode is conditionally included, so silence the errors for when it's
  ;; not present.
  (eval-when-compile (defvar cscope-database-file))

  (setq cscope-database-file "/home/mbisson/ws/src/cscope.out")
  (setq compile-command      "make -k -j3 -C /home/mbisson/ws/mail/main")
  (setenv "HM_DEBUG_LEVEL" "0")

  ;; Shell environment crap
  (if (string= (getenv "HOLLY_ARCH") "i386-linux")
      ;; We might need this for debugging (only on Intel simulator)...
      (progn
        (setenv "HX_APP_PATH"    (getenv "HOLLY_RESULT_ROOT"))
        (setenv "HM_SAMSDIR"     (concat (getenv "HOLLY_RESULT_ROOT")
                                         "/sams"))
        (setenv "LD_LIBRARY_PATH"
                (concat (getenv "LD_LIBRARY_PATH")                       ":"
                        (concat (getenv "HOLLY_SYSTEM_ROOT") "/usr/lib") ":"
                        (concat (getenv "HOLLY_RESULT_ROOT") "/fpi")     ":"
                        (concat (getenv "HOLLY_RESULT_ROOT") "/lib"))))

    ;; We're debugging on the device.
    (setq gud-gdb-command-name
          "/opt/holly/toolroot/arm-linux/bin/gdb --annotate=3")))

;; VMware's coding style is sufficiently unique to warrant its own C style
;; definition.
(c-add-style
 "vmware-c-c++-engineering-manual"
 '((c-basic-offset . 3)         ; Three-space indent
   (indent-tabs-mode . nil)     ; Use spaces instead of tabs
   (comment-style . extra-line) ; Use C-style comments even in C++-mode
   (comment-start . "/*") (comment-end . "*/")
   (c-comment-only-line-offset . 0)
   (c-hanging-braces-alist . ((substatement-open before after)))
   (c-offsets-alist
    . ((access-label         . -)
       (cpp-macro            . [0])
       (extern-lang-open     . 0)
       (inclass              . +)
       (inline-open          . 0)
       (inextern-lang        . 0)
       (innamespace          . 0)
       (label                . 0)
       (statement-case-open  . +)
       (statement-cont       . +)
       (substatement         . +)
       (substatement-open    . 0)
       (topmost-intro        . 0)
       (topmost-intro-cont   . 0)))))

(defun configure-vmware-dev-env ()
  "Configure Emacs to develop VMware code."

  (let ((vmware-style-hook
         (lambda () (c-set-style "vmware-c-c++-engineering-manual"))))
    (add-hook 'c-mode-hook   vmware-style-hook)
    (add-hook 'c++-mode-hook vmware-style-hook)
    (add-hook 'java-mode-hook
              (function (lambda ()(setq c-basic-offset 3)))))

  ;; We are only using a certain few compilers, so clean this up.
  (setq compilation-error-regexp-alist '(gcc-include gnu msft))
  (add-hook
   'compilation-filter-hook
   (lambda ()
     (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (let ((srcdir (getenv "RP_SRCDIR")))
    (if (and srcdir (file-exists-p srcdir))
        (setq compile-command
              (concat "cd " srcdir " && iscons PRODUCT=vsanrp vsanrp-all"))))

  (let ((srcdir (getenv "DR_SRCDIR_UNIX")))
    (if (and srcdir (file-exists-p srcdir))
        (setq compile-command (concat "make -C " srcdir " server")))))

(or (fboundp 'declare-function)
    ;; taken from Emacs 22.2, not present in 22.1:
    (defmacro declare-function (&rest args)
      "In Emacs 22, does nothing.  In 23, it will suppress byte-compiler
warnings.

This definition is so that packages may take advantage of the
Emacs 23 feature and still remain compatible with Emacs 22."
      nil))

;; TODO: This should do different things based on the prog-mode of the buffer.
(defun vmware-insert-file-header ()
  "Provides a boiler-plate C/C++ file header for VMware sources."

  (interactive)
  (insert (concat "\
/* *****************************************************************************
 * Copyright (c) " (int-to-string (nth 5 (decode-time)))
 " VMware, Inc.  All rights reserved. -- VMware Confidential.
 * ****************************************************************************/

/**
 * @file
 *    TODO: Brief description of this file.
 */
")))

;; -----------------------------------------------------------------------------
;; External package loaders:
;; -----------------------------------------------------------------------------

(defun provide-customized-features ()
  "Load external packages and those that are not enabled by default."

  (if (< 19 emacs-major-version) (provide-customized-features-20))

  ;; All this ccs stuff happens if the dotfile exists...
  (if (file-exists-p "/ccs/etc/dotfiles/.emacs")
      (configure-ccs-dev-env)
      ;; TODO: Illogical location for this...
      (setq inferior-lisp-program "sbcl --noinform"))

  ;; Set up my Palm development environment
  (if (getenv "USE_HOLLY") (configure-palm-dev-env))

  (if (getenv "VMWARE_CODE") (configure-vmware-dev-env)))

(defun provide-customized-features-20 ()
  "Load features that only work with Emacs 20 and above."

  (if (< 20 emacs-major-version) (provide-customized-features-21))

  ;; Enable wheelmouse support by default
  (if (not terminal-frame) (load "mwheel" t))

  ;; Load my MUD/MOO stuff
  (if (file-exists-p "~/elisp/mud.elc") (load-file "~/elisp/mud.elc"))

  ;; Load CSCOPE stuff
  (if (file-exists-p "~/elisp/xcscope.elc") (load-file "~/elisp/xcscope.elc"))

  ;; Load GTAGS stuff
  (if (file-exists-p "~/elisp/gtags.elc") (load-file "~/elisp/gtags.elc"))

  (if (file-exists-p "~/elisp/xgtags.elc")
      (progn
        ;; Silence compiler warnings when using a conditionally-included
        ;; function.
        (eval-when-compile (declare-function xgtags-mode (on)))

        (load-file "~/elisp/xgtags.elc")
        (add-hook    'c-mode-common-hook (lambda () (xgtags-mode 1)))
        (remove-hook 'c-mode-hook        'cscope:hook)
        (remove-hook 'c++-mode-hook      'cscope:hook)))

  ;; I customized SQL mode
  (if (file-exists-p "~/elisp/sql.elc") (load-file "~/elisp/sql.elc"))

  ;; Get fancy with wiki content
  (if (file-exists-p "~/elisp/media-wiki.elc")
      (load-file "~/elisp/media-wiki.elc"))

  ;; Get fancy with wiki content
  (if (file-exists-p "~/elisp/markdown-mode.elc")
      (load-file "~/elisp/markdown-mode.elc"))

  ;; Perforce is a horrible version control system -- it has an Emacs mode
  (if (file-exists-p "~/elisp/p4.elc") (load-file "~/elisp/p4.elc"))) ;; 2 slow

(defun provide-customized-features-21 ()
  "Load features that only work with Emacs 21 and above."

  (if (< 22 emacs-major-version) (provide-customized-features-23))

  ;; Load Batch-script stuff
  (if (file-exists-p "~/elisp/dos.elc") (load-file "~/elisp/dos.elc")))

(defun provide-customized-features-23 ()
  "Load features that only work with Emacs 23 and above."

  (if (< 23 emacs-major-version) (provide-customized-features-24))

  (if (file-exists-p "~/elisp/undo-tree.elc")
      (progn
        ;; Otherwise, Emacs complains on conditional inclusion:
        (eval-when-compile (declare-function global-undo-tree-mode ()))

        (load-file "~/elisp/undo-tree.elc")
        (global-undo-tree-mode))))

(defun provide-customized-features-24 ()
  "Load features that only work with Emacs 24 and above."

  (if (< 24 emacs-major-version) (provide-customized-features-25))

  (if (file-exists-p "~/elisp/clang-format.elc")
      (progn
        ;; Otherwise, Emacs complains on conditional inclusion:
        (eval-when-compile (defvar clang-format-executable))

        (load-file "~/elisp/clang-format.elc")
        (global-set-key (kbd "C-M-i") 'clang-format-region)
        (global-set-key (kbd "C-c d") 'clang-format)
        (setq clang-format-executable "~/bin/clang-format"))))

(defun provide-customized-features-25 ()
  "Load features that only work with Emacs 25 and above."
  (require 'ebrowse))

;; -----------------------------------------------------------------------------
;; Functions for keyboard macros:
;; -----------------------------------------------------------------------------

(defun set-key-bindings ()
  "Bind various useful function to various key sequences."

  (global-set-key "\M-+" 'insert-date)

  ;; I can't believe this isn't mapped by default, but...
  (global-set-key "\M-p" 'goto-line)

  (global-set-key [end]  'end-of-buffer)
  (global-set-key [home] 'beginning-of-buffer)

  (global-set-key "\C-c\C-r" 'recompile)

  ;; It's really annoying to have 10 windows open and fat-finger C-x 1, closing
  ;; them all.  Changing this behavior to ask for confirmation if there are four
  ;; windows or more.
  (global-set-key "\C-x1"
   (lambda (&optional confirmed)
     "Asks for confirmation before running `delete-other-windows' if there are
     four windows or more."
     (interactive (list (if (< 3 (count-windows))
                            (y-or-n-p "Really close all windows? ")
                          t)))
     (when confirmed (delete-other-windows))))

  ;; It helps to go backwards sometimes.
  (global-set-key "\C-xp"
   (lambda (&optional count)
     "Select another window in reverse cyclic ordering of windows."
     (interactive "p")
     (other-window (- (if (null count) 1 count)))))
  (global-set-key "\C-x9" 'delete-other-windows-vertically)

  (global-set-key "\C-c\C-v"
   (function (lambda ()
     "Copy the buffer's full path into the kill-ring."
     (interactive)
     (when buffer-file-name (kill-new (file-truename buffer-file-name))))))

  ;; Make sure there is no confusion about delete characters
  (if terminal-frame
      (set-key-bindings-term)
      (set-key-bindings-xwin)))

(defun set-key-bindings-term ()
  "Set key bindings to make Emacs work well on the terminal."

  ;; TODO: These used to be required, but now Emacs and the console have figured
  ;; out how to place nice.  Re-enable these (with an appropriate conditional)
  ;; when we encounter another terminal / keyboard issue.
; (global-set-key "\C-h" 'delete-backward-char)
; (global-set-key "\C-?" 'delete-char)
)

(defun set-key-bindings-xwin ()
  "Set key bindings that are specific to graphical Emacs instances."

  (global-set-key [delete]      'delete-char)
  (global-set-key [kp-delete]   'delete-char)
  (global-set-key [C-delete]    'kill-word)
  (global-set-key [C-kp-delete] 'kill-word)
  (global-set-key [C-tab]       'other-frame)
  (global-set-key [C-S-tab]     (lambda () (interactive) (other-frame -1))))

;; -----------------------------------------------------------------------------
;; Misc. hooks and functions:
;; -----------------------------------------------------------------------------

(defun associate-file-types ()
  "Set up associations between file types and Emacs major modes."

  ;; There are no hooks for fundamental mode...
  (setq
   default-major-mode 'text-mode
   initial-major-mode 'text-mode
   startup-major-mode 'text-mode)

  (if (member 'mediawiki features)
      (setq auto-mode-alist
            (append '(("\\.wiki$" . mediawiki-mode)) auto-mode-alist)))

  (setq
   auto-mode-alist
   (append
    (if (member 'dos features)
        '(("\\.bat$" . dos-mode) ("\\.cmd$" . dos-mode))
        '(("\\.bat$" . sh-mode)  ("\\.cmd$" . sh-mode)))
    auto-mode-alist))

  (setq
   auto-mode-alist
   (append '(("\\.C$"       . c++-mode)
             ("\\.cc$"      . c++-mode)
             ("\\.H$"       . c++-mode)
             ("\\.h$"       . c++-mode)
             ("\\.hpp$"     . c++-mode)
             ("\\.i$"       . c++-mode)
             ("\\.rc$"      . c++-mode)
             ("\\.sc$"      . python-mode)
             ("\\.r$"       . c-mode)
             ("\\.make$"    . makefile-mode)
             ("\\.mk$"      . makefile-mode)
             ("[Mm]akefile" . makefile-mode)
             ("\\.M$"       . nroff-mode)
             ("\\.ms$"      . nroff-mode)
             ("sendmail.cf" . sh-mode) ;; Don't ask...
             ("Doxyfile"    . sh-mode) ;; Don't ask...
             ("\\.bas$"     . basic-mode)
             ("\\.vb$"      . basic-mode)
             ("\\.vbs$"     . basic-mode)
             ("\\.csh$"     . sh-mode)
             ("\\.ksh$"     . sh-mode)
             ("\\.sh$"      . sh-mode)
             ("\\.txt$"     . text-mode))
           auto-mode-alist)))

(defun buffer-list-sorted-by-path ()
  "Gathers a list of buffers, ordered by their file name (see sort-buffers)."

  (sort (buffer-list)
        (function (lambda (o1 o2)
                    (let ((buf-file1 (buffer-file-name o1))
                          (buf-file2 (buffer-file-name o2)))
                      (cond
                       ((and (null buf-file1) (null buf-file2))
                        (string<
                         (downcase (buffer-name o1))
                         (downcase (buffer-name o2))))
                       ((null buf-file1) nil)
                       ((null buf-file2) t)
                       (t (string< buf-file1 buf-file2))))))))

(defun compat-font-exists-p (font-name)
  "Determines if a font exists by its name.  This function does so in a way that
is compatible with all versions of Emacs.  Before version 21, the font system
had a different set of APIs."

  (if (< 20 emacs-major-version)
      (find-font (font-spec :name font-name))
    (not (null (x-list-fonts font-name nil nil 1))))) ; Never actually fails :(

(defun custom-configure-backups (custom-backup-dir)
  "Configure the Emacs backup settings."

  (setq
   ;; Send backups to a local directory.  This must exist and be writable.
   backup-directory-alist (list (cons "." custom-backup-dir))

   ;; Enable backups by number instead of the default 'tilde' style..
   version-control   t

   ;; Silently delete old backups.  My thinking here is that I don't care if
   ;; backups get deleted because it's still better than the default, which only
   ;; saves the LAST version...
   delete-old-versions t

   ;; Keep this many of the the latest versions
   kept-new-versions 2

   ;; Leave this many of the oldest versions
   kept-old-versions 2))

(defun custom-configure-for-terminal ()
  "Configure setting that only apply to Emacs when run in a terminal."

  (menu-bar-mode -1)

  ;; I just find syntax highlighting annoying on the Windows terminal, so
  ;; disable it by default there.
  ;; TODO: XEmacs?!?
  (if (not running-xemacs)
      (global-font-lock-mode (if (eq system-type 'windows-nt) -1 1))))

(defun custom-configure-for-xwindows ()
  "Configure settings that only apply to Emacs when run in (X) Windows."

  (setq
   ;; Why was this turned off in Emacs 24.1?
   mouse-drag-copy-region       t

   ;; Quite an assumption, but there's really no uniform way to tell (even the
   ;; Emacs docs say this).
   focus-follows-mouse          (or (eq system-type 'windows-nt)
                                    (eq system-type 'darwin))

   ;; Let's set up some universal window (frame) attributes.
   default-frame-alist
   (cons
    (cons 'cursor-type 'box)

    (cond
     ((eq system-type 'windows-nt)
      (list
       ;; Preserve the background at configuration time for future frames.
       ;; Because there's no .Xdefaults, we rely on command-line arguments.
       (cons 'background-color
             (frame-parameter (selected-frame) 'background-color))

       ;; Determine the font we can use somewhat dynamically by falling through
       ;; until we find one that exists on our system.
       (cons 'font
             (find-first-defined-font
              "-*-Courier New-*-*-*-*-11-*-*-*-c-*-iso8859-1"
              '(;; Consolas 11
                "-*-Consolas-normal-r-*-*-12-90-*-*-c-*-iso8859-1"
                ;; Liberation Mono 9
                "-*-Liberation Mono-*-*-*-*-12-*-*-*-c-*-iso8859-1"
                ;; Lucida Console 8 (thinner)
                "-*-Lucida Console-*-*-*-*-11-*-*-*-c-*-iso8859-1")))
       (cons 'height 70)
       (cons 'width  81)))
     ((eq system-type 'darwin)
      (list
       (cons 'font
             (find-first-defined-font
              "Menlo 12"
              '("DejaVu Sans Mono 9")))
       (cons 'height 50)
       (cons 'width  81)))
     (t
      (list
       (if (eq system-type 'cygwin)
           (cons 'font (find-first-defined-font "8x13" '("Consolas 9")) )
         (cons 'font
               (find-first-defined-font
                "8x13"
                '("DejaVu Sans Mono 9"; 10"
                  "FreeMono 10"
                  "Nimbus Mono L 10"
                  "-Misc-Fixed-normal-normal-normal-*-13-*-*-*-c-*-iso10646-1"))
               ))
       (cons 'height (frame-parameter (selected-frame) 'height))
       (cons 'width  (frame-parameter (selected-frame) 'width)))))))

  ;; Print the name of the visited file in the title of the window...
  (set-emacs-title-format "%b")

  ;; Emacs doesn't properly set the cursor/mouse color for dark backgrounds
  ;; unless the background is pure black.
  (let ((frame-params (frame-parameters)))
    (if (eq 'dark (cdr (assq 'background-mode frame-params)))
        (let ((fg-color (cdr (assq 'foreground-color frame-params))))
          (set-cursor-color fg-color)
          (set-mouse-color  fg-color))))

  ;; Can't customize font lock until we load the libary (and faces) first
  (load-library "font-lock")
  (customize-font-lock))

(defun customize-font-lock ()
  "Set up syntax highlighting."

  ;; Add color to the current GUD line
  (make-face 'gdb-selection)

  ;; By default, this face is UNREADABLE.
  (if (or (and (= 20 emacs-major-version) (< 2 emacs-minor-version))
          (< 20 emacs-major-version))
      (make-face 'sh-heredoc)
      (make-face 'font-lock-doc-face))

  ;; Go all out with the font colors
  (setq font-lock-maximum-decoration t)

  ;; Syntax highlighting
  ;; TODO: XEmacs?!?
  (if (not running-xemacs) (global-font-lock-mode 1))

  ;; Give me some nice pretty colors...
  (if (not terminal-frame)
      (update-emacs-font-lock-faces
       (if (light-background-p) bg-light-faces bg-dark-faces))))

(defun find-first-defined-font (default-font-name font-names)
  "Searches the provided list of font name (strings), returning the name of the
first font that Emacs can find on this system.  If it finds no fonts, it uses
the default-font-name."

  (if (null font-names) default-font-name

    (let ((font-name       (car font-names))
          (rest-font-names (cdr font-names)))
      (if (compat-font-exists-p font-name) font-name ;; <- Found it.
        (if (null rest-font-names) default-font-name ;; <- Found nothing...
          ;; Keep searching...
          (find-first-defined-font default-font-name rest-font-names))))))

(defun gud-kill-buffer ()
  "Get rid of the GDB highlight.  This should be added as a hook for when the
GDB buffer goes away."

  (if (eq major-mode 'gud-mode) (delete-overlay gud-overlay)))

(defun insert-date (&optional arg)
  "This function exists because I want to map M-+ so that it enters a time into
my file."

  (interactive "P")
  (insert (format-time-string "%A, %e %B %Y" (current-time))))

(defun light-background-p ()
  "Determines if Emacs considers the background color to be 'light'."

  (eq 'light
      (cdr (assq 'background-mode (frame-parameters (selected-frame))))))

(defun set-emacs-title-format (title-format)
  "Set the title for Emacs frames (iconized or not)."

  (setq frame-title-format title-format
        icon-title-format  title-format))

(defun set-active-frame-width-for-parallel-windows (window-count)
  "Set the width of the frame to allow WINDOW-COUNT parallel windows to have a
uniform 80 column width"

  (interactive "nEnter desired width (in 80 column windows): ")
  (when (> 1 window-count) (user-error "Window count less than 1"))
  ; Note: ignoring "too many windows" problems for now.

  (set-frame-size
   (selected-frame)
   (+ (* 80 window-count)           ; Column count could be configurable...
      window-count                  ; One extra letter so cursor sits on the end
      (* 5 (- window-count 1)))     ; Ballpark scroll-bar width
   (frame-parameter (selected-frame) 'height))
  (balance-windows))

(defun sort-buffers ()
  "Re-order the buffers alphabetically by their path."

  (interactive)
  (dolist (cur (buffer-list-sorted-by-path))
    (bury-buffer cur))
  (if (called-interactively-p 'interactive) (list-buffers)))

(defun term-named (program name-prefix)
  "Start a terminal-emulator in a new buffer, prefixed by
NAME-PREFIX.  The buffer is in Term mode; see `term-mode' for the
commands to use in that buffer.

\\<term-raw-map>Type \\[switch-to-buffer] to switch to another buffer."
  (interactive (list (read-from-minibuffer "Run program: "
                                           (or explicit-shell-file-name
                                               (getenv "ESHELL")
                                               (getenv "SHELL")
                                               "/bin/sh"))
                     (read-from-minibuffer "Prefix (may be empty): ")))
  (let ((term-name
         (if (string= "" name-prefix)
             "terminal"
           (format "%s-terminal" name-prefix))))
    (set-buffer (make-term term-name program))
    (term-mode)
    (term-char-mode)
    (switch-to-buffer (format "*%s*" term-name))))

;; -----------------------------------------------------------------------------
;; Global variables:
;; -----------------------------------------------------------------------------

(defvar gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'gdb-selection)
    ov)

  "Overlay variable for GUD highlighting."
)

;; Are we running XEmacs or Emacs?
;; TODO Test against XEmacs.  This will be a problem...
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "Evaluates to t if this is the (obviously inferior) XEmacs."
)

;; TODO: Why bother?
(defalias 'run-lisp 'inferior-lisp)

;; -----------------------------------------------------------------------------
;; "Advice" for existing functions:
;; -----------------------------------------------------------------------------

(defadvice gud-display-line (after my-gud-highlight act)
  "Highlight current line."

  (let* ((ov gud-overlay)
         (bf (gud-find-file true-file)))
    (save-excursion
      (set-buffer bf)
      (move-overlay ov
                    (line-beginning-position)
                    (+ (line-end-position) 1)
                    (current-buffer)))))

;; -----------------------------------------------------------------------------
;; GO CONFIGURE!!
;; -----------------------------------------------------------------------------

(custom-configure-emacs)
