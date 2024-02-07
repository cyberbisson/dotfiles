;;; emacs.el --------- My Emacs initialization script -*- lexical-binding: t -*-

;; Matt Bisson <mbisson@ccs.neu.edu>
;; Homepage:		https://cyberbisson.com/
;; Keywords:		initialization
;; Last Major Edit:	12/10/2023 (MM/DD/YYYY)

;;; Commentary:

;; This is my Emacs initialization script.  It works with Emacs 19-26 and
;; greater.  Compile this file to get the most benefit.
;;
;; Features:
;;
;; - Fully compatible with Emacs v19 to current, as well as XEmacs.  All
;;   compatibility extends to X, Windows, MacOS, and the terminal.
;; - Aims to maintains basic text editing behavior of Emacs since v19.
;; - Minimal startup fuss (e.g., no text in *scratch*, no start-up message).
;; - More modeline info (e.g., day of week, battery %).
;; - --merge and --diff command-line options (suitable for Perforce, et al.).
;; - Automatic load of desktop when $EMACS_SERVER_FILE is defined in the env.
;; - Versioned backup away from local directory when ~/.emacs.bak exists.
;; - --instance-id command line gives Emacs a name (good for keeping track).
;; - Different customized syntax coloration based on dark or light background.
;;   The background can be different with different frames, and still receive
;;   the correct treatment.
;;
;; Design Notes:
;;
;; - This file is huge.  This is intentional.  My Emacs occasionally runs on
;;   computers across the continental US, and accessing multiple files over a
;;   slow NFS filesystem was causing noticeable delays.
;;
;; - There should be no explicit loading for modules solely for the purpose of
;;   customizing variables or faces.  This pollutes an Emacs with no buffers by
;;   loading all sorts of modes that may never be used during execution.
;;   Instead, respond to `eval-after-load' for given modules.  This has the
;;   added benefit of not requiring version checks, because the module simply
;;   won't be loaded, and incompatible code won't run.  The flip-side of this is
;;   that a module rename will cause customizations to mysteriously no longer
;;   happen.  Note: be careful when registering hooks in `eval-after-load'
;;   bodies, because it might be after the first mode's hook has run.  For
;;   `cc-mode' specifically, we're using `c-initialization-hook' to solve this
;;   problem.
;;
;; - This file should only use `declare-function' or `defvar' to stifle warnings
;;   from other modules when those modules are loaded explicitly, and they are
;;   not available in the source code repo (e.g., a file loaded from
;;   "/ccs/etc/dotfiles" is not going to be available outside of the
;;   Northeastern CCS network).
;;
;; - When compiling, this file should be able to access all the "required"
;;   packages -- particularly those in my own repo.  All those files need not be
;;   loaded in a running Emacs, but they must be available for compilation.

;;; To-do:

;; - Can `font-lock-function' customization optimize font-lock coloration?
;; - Illogical location to set `inferior-lisp-program' (after `file-exists-p').
;; - Consolidate various background-color detection functionality.

;;; Code:

;; Set this to T to see what gets loaded (and when) in the *Messages* buffer.
;; It is located first for obvious reasons.
;;;(setq force-load-messages t)

;; This is actually interesting on older Emacs to see what exactly doesn't ever
;; get used.
;;;(eval-when-compile (setq byte-compile-generate-call-tree t))

;; Tell the byte-compiler to log everything it's modifying.  This can be
;; interesting, but also make compilation warnings harder to see.
;;;(eval-when-compile (setq byte-optimize-log t))

;; Put this early in the file to control the behavior of the Macros section.
(eval-and-compile
  (defvar lock-dotfile-version t
    "Determines if the configuration should `lock' to a particular Emacs version
by compiling version checks out of existence.

If NIL, the configuration will dynamically check what features may run by
examining the Emacs major version.  If T, branches will be removed during
compilation, and the configuration will only be valid for the Emacs major
version on which it was compiled."))

;; -----------------------------------------------------------------------------
;; Macros:
;;
;; This should be first.  As macros are evaluated lazily, placing them lower in
;; the file has little benefit, but these macros help optimize compiled code, so
;; they should be as early in the file as possible.  We use macros like
;; `unless-running-xemacs' to simply remove if statements (and the code from one
;; of the branches) when we know at compile-time that a value will never change
;; (i.e., we're either compiling for XEmacs or GNU Emacs).
;; -----------------------------------------------------------------------------

;; These macros optimize compilation, so they only need to exist there.  If you
;; edit this file, you may wish to start Emacs with "-q -l emacs.el", because it
;; helps indentation and syntax highlighting work better...
;;
;; On older Emacs versions, the `eval' part of `eval-when-compile' doesn't
;; happen until the statement completes, so things break when you have a macro
;; based on a macro from the same `eval-when-compile' statement.  Consequently,
;; we have one statement that injects definitions into the compilation
;; environment for the second statement.
(eval-when-compile

;; Provide older versions of Emacs with a `declare' macro.  This should only be
;; required for Emacs versions prior to 21.
(unless (fboundp 'declare) (require 'cl))

;;(setq byte-compile-call-tree t)
;; This is outside the `eval-when-compile' block because it seems older Emacs
;; cannot evaluate nested macros.
(defmacro running-xemacs-p ()
  "Test if this running instance is XEmacs."
  (if (boundp 'running-xemacs)
      running-xemacs
    (string-match "XEmacs\\|Lucid" emacs-version)))

) ;; END eval-when-compile

;; Part II: See the eval-when-compile above.
(eval-when-compile

;;
;; Are we running XEmacs or GNU Emacs?
;;

(defmacro if-running-xemacs (then &rest else)
  "If this is XEmacs, execute THEN, otherwise run the ELSE body."
  (declare (indent 0) (debug t))
  (if (running-xemacs-p) then (cons 'progn else)))

(defmacro unless-running-xemacs (&rest body)
  "Execute BODY unless this is XEmacs."
  (declare (indent 0) (debug t))
  (if (not (running-xemacs-p)) (cons 'progn body) nil))

(defmacro when-running-xemacs (&rest body)
  "Execute BODY if this is XEmacs."
  (declare (indent 0) (debug t))
  (if (running-xemacs-p) (cons 'progn body) nil))

;;
;; Conditional versioning optimizations (controlled by `lock-dotfile-version').
;;

(defmacro get-compiled-dotfile-version ()
  "Return the version the configuration was compiled against.  This is NIL if
`lock-dotfile-version' is NIL."
  (if lock-dotfile-version emacs-major-version nil))

(defmacro version-if (cond then &rest else)
  "If COND yields non-nil, do THEN, else do ELSE...

This function is specifically designed for Emacs version checks.  This interacts
with `lock-dotfile-version' to either behave like a normal `if' statement, or to
completely evaluate the test at compile time."
  (declare (indent 1) (debug t))
  (if lock-dotfile-version
      ;; To optimize compiled Emacs Lisp to only the file for which this was
      ;; compiled, use the following form:
      (if (eval cond) then `(progn ,@else))
    ;; To enable multiple versions of Emacs from a single compiled file, use
    ;; the following form:
    `(if ,cond ,then ,@else)))

(defmacro version-when (cond &rest body)
  "Execute BODY based on COND.

Depending on the value of `lock-dotfile-version', COND may be evaluated at
compile time."
  (declare (indent 1) (debug t))
  `(version-if ,cond (progn ,@body) nil))

) ;; END eval-when-compile

(defmacro terminal-frame-p (&optional frame)
  "Determine if FRAME shows on a terminal (and not a window manager).  If FRAME
is not specified, this uses the currently selected frame."

  ;; NOTE: The display type is only *sometimes* encoded in the frame.  Most
  ;; notably, when I start Emacs in a terminal, then create a new GUI frame, it
  ;; sets the display type incorrectly.  The `tty-type' function seems to work
  ;; all the time.
  `(not (null (compat-tty-type ,(or frame '(selected-frame))))))

(defmacro thin-thickness-p ()
  "Return T if the Motif `enableThinThickness' resource has been set to `true'."
  `(and (boundp 'motif-version-string)
        x-initialized
        (string= "true" (x-get-resource "enableThinThickness" "Emacs"))))

;; -----------------------------------------------------------------------------
;; Compatibility functions:
;;
;; Keep this section small.  These functions should be simple conversions from
;; one dialect of Emacs to another (e.g., GNU Emacs to XEmacs), or to cover up
;; differences between basic functionality in different Emacs versions (e.g.,
;; `called-interactively-p' has a different number of parameters in Emacs v24).
;; -----------------------------------------------------------------------------

;; Silly kludge to support `alist-get' in older Emacs.  This is probably better
;; off as a macro (TODO).
(version-if (and (> 25 emacs-major-version) (not (functionp 'alist-get)))
    (defun alist-get (key alist &optional default remove)
      "Return the value associated with KEY in ALIST, using `assq'.
If KEY is not found in ALIST, return DEFAULT.

This is a generalized variable suitable for use with `setf'.  When using it to
set a value, optional argument REMOVE non-nil means to remove KEY from ALIST if
the new value is `eql' to DEFAULT."
      (let ((x (assq key alist)))
        (if x (cdr x) default))))

(defmacro compat-called-interactively-p ()
  "Replace `called-interactively-p' with a function that take no parameters.

Prior to Emacs 23.2, `called-interactively-p' took no parameters, but they made
a backwards-incompatible change to add a parameter, so this macro exists for
compatibility purposes."
  (version-if (or (and (= 23 emacs-major-version) (< 1 emacs-minor-version))
                  (< 23 emacs-major-version))
    '(called-interactively-p 'interactive)
    '(called-interactively-p)))

(defun compat-display-color-cells (frame)
  "Return the number of colors (color cells) that FRAME supports.

This function does so in a way that is compatible with all versions of Emacs.
Before version 21, the font system had a different set of APIs."

  (if-running-xemacs
    (or (device-color-cells frame) 16)
    (version-if (< 20 emacs-major-version)
      (display-color-cells frame)
      ;; `x-defined-colors' does not exist in Emacs 20 on the Windows console.
      (if (fboundp #'x-defined-colors)
          (length (x-defined-colors frame)) ; Very approximate...
        16))))

(defun compat-font-exists-p (font-name &optional frame)
  "Determine if a font (given by FONT-NAME) exists by its name on FRAME.

This function does so in a way that is compatible with all versions of Emacs.
Before version 22, the font system had a different set of APIs.

This uses the specified FRAME, or the current frame, if none was specified."

  (version-if (< 22 emacs-major-version)
    (find-font (font-spec :name font-name) frame)
    (not (null
          (if-running-xemacs
              (list-fonts font-name frame)
            (x-list-fonts font-name nil frame 1)))))) ; Never actually fails :(

(version-if (not (fboundp 'daemonp))
  (defmacro daemonp ()
    "Always return nil.  Emacs as a daemon does not exist until Emacs 23.1, so
this is a compatibility function."
    nil))

(version-if (not (fboundp 'declare-function))
  ;; Taken from Emacs 22.2, not present in 22.1:
  (defmacro declare-function (&rest _args)
    "In Emacs 22, does nothing.  In 23, it will suppress byte-compiler warnings.

This definition is so that packages may take advantage of the Emacs 23 feature
and still remain compatible with Emacs 22."
    nil))

;; XEmacs has a different name for these, but the same meaning.
(when-running-xemacs
  (defalias 'frame-parameter #'frame-property)
  (defalias 'global-font-lock-mode #'font-lock-mode))

;; `mapc' is a built-in function only starting with Emacs 21, and we use it
;; extensively in this file.
(version-if (and (> 21 emacs-major-version) (< 27 emacs-major-version))
  (require 'cl))

(defmacro compat-mwheel-install ()
  "Enable the mouse wheel.  The name of this change in version 27 Emacs, and it
may also not be present if it was not added at compilation time."
  (version-if (< 26 emacs-major-version)
    ;; NOTE: I'm not using version-if in 27+ (and therefore not compiling out
    ;; the check) because of a corner case where `mouse-wheel-mode' is defined
    ;; for Windows Emacs, but not for the Cygwin Emacs (terminal-only).  By
    ;; contrast, `mwheel-mode' seemd to be present always.
    '(if (fboundp 'mouse-wheel-mode) (mouse-wheel-mode 1))
    (version-if (fboundp 'mwheel-install) '(mwheel-install))))

;; These functions are obsolete in Emacs v24.3 and 24.4.
(unless-running-xemacs
  (version-if (not (fboundp 'set-face-bold))
    (defalias 'set-face-bold #'set-face-bold-p))
  (version-if (not (fboundp 'set-face-italic))
    (defalias 'set-face-italic #'set-face-italic-p)))
(version-if (not (fboundp 'set-face-underline))
  (defalias 'set-face-underline #'set-face-underline-p))

(unless-running-xemacs
  (defmacro set-specifier (&rest _args)
    "Ignore extra configuration functions from XEmacs when in GNU Emacs."
    nil))

(defmacro compat-tool-bar-mode (arg)
  "Enable the tool-bar if ARG is positive; disable it otherwise."
  (if-running-xemacs
    `(set-specifier default-toolbar-visible-p ,(> arg 0))
    ;; See the Windows/Cygwin note in `compat-mwheel-install' for why the
    ;; following line is not the ELSE case here:
    ;;
    ;; (version-if (fboundp 'tool-bar-mode) `(tool-bar-mode ,arg) arg)))
    `(if (fboundp 'tool-bar-mode) (tool-bar-mode ,arg))))

(defmacro compat-tty-type (&optional terminal)
  "Return the type of TTY device that TERMINAL uses.  Returns nil if TERMINAL is
not on a TTY device.

This function adds functionality that was not present in Emacs versions prior to
23.1."
  (version-if (< 22 emacs-major-version)
    `(tty-type ,terminal)
    ;; This just happens to be a clue in Emacs 22 and before (XEmacs as well).
    `(null (frame-parameter ,terminal 'window-id))))

(unless-running-xemacs
  (version-if (> 22 emacs-major-version)
    (defun warn (fmt-message &rest args)
      "Display a warning message as an error.

This generates the message with `format', using FMT-MESSAGE and ARGS.  Warnings
were not introduced until Emacs 22."
      (error (apply #'format fmt-message args)))))

;; -----------------------------------------------------------------------------
;; Compile-time "Requirements":
;;
;; We list the dependencies up front for compilation.  At run time, everything
;; should be loaded lazily.
;; -----------------------------------------------------------------------------

(unless-running-xemacs
  (eval-when-compile
    (require 'bytecomp)

    (let* ((this-file-name (or load-file-name ;; One of these will work...
                               byte-compile-current-file
                               byte-compile-dest-file
                               buffer-file-name))
           ;; A heuristic to determine if this emacs.el is separated from the
           ;; rest of my added packages (that will be required below).  If there
           ;; is 1 or fewer files named .el (assuming emacs.el is one of them),
           ;; this is a "stand-alone" file.
           (stand-alone (>= 1 (length (directory-files
                                       (file-name-directory this-file-name)
                                       t
                                       "^[^.].+\.el$")))))

      ;; When compiling, make sure that `require' finds all the Emacs modules
      ;; next to 'emacs.el'.  They may or may not make it into the `load-path'
      ;; for a running Emacs session.
      (unless stand-alone
        (add-to-list 'load-path (file-name-directory this-file-name)))

      (require 'battery)
      (require 'cc-vars)
      (require 'compile)
      (require 'desktop)
      (require 'dired)
      (require 'ediff)
      (require 'inf-lisp)
      (require 'man)
      (require 'server)
      (require 'term)
      (require 'time)

      (when (< 20 emacs-major-version)
        (require 'cc-fonts)
        (require 'emacs)
        (require 'newcomment)
        ;; Legacy: replaced with `display-battery-mode'.
        (declare-function display-battery ()))
      (when (< 21 emacs-major-version)
        (unless stand-alone (require 'undo-tree))
        (require 'whitespace))
      (when (< 22 emacs-major-version)
        (unless stand-alone (require 'gtags))
        (require 'org)
        (require 'org-clock)
        (unless stand-alone (require 'xgtags)))
      (when (< 23 emacs-major-version)
        (require 'gdb-mi)
        (unless stand-alone
          (require 'clang-format)
          (require 'vmw-c-dev)))
      (when (< 24 emacs-major-version)
        (require 'ox-html)))))

;; -----------------------------------------------------------------------------
;; Global constants:
;; -----------------------------------------------------------------------------

(defconst compiled-dotfile-version
  (get-compiled-dotfile-version)
  "If `lock-dotfile-version' is T, this reports the version that is locked.")

(defconst custom-loaddefs-file "~/elisp/cust-loaddefs.el"
  "This is the file containing generated autoload content for my own custom set
of modules.")

(defconst daemon-frame (if (daemonp) terminal-frame)
  "The pseudo-frame used by Emacs daemon.  This frame is not a real frame,
but (if nothing else) the daemon process uses it to keep Emacs alive when all
other frames have closed.")

(defconst ideal-window-columns 80
  "I think all source code should be 80 columns, and that's how large I like my
windows.")

(defconst running-xemacs (running-xemacs-p)
  "Evaluates to t if this is the (obviously inferior) XEmacs.")

(defconst scroll-bar-fudge (if (thin-thickness-p) 4 5)
  "The amount of space between graphical windows on a frame.  GUI scroll bars
are measured in text columns, and different GUI systems have different
measurements.")

;; -----------------------------------------------------------------------------
;; Global variables:
;; -----------------------------------------------------------------------------

(defvar custom-environments '()
  "This contains a list of symbols to identify what customized environments are
used in this Emacs session.  For instance, `\\='(palm vmware)' indicates that
both Palm and VMware development modules should be present.

The function `provide-customized-features' populates this structure, and it can
be utilized any time after that.")

(defvar gud-overlay nil
  "Overlay variable for GUD highlighting.  It is created lazily.")

(defconst known-elisp-files
  '("~/elisp/clang-format.el"
    "~/elisp/dos.el"
    "~/elisp/gtags.el"
    "~/elisp/markdown-mode.el"
    "~/elisp/media-wiki.el"
    "~/elisp/mud.el"
    "~/elisp/p4.el"
    "~/elisp/protobuf-mode.el"
    "~/elisp/sql.el"
    "~/elisp/undo-tree.el"
    "~/elisp/visual-basic-mode.el"
    "~/elisp/vmw-c-dev.el"
    "~/elisp/xcscope.el"
    "~/elisp/xgtags.el")
  "The list of Emacs modules that will be searched for autoload directives.

I do not search for more than this because I want to keep pretty tight control
on what gets loaded into my Emacs.  Any other files not in this list are
probably something I'm playing with, and will have to be dealt with manually for
the time being.")

;; This variable exists so we can alter the title format without caring what it
;; currently is.  Every alteration of this must be followed by
;; `set-emacs-title-format' to have any effect.  This seems a bit redundant, but
;; something needs to set the title at least once (so we need the function), and
;; not having the variable means we need a "get" function.
(defvar emacs-title-format "%b"
  "Used to set the title for Emacs frames (iconized or not).  See
`set-emacs-title-format' for details.")

(defvar last-restored-frameset nil
  "The last frameset restored by a `desktop-read'.  This will be consulted by
functions like `restore-last-desktop-frameset'")

(defvar org-i-died-clock-timer nil
  "A timer that executes after a period of time that means I died while clocked
in.")

(defvar server-title-mark (if (daemonp) " <%N>" " [%N]")
  "This text will be added to frame titles when Emacs runs in `server-mode'.

The string is printed verbatim, except if it contains a formatter string.  Aside
from those available to `frame-title-format', this function supports %N, which
displays the `server-name' in the title.")

;; =========================================================
;; Font coloring configuration:
;; =========================================================

;; Note that the symbol `ign' directs the `modify-face-compat' function to take
;; no action with a given face setting.  T or NIL explicitly enables or disables
;; a setting, so if you wish to leave it alone (e.g., `region' should not stomp
;; in the italics of the underlying face), specify "ign".

(defvar bg-dark-faces
  ;; face                         fg              bg  b   i   u
  '((font-lock-builtin-face       "LightSalmon"   nil t   nil nil)
    (font-lock-comment-face       "PaleTurquoise" nil nil t   nil)
    (font-lock-constant-face      "Aquamarine2"   nil nil nil nil)
    (font-lock-doc-face           "LightBlue"     nil nil t   nil)
    (font-lock-function-name-face "Aquamarine"    nil nil t   nil)
    (font-lock-keyword-face       "IndianRed"     nil t   nil nil)
    (font-lock-string-face        "LightSkyBlue"  nil nil nil nil)
    (font-lock-type-face          "Violet"        nil nil nil nil)
    (font-lock-variable-name-face "Turquoise"     nil nil nil nil)
    (minibuffer-prompt            "DodgerBlue"    nil t   nil nil)
    (sh-heredoc                   "Chocolate"     nil nil nil nil)

    ;; Changing the background here:
    (highlight                    ign "CadetBlue" ign ign ign)
    (region                       ign "Firebrick" ign ign ign)

    ;; Setting this is "optional," but if you start emacs (daemon) on a light
    ;; background, clients started on a dark background unfortunately take on
    ;; the light color scheme, which is very difficult to tell apart.
    (mode-line                     "Black"  "Grey75" ign ign ign)
    (mode-line-inactive            "Grey80" "Grey30" ign ign ign))
  "The complete set of `font-lock-mode' faces for Emacs used when the background
is dark.")

(defvar bg-light-faces
  ;; face                         fg              bg  b   i   u
  '((font-lock-builtin-face       "DodgerBlue4"   nil t   nil nil)
    (font-lock-comment-face       "DarkGreen"     nil nil t   nil)
    (font-lock-constant-face      "CadetBlue"     nil nil nil nil)
    (font-lock-doc-face           "Green4"        nil nil t   nil)
    (font-lock-function-name-face "OrangeRed2"    nil nil t   nil)
    (font-lock-keyword-face       "FireBrick"     nil t   nil nil)
    (font-lock-string-face        "Chocolate"     nil nil nil nil)
    (font-lock-type-face          "Maroon"        nil nil nil nil)
    (font-lock-variable-name-face "SteelBlue"     nil nil nil nil)
    (minibuffer-prompt            "DodgerBlue4"   nil t   nil nil)
    (sh-heredoc                   "Chocolate"     nil nil nil nil)

    ;; Changing the background here:
    (highlight                     ign "CadetBlue"       ign ign ign)
    (region                        ign "LightSteelBlue3" ign ign ign)

    ;; On light colored terminals, the mode-line can sometimes be annoyingly
    ;; similar between active and inactive windows.
    (mode-line                     "Black"  "Grey70" ign ign ign)
    (mode-line-inactive            "Grey40" "Grey80" ign ign ign))
  "The complete set of `font-lock-mode' faces for Emacs used when the background
is light.")

;; On dark colored terminals, the diff colors are purple and teal, which is
;; horrible.  They look fine on X, but it's so bad on the terminal, I'm changing
;; it.  In some cases, the terminal supports colors that look good, but there's
;; no "name" for it, so I'm using just the hex value.
(defconst diff-faces
  '((dark . ((diff-added   ign "DarkGreen" ign ign ign)
             (diff-removed ign "#5f0000"   ign ign ign))))
  "Custom faces for `diff-mode'.  These are modified lazily.")

(defconst ediff-faces
  '((dark
     . ((ediff-current-diff-A         ign "#5f0000"   ign ign ign)
        (ediff-current-diff-B         ign "DarkGreen" ign ign ign)
        (ediff-current-diff-Ancestor  ign "DarkGreen" ign ign ign))))
  "Custom faces for `ediff-mode'.  These are modified lazily.")

(defconst ebrowse-faces
  '((dark  . ((ebrowse-root-class "Violet" nil nil nil nil)))
    (light . ((ebrowse-root-class "Maroon" nil nil nil nil))))
  "Custom faces for `ebrowse-mode'.  These are modified lazily.")

(defconst gud-faces
  '((dark  . ((gdb-selection ign "MidnightBlue"  ign ign ign)))
    (light . ((gdb-selection ign "DarkSeaGreen3" ign ign ign))))
  "Custom faces for `gud-mode'.  These are modified lazily.")

(defconst org-faces
  '((dark
     . ((org-ellipsis "LightGoldenrod" nil t   nil nil)
        (org-level-1  "Aquamarine2"    nil nil nil nil)
        (org-level-2  "LightCoral"     nil nil nil nil)
        (org-level-3  "LightSkyBlue"   nil nil nil nil)
        (org-level-4  "LightSalmon"    nil nil nil nil)
        (org-level-5  "PaleTurquoise"  nil nil nil nil)
        (org-level-6  "Violet"         nil nil nil nil)
        (org-level-7  "Turquoise"      nil nil nil nil)
        (org-level-8  "Pink"           nil nil nil nil)))
    (light
     . ((org-ellipsis "DarkSlateBlue"  nil t   nil nil)
        (org-level-1  "OrangeRed2"     nil nil nil nil)
        (org-level-2  "DodgerBlue4"    nil nil nil nil)
        (org-level-3  "FireBrick"      nil nil nil nil)
        (org-level-4  "DarkGreen"      nil nil nil nil)
        (org-level-5  "Maroon"         nil nil nil nil)
        (org-level-6  "CadetBlue"      nil nil nil nil)
        (org-level-7  "Chocolate"      nil nil nil nil)
        (org-level-8  "Red"            nil nil nil nil))))
  "Custom faces for `org-mode' buffers.")

;; Without changing blue, certain terminal output is unreadable in a dark
;; background.
(defconst ansi-color-faces
  '((dark
    . ((ansi-color-blue        "RoyalBlue"      "RoyalBlue"      ign ign ign)
       (ansi-color-bright-blue "CornflowerBlue" "CornflowerBlue" ign ign ign))))
  "Custom faces for `term-mode' buffers.")

(defconst whitespace-faces
  '((light . ((whitespace-line "Red1" nil t nil nil))))
  "Custom faces for `whitespace-mode'.  These are modified lazily.")

(defconst faces-all-versions
  '(font-lock-comment-face
    font-lock-keyword-face
    font-lock-type-face
    font-lock-variable-name-face)
  "Faces known to all versions of Emacs with `font-lock-mode'.")

(defconst faces-version-20
  `(font-lock-builtin-face
    font-lock-function-name-face
    font-lock-string-face
    gdb-selection
    highlight
    ,(unless-running-xemacs 'region))
  "Faces introduced in Emacs v20.")

(defconst faces-version-20-2 '(font-lock-constant-face sh-heredoc)
  "Faces that only exist from Emacs 20.0 and were removed in 20.2.")

(defconst faces-version-21
  ;; Note: mode-line was "modeline" in Emacs 20, and there was no "inactive."
  ;; Not worth bothering with it, since an inverse color modeline is just fine
  ;; there.
  `(diff-added
    diff-removed
    ediff-current-diff-A
    ediff-current-diff-Ancestor
    ediff-current-diff-B
    ,@(unless-running-xemacs
        '(font-lock-doc-face
          minibuffer-prompt
          mode-line
          mode-line-inactive)))
  "Faces introduced in Emacs v21.")

(defconst faces-version-22
  '(org-ellipsis
    ;; All the structural faces for `org-mode':
    org-level-1 org-level-2 org-level-3 org-level-4 org-level-5 org-level-6
    org-level-7 org-level-8)
  "Faces introduced in Emacs v22.")

(defconst faces-version-25
  '(ebrowse-root-class font-lock-constant-face whitespace-line)
  "Faces introduced in Emacs v25.")

(defconst faces-version-28
  '(ansi-color-bright-blue ansi-color-blue)
  "Faces introduced in Emacs v28.")

;; -----------------------------------------------------------------------------
;; External package loaders:
;;
;; All `load' and `require' functions should be called within this grouping of
;; functions.  There should be little other configuration here.  The purpose of
;; this grouping is to load dependent modules based on what the current Emacs
;; version is capable of, and based on what particular custom environment we're
;; using (i.e., "is this session doing Palm development?").
;; -----------------------------------------------------------------------------

(defun provide-customized-features ()
  "Load external packages and those that are not enabled by default."

  ;; If there are files in the elisp directory, there should be a "loaddefs"
  ;; file for autoloads.  Warn on the lack of this file, or load it if it
  ;; exists.  If there is nothing, do nothing.
  (let ((elisp-files (if (file-exists-p "~/elisp/")
                         (directory-files "~/elisp/" t "\.el[c]?"))))
    (if (or (file-exists-p custom-loaddefs-file) (null elisp-files))
        (when elisp-files
          (add-to-list 'load-path "~/elisp/")
          (load custom-loaddefs-file nil t))
      (warn (concat "Custom autoload file should exist at '%s' -- run "
                    "`update-custom-autoloads' to generate it.")
            custom-loaddefs-file)))

  ;; All this ccs stuff happens if the dotfile exists...
  (if (file-exists-p "/ccs/etc/dotfiles/.emacs")
      (progn
        (load "/ccs/etc/dotfiles/.emacs" nil t)
        (add-to-list 'custom-environments 'ccs-dev))
      (setq inferior-lisp-program "sbcl --noinform"))

  ;; Set up my Palm development environment
  (when (getenv "USE_HOLLY")
    (add-to-list 'custom-environments 'palm-dev))

  (when (getenv "VMWARE_CODE")
    (if (file-exists-p "~/elisp/vmw-c-dev.elc")
      (add-hook 'c-initialization-hook
        #'(lambda ()
            (require 'vmw-c-dev)
            (condition-case err (vmw-c-macro-initialize t)
              (error (message "Cannot use C++ preprocessor (yet): %s"
                              (error-message-string err))))
            (add-hook 'c-mode-hook   #'vmw-c-macro-initialize)
            (add-hook 'c++-mode-hook #'vmw-c-macro-initialize)))
       (message "Wouldn't it be nice if there were a VMware mode..."))
    (add-to-list 'custom-environments 'vmware-dev))

  (version-if (< 19 emacs-major-version) (provide-customized-features-20)))

(defun provide-customized-features-20 ()
  "Load features that only work with Emacs 20 and above."

  ;; This MOO chatroom functionality is used at Northeastern CCS (c. 1999).
  (if (file-exists-p "~/elisp/mud.elc") (require 'mud))

  ;; First type XGTags, then GTags, and finally fall back to xcscope.
  (if (file-exists-p "~/elisp/xgtags.elc")
      (add-hook 'c-initialization-hook
        #'(lambda ()
            (require 'xgtags)
            (add-hook 'c-mode-common-hook #'(lambda () (xgtags-mode 1)))))
    (if (file-exists-p "~/elisp/gtags.elc")
        (add-hook 'c-mode-common-hook #'gtags-mode)
      (if (file-exists-p "~/elisp/xcscope.elc")
          (eval-after-load 'cc-mode '(require 'xcscope)))))

  ;; Perforce is a horrible version control system -- it has an Emacs mode.
  (if (file-exists-p "~/elisp/p4.elc") ;; 2 slow
      ;; The worst part about this is it cannot really be autoloaded because
      ;; there's nothing intrinsic to a P4 repo that can tell Emacs that it's
      ;; looking at a versioned file.  The entire set of information for where
      ;; Perforce files exist lives within the client-spec, so the entire
      ;; package must be loaded at all time, just so it can keep asking
      ;; Perforce, "are we looking at a version file yet??"
      (require 'p4))

  (version-if (< 22 emacs-major-version) (provide-customized-features-23)))

(defun provide-customized-features-23 ()
  "Load features that only work with Emacs 23 and above."

  ;; Put hooks in to color Doxygen-style comments when C/C++ files load.  This
  ;; actually works in Emacs 22, but I'm too lazy to add another function here
  ;; in 2019.
  (add-hook 'c-initialization-hook #'add-doxygen-comment-style)

  (if (file-exists-p "~/elisp/undo-tree.elc") (global-undo-tree-mode))

  (version-if (< 23 emacs-major-version) (provide-customized-features-24)))

(defun provide-customized-features-24 ()
  "Load features that only work with Emacs 24 and above."

  (if (file-exists-p "~/elisp/clang-format.elc")
    (add-hook 'c-initialization-hook
      #'(lambda ()
          (add-hook 'c-mode-common-hook
           #'(lambda ()
               (local-set-key (kbd "C-c \\") #'clang-format-region))))))

  (version-if (or (< 27 emacs-major-version)
                  (and (= 27 emacs-major-version) (< 0 emacs-minor-version)))
    ;; Checking against 27.1 because some of what I'm looking for exists at that
    ;; minor version.
    (provide-customized-features-27)))

(defun provide-customized-features-27 ()
  "Load features that only work with Emacs 27.1 and above."

  ;; Since Emacs 27, the `fill-column-indicator' works natively, and disrupts
  ;; fewer minor modes.
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

  ;; We can consider `so-long-mode', but that's a major mode.  A less impacting
  ;; work-around is to disable the bidirectional parenthesis matching.  I do not
  ;; use any bidirectional tests, and if I did, I could live without the
  ;; parentheses matching.
  (setq bidi-inhibit-bpa t))

;; -----------------------------------------------------------------------------
;; Top-level configuration routines:
;;
;; Ideally, there would be one function named `custom-configure-emacs' that
;; would do all the work.  This unfortunately gets confusing because of the
;; Emacs version number checks.  This section should be thought of as a bunch of
;; functions that comprise a single configuration routine, broken up based on
;; version requirements.  Place nothing in this section that doesn't begin with
;; "custom-configure-emacs" in the name.
;; -----------------------------------------------------------------------------

(defun custom-configure-emacs ()
  "This is the full configuration/customization function for Emacs."

  (if (and lock-dotfile-version
           (not (= compiled-dotfile-version emacs-major-version)))
      (warn (concat "Emacs initialization file has been locked to (major) "
                    "version %d (running: %d)")
            compiled-dotfile-version emacs-major-version))

  (provide-customized-features)

  (set-key-bindings)

  ;; Add some file types to interpret.
  (associate-file-types)

  ;; Set "defaults" for buffer-local variables in all buffers.
  (setq-default
   ;; By default, I'll want to wrap my lines at 80 columns (default-fill-column
   ;; was deprecated fully by Emacs 26).
   fill-column                  ideal-window-columns


   ;; I hate tabs.
   indent-tabs-mode             nil)

  ;; Set some configuration variables.  When possible, limit the number of times
  ;; we `eval-after-load'.  Using `setq' on some free variables that will be
  ;; initialized during customization is an easy way to do that.
  (setq
   ;; Some C specific modes
   c-backslash-column           45
   c-default-style              "bsd"
   c-basic-offset               4
   c-tab-always-indent          nil
   compilation-scroll-output    t

   ;; I don't like that I can delete the prompt in the shell.
   comint-prompt-read-only      t

   ;; I've always preferred the -F postfixes in ls output.
   dired-listing-switches       (if (eq system-type 'darwin)
                                    "-aFhlv" "-aDFhlv")
   dired-ls-F-marks-symlinks    (eq system-type 'darwin)

   ;; Longer selections tend to make the mode-line too long (with battery).
;  display-time-24hr-format     t
;  display-time-day-and-date    t
;  display-time-format          "%a-%Y/%m/%d-%H:%M"
   display-time-format          "%H:%M/%a"

   ;; This isn't working on FVWM.  It's taking my mouse and putting it at the
   ;; bottom left corner of the screen, then the desktop jumps.
   ediff-grab-mouse             nil

   ;; So you're not supposed to make this longer than 65 because it looks better
   ;; in apropos for documentation, but I really haven't noticed anything odd in
   ;; the help text for my own functions here...
   emacs-lisp-docstring-fill-column ideal-window-columns

   ;; I've already read it.
   inhibit-startup-message      t

   ;; Don't like the initial stuff in my scratch buffer.
   initial-scratch-message      nil

   ;; I'm not sure why anybody thought this was a good idea...
   isearch-lax-whitespace       nil
   isearch-regexp-lax-whitespace nil

   ;; Always making backups.
   make-backup-files            t

   ;; Look, just put the new man page into the currently selected window.  By
   ;; default to makes a new window, and that's almost always exactly where I
   ;; don't want it.
   Man-notify-method            'pushy

   ;; Restore functionality s.t. down adds lines to the end of the file.  This
   ;; was changed in Emacs 22.
   next-line-add-newlines       t

   ;; The default is T, which I lived with for a while, but the indentation
   ;; seems like a pointless waste of space.
   org-adapt-indentation        nil

   ;; You can change how collapsed nodes are indicated in `org-mode', but more
   ;; than this, the custom `org-ellipsis' face is not used unless this is set
   ;; for some reason.
   org-ellipsis                 "[+]"

   ;; Smart quotes are just plain better than straight quotes (if I could get it
   ;; to put in &ensp; after punctuation, so much the better).
   org-export-with-smart-quotes t

   ;; This has to be done for Confluence, because the stupid web GUI strips out
   ;; CSS when I paste in source code snippets.
   org-html-htmlize-output-type 'font

   ;; Org-mode introduced a really annoying feature to indent any `src' blocks
   ;; by 2 spaces whenever they are touched.  No need for that.
   org-src-preserve-indentation t

   ;; I don't like `truncate-lines'.
   org-startup-truncated        nil

   ;; I tend to like to know that I'm working on a certain to-do item.
   org-todo-keywords             '((sequence "TODO" "BUSY" "DONE"))

   ;; Older versions of Emacs ask every time I open this file, but
   ;; `lexical-binding' is definitely OK to be set.
   safe-local-variable-values   '((lexical-binding . t))

   ;; Only Enter and C-g exit the search.
   search-exit-option           nil

   ;; Get rid of shift moving the mark around...
   shift-select-mode            nil)

  ;; Put some indication as to when lines are wrapped.  (Actually, don't do
  ;; this, it's really distracting.  I left this here has a reminder.)
;;(setq visual-line-fringe-indicators
;;      (alist-get 'continuation fringe-indicator-alist))

  ;; It's stupid that this is not the default behavior
  (let ((ev-server-name (getenv "EMACS_SERVER_FILE")))
    (unless (null ev-server-name) (setq server-name ev-server-name)))

  ;; When the server is running, check that the user really wants to exit, as
  ;; C-x/C-c can sometimes be inadvertent.  Only do this check, however, when
  ;; there is no desktop, as a persistent desktop state can be restored in short
  ;; order.  This function is trying to guard against losing a large number of
  ;; documents that is not meant to persist across sessions."
  (add-hook 'kill-emacs-query-functions
            #'(lambda ()
              (if (and (and (featurep 'server) server-process)
                       (or (not (featurep 'desktop)) (null desktop-dirname)))
                  (y-or-n-p "Really close all windows? ")
                t)))

  ;; If we have a backup drop-zone, customize it to use versions, etc...
  (let* ((backup-dir   "~/.emacs.bak")
         (attributes   (file-attributes backup-dir))
         (file-exists  (not (null attributes)))
         (is-directory (car attributes)))
    (if (and (not (eq system-type 'ms-dos))
             file-exists
             is-directory)
        (custom-configure-backups backup-dir)))

  ;; If and when we use Google Go, the language relies on TAB indentation by
  ;; default.  Set the tab stop to something reasonable.
  (add-hook 'go-mode-hook #'(lambda () (setq tab-width 4)))

  ;; For whatever reason, the lambda keyword no longer gets highlighted when
  ;; using the #' shorthand.  Fixing this here.  Note that this is going before
  ;; the `customize-font-lock' function because it runs on the
  ;; `window-setup-hook', which is too late to change the highlights on buffers
  ;; that load at start-up (e.g., .el files specified on the command line).
  (add-hook
   'emacs-lisp-mode-hook
   #'(lambda ()
       (if (featurep 'font-lock) ; Just in case font-lock doesn't exist!
           (font-lock-add-keywords
            nil '(("#'(\\(lambda\\)\\b" 1 'font-lock-keyword-face))))))

  ;; There is not a great way to paste into the term-mode buffer by default.
  (add-hook 'term-mode-hook
   #'(lambda ()
       ;; The "fixed" function can be version-if removed when the fix reaches
       ;; the Emacs code base.
       (define-key term-raw-map [xterm-paste]   #'term--xterm-paste-fixed)
       (define-key term-raw-map (kbd "C-c C-y") #'term-paste)
       (define-key term-raw-map (kbd "C-c M-y") #'yank-pop))) ;; Doesn't work.

  ;; Provide some nice GUI tools from the Emacs command-line for diff and merge
  (add-to-list 'command-switch-alist `("--diff"  . ,#'command-line-diff))
  (add-to-list 'command-switch-alist `("--merge" . ,#'command-line-merge))

  ;; Allow us to "name" our Emacs instance from the command line
  (add-to-list 'command-switch-alist
               `("--instance-id" . ,#'command-line-instance-id))

  (unless-running-xemacs
    ;; Show me the time, so I can tell how bored I am.  Have to update the time
    ;; _after_ changing settings.
    (display-time)

    ;; Show selected marked area
    (transient-mark-mode -1))

  ;; I want to know what column and line I am in
  (column-number-mode 1)
  (line-number-mode   1)

  ;; I really don't get what all the hubub is about...
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region   'disabled nil)
  (put 'scroll-left     'disabled nil)
  (put 'scroll-right    'disabled nil)

  ;; Some additional major customizations based on display capabilities:
  (custom-configure-display)

  ;; Special configurations for saved Emacs sessions.
  (unless-running-xemacs (custom-configure-desktop)) ; Doesn't exist in XEmacs.

  ;; Descend into version-specific customizations.
  (version-if (< 19 emacs-major-version) (custom-configure-emacs-20))

  ;; Now that we've detected what custom development environments we're using
  ;; from `provide-customized-features', call the configuration functions as
  ;; needed.  These should be done last so they can even override the
  ;; configurations above.
  (let ((custom-env-funcs `((ccs-dev    . ,#'configure-ccs-dev-env)
                            (palm-dev   . ,#'configure-palm-dev-env)
                            (vmware-dev . ,#'configure-vmware-dev-env))))
    (mapc #'(lambda (custom-env)
              (let ((found-cust-env (assq custom-env custom-env-funcs)))
                (unless (null found-cust-env) (funcall (cdr found-cust-env)))))
          custom-environments)))

(defun custom-configure-emacs-20 ()
  "Customizations that are only applicable to Emacs 20 and above."

  (unless-running-xemacs (show-paren-mode 1))

  (version-if (< 20 emacs-major-version)
    (custom-configure-emacs-21)
    (display-battery)))

(defun custom-configure-emacs-21 ()
  "Customizations that are only applicable to Emacs 21 and above."

  (eval-after-load 'ansi-color '(merge-font-lock-settings ansi-color-faces))
  (eval-after-load 'diff-mode  '(merge-font-lock-settings diff-faces))
  (eval-after-load 'ebrowse    '(merge-font-lock-settings ebrowse-faces))
  (eval-after-load 'ediff      '(merge-font-lock-settings ediff-faces))
  (eval-after-load 'gud        '(merge-font-lock-settings gud-faces))
  (eval-after-load 'org        '(merge-font-lock-settings org-faces))
  (eval-after-load 'whitespace '(merge-font-lock-settings whitespace-faces))

  (eval-when-compile (defvar default-toolbar-visible-p)) ; XEmacs noise...

  ;; Enable mouse wheel support by default; disable the tool bar.
  (compat-mwheel-install)
  (compat-tool-bar-mode -1)

  (unless-running-xemacs
    (blink-cursor-mode -1)

    (condition-case nil
        (display-battery-mode 1)
      (error nil)) ; Ignore errors if AC powered!
    (unless display-battery-mode (unload-feature 'battery)))

  (version-if (< 21 emacs-major-version) (custom-configure-emacs-22)))

(defun custom-configure-emacs-22 ()
  "Customizations that are only applicable to Emacs 22 and above."

  ;; The `newcomment' package only exists in Emacs 21.1 and above, but just to
  ;; keep 21.0 safe (if there was such a version?) we'll use it here.
  ;; Essentially, this code makes `text-mode' "quote" paragraphs, email-style
  ;; with `comment-region' and friends.
  (add-hook 'text-mode-hook
            #'(lambda ()
                (make-local-variable 'comment-start) ;; Always make buffer-local
                (setq comment-start "> ")
                (local-set-key "\C-c\C-c" #'comment-region)))

  ;; Show me a small set of extraneous bits of whitespace.
  (setq whitespace-global-modes
        '(not dired-mode elisp-byte-code-mode org-mode text-mode))
  (unless-running-xemacs (global-whitespace-mode 1))
  (setq whitespace-style '(face trailing table lines empty tab-mark))

  ;; Org-mode only exists in version 22 and above, but it doesn't seem to alter
  ;; the `auto-mode-alist'.
  (setq auto-mode-alist (append `(("\\.org$" . ,#'org-mode)) auto-mode-alist))

  ;; This sets up hooks for org-clock that prevent me from leaving work with a
  ;; running clock, and accidentally claiming that I've worked 47 straight hours
  ;; on a single task.
  (add-hook 'org-clock-in-hook #'on-org-clock-in)
  (add-hook 'org-clock-out-hook #'on-org-clock-out))

;; -----------------------------------------------------------------------------
;; Customizations for Specific Modules:
;;
;; Larger, related bits of configuration have been broken out into functions
;; here.  If there is a bunch of configuration that takes more than a few lines,
;; it should not be directly in `custom-configure-emacs', but it should be its
;; own function in this section.
;; -----------------------------------------------------------------------------

(defun associate-file-types ()
  "Set up associations between file types and Emacs major modes."

  ;; There are no hooks for fundamental mode...
  (setq-default major-mode 'text-mode)
  (setq initial-major-mode 'text-mode)

  (when (file-exists-p "~/elisp/media-wiki.elc")
    ;; NOTE: `mediawiki-mode' is not autoload enabled...
    (autoload 'mediawiki-mode "media-wiki" "Edit wiki documents." t)
    (setq auto-mode-alist
          (append `(("\\.wiki$" . ,#'mediawiki-mode)) auto-mode-alist)))

  ;; Load Batch-script stuff.
  (setq auto-mode-alist
        (append (if (file-exists-p "~/elisp/dos.elc")
                    (progn
                      (autoload 'dos-mode "dos" "Edit Dos scripts." t)
                      `(("\\.bat$" . ,#'dos-mode) ("\\.cmd$" . ,#'dos-mode)))
                  `(("\\.bat$" . ,#'sh-mode)  ("\\.cmd$" . ,#'sh-mode)))
                auto-mode-alist))

  (when (file-exists-p "~/elisp/visual-basic-mode.elc")
    (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
    (setq auto-mode-alist
          (append `(("\\.bas$" . ,#'visual-basic-mode)
                    ("\\.vb$"  . ,#'visual-basic-mode)
                    ("\\.vbs$" . ,#'visual-basic-mode)))))

  (setq auto-mode-alist
        (append `(("\\.C$"       . ,#'c++-mode)
                  ("\\.cc$"      . ,#'c++-mode)
                  ("\\.H$"       . ,#'c++-mode)
                  ("\\.h$"       . ,#'c++-mode)
                  ("\\.hpp$"     . ,#'c++-mode)
                  ("\\.i$"       . ,#'c++-mode)
                  ("\\.rc$"      . ,#'c++-mode)
                  ("\\.sc$"      . ,#'python-mode)
                  ("\\.r$"       . ,#'c-mode)
                  ("\\.make$"    . ,#'makefile-mode)
                  ("\\.mk$"      . ,#'makefile-mode)
                  ("[Mm]akefile" . ,#'makefile-mode)
                  ("\\.M$"       . ,#'nroff-mode)
                  ("\\.ms$"      . ,#'nroff-mode)
                  ("sendmail.cf" . ,#'sh-mode) ;; Don't ask...
                  ("Doxyfile"    . ,#'sh-mode) ;; Don't ask...
                  ("\\.csh$"     . ,#'sh-mode)
                  ("\\.ksh$"     . ,#'sh-mode)
                  ("\\.sh$"      . ,#'sh-mode)
                  ("\\.txt$"     . ,#'text-mode))
                auto-mode-alist)))

(defun custom-configure-backups (custom-backup-dir)
  "Configure the Emacs backup settings.

Specify the directory where Emacs creates backup files with CUSTOM-BACKUP-DIR."

  (setq
   ;; Send backups to a local directory.  This must exist and be writable.
   backup-directory-alist `(("." . ,custom-backup-dir))

   ;; Enable backups by number instead of the default 'tilde' style..
   version-control t

   ;; Silently delete old backups.  My thinking here is that I don't care if
   ;; backups get deleted because it's still better than the default, which only
   ;; saves the LAST version...
   delete-old-versions t

   ;; Keep this many of the latest versions
   kept-new-versions 2

   ;; Leave this many of the oldest versions
   kept-old-versions 2))

(defun custom-configure-desktop ()
  "Set up `desktop-mode' functionality."

  (setup-desktop-restore-transient-buffer 'compilation-mode)

  (version-when (< 24 emacs-major-version)
    ;; Save this off for things like `restore-last-desktop-frameset'.
    (add-hook 'desktop-after-read-hook
              #'(lambda ()
                  (setq last-restored-frameset desktop-saved-frameset)))

    ;; Saving the display is nice, but since I tend to do a lot of work over
    ;; SSH-tunneled displays, the value is invariably wrong (the display is
    ;; generated dynamically).  To make matters worse, I would think
    ;; `desktop-restore-in-current-display' would just solve the problem, but
    ;; it doesn't seem to.  I'm therefore skipping the ability to restore to
    ;; multiple displays (so I can make remote displays show up) by keeping
    ;; the `display' property out of saved framesets.
    (add-to-list 'frameset-filter-alist '(display . :never))

    ;; Restoring the frameset under Emacs daemon kills the daemon "frame", and
    ;; results in closing the last frame causing Emacs to exit.  This is not
    ;; what we want at all.  Instead, force the desktop to keep around the old
    ;; frames instead of trying to reuse them.
    (when (daemonp)
      (modify-frame-parameters daemon-frame '((desktop-dont-save . t)))
      (setq desktop-restore-reuses-frames 'keep))

    (when (terminal-frame-p)
      ;; THIS COULD NOT BE STUPIDER.  In order for `desktop-read' to restore
      ;; the frameset that was saved in the desktop, it must call
      ;; `desktop-restoring-frameset-p', which in turn checks the function
      ;; `display-graphic-p'.  This essentially means that, even though
      ;; restoring the frameset works on the TTY (providing
      ;; `desktop-restore-forces-onscreen' is NIL), it NEVER runs.  So we get
      ;; the full desktop, but none of the frames.  FIXING THIS NOW.
      (setq desktop-restore-forces-onscreen nil)
      (add-hook
       'desktop-after-read-hook
       #'(lambda ()
           (frameset-restore
            desktop-saved-frameset
            :reuse-frames (eq desktop-restore-reuses-frames t)
            :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
            :force-display desktop-restore-in-current-display
            :force-onscreen desktop-restore-forces-onscreen)))))

  ;; Set everything up for us to use a desktop (saved session) if asked.
  ;;
  ;; NOTE: Calling `desktop-read' should be the last thing the init-file does so
  ;; that all modes are fully configured prior to resurrecting any stateful
  ;; buffers from the desktop.  Otherwise, we get things like `whitespace-mode'
  ;; with all the settings we don't want to have enabled.
  ;;
  ;; It is also important to have this run after the hooks are installed for
  ;; `customize-font-lock-on-frame', or else the colors will look right on the
  ;; first frame, but none of the subsequent frames will be right.
  ;; Consequently, see `customize-font-lock' for where `desktop-read' gets
  ;; invoked.
  (let ((env-dt-dir (getenv "EMACS_DESKTOP_DIR")))
    (if (and env-dt-dir
             (file-exists-p env-dt-dir)
             (not (member "--no-desktop" command-line-args)))
        (setq desktop-path (list env-dt-dir))
      ;; Clear out the EV that causes `desktop-read' on the false path.
      (setenv "EMACS_DESKTOP_DIR" nil))

    ;; Because we're handling the desktop load manually, passing "--no-desktop"
    ;; works just fine, but gives a spurious warning about how the desktop isn't
    ;; loaded.  Go ahead and delete it like `desktop' would do.
    (when (member "--no-desktop" command-line-args)
      (unless (featurep 'desktop)
        (setq command-line-args (delete "--no-desktop" command-line-args))))))

(defun custom-configure-display ()
  "Configure settings that apply to how Emacs behaves on various kinds of
display."
  (setq
   ;; Why was this turned off in Emacs 24.1?
   mouse-drag-copy-region       t

   ;; Quite an assumption, but there's really no uniform way to tell (even the
   ;; Emacs docs say this).
   focus-follows-mouse          (or (eq system-type 'windows-nt)
                                    (eq system-type 'darwin)))

  ;; Print the name of the visited file in the title of the window...
  (set-emacs-title-format emacs-title-format)

  (let ((terminal-frame-p (terminal-frame-p))
        (selected-frame   (selected-frame)))
    ;; Add hooks for setting up frames lazily (run once, as
    ;; `after-make-frame-functions' does not run on the first frame).
    (add-hook 'after-make-frame-functions #'custom-configure-frame)
    (custom-configure-frame selected-frame)

    ;; Hack attack!  On older Emacs versions, something kicks `menu-bar-mode' to
    ;; on after the initial frame's configuration (I don't think it's me).  As a
    ;; result, just set `menu-bar-mode' to whatever `custom-configure-frame'
    ;; would have set it, and that first frame won't be clobbered.
    (unless-running-xemacs (menu-bar-mode (if terminal-frame-p -1 1)))
    (if (or terminal-frame-p (daemonp))
        ;; Set a hook that removes itself prior to executing.  It only runs when
        ;; there is frame on a windowing system.
        (add-hook 'after-make-frame-functions #'exec-on-first-gui)
      ;; Else, skip the hook, and configure the GUI right now.
      (custom-configure-on-first-gui selected-frame)))

  ;; Can't customize font lock until we load the library (and faces) first.  On
  ;; the terminal, Emacs does not reliably detect the color scheme until late in
  ;; the initialization, causing us to potentially put in the wrong colors.
  (add-hook 'window-setup-hook #'customize-font-lock))

(defun custom-configure-frame (frame)
  "Modify the current configuration for the frame, FRAME."

  (let ((terminal-frame-p (terminal-frame-p frame)))
    (modify-frame-parameters
     frame `((menu-bar-lines
              . ,(if (or terminal-frame-p
                         ;; Don't mess with the ediff control frame.
                         (string= (frame-parameter frame 'name) "Ediff"))
                     0 1))))

    ;; Emacs doesn't properly set the cursor/mouse color for dark backgrounds
    ;; unless the background is pure black.  TODO: There is something wrong
    ;; somewhere here -- maybe with Xresources?
    (unless-running-xemacs
      ;; Only capture a GUI's parameters -- capturing them from the terminal
      ;; will mess things up.
      (unless terminal-frame-p
        (let ((frame-params (frame-parameters frame)))
          (if (eq 'dark (cdr (assq 'background-mode frame-params)))
              (let ((fg-color (cdr (assq 'foreground-color frame-params)))
                    (bg-color (cdr (assq 'background-color frame-params))))
                (set-cursor-color fg-color)
                (set-mouse-color  fg-color)
                ;; Reversed color looks best for scroll bars...
                (unless (boundp 'motif-version-string)
                  (set-face-foreground 'scroll-bar bg-color frame)
                  (set-face-background 'scroll-bar fg-color frame)))))))))

(defun custom-configure-on-first-gui (frame)
  "Configure the window-system options (lazily) when the first such frame is
created.  This function uses FRAME, which is the newly created UI frame.

This functionality is needed for a few reasons.  Most notably, the
`find-first-defined-font' actually crashes the Emacs process if it executes on a
terminal frame.  We only need this information once, so it can be gathered as
soon as there is a windowed frame to use, and it can then be stashed into
`default-frame-alist'.

Beside this, the function sets other aspects of `default-frame-alist' that
require a windowed frame, like the default frame geometry (which is based on the
first created frame)."

  ;; Determine the font we can use somewhat dynamically by falling through until
  ;; we find one that exists on our system.
  (let ((chosen-gui-font
         (cond
          ((eq system-type 'windows-nt)
           (find-first-defined-font
            "-*-Courier New-*-*-*-*-11-*-*-*-c-*-iso8859-1"
            '(;; Consolas 11
              "-*-Consolas-normal-r-*-*-12-*-*-*-c-*-iso8859-1"
              ;; Liberation Mono 9
              "-*-Liberation Mono-*-*-*-*-12-*-*-*-c-*-iso8859-1"
              ;; Lucida Console 8 (thinner)
              "-*-Lucida Console-*-*-*-*-11-*-*-*-c-*-iso8859-1")
            frame))
          ((eq system-type 'darwin)
           (find-first-defined-font "Menlo 12" '("DejaVu Sans Mono 9") frame))
          ((eq system-type 'cygwin)
           (find-first-defined-font "8x13" '("Consolas 9") frame))
          (t
           (find-first-defined-font
            "8x13"
            '("DejaVu Sans Mono 9"
              "FreeMono 10"
              "Nimbus Mono L 10"
              "-Misc-Fixed-normal-normal-normal-*-13-*-*-*-c-*-iso10646-1")
            frame)))))
    ;; Let's set up some universal window (frame) attributes.
    (setq default-frame-alist
          (append
           `((cursor-type . box)
             (font . ,(adjust-font-for-hires chosen-gui-font)))
           (cond
            ((eq system-type 'windows-nt)
             ;; Preserve the background at configuration time for future frames.
             ;; Because there's no .Xdefaults on older version of Windows Emacs,
             ;; we rely on command-line arguments.
             `((background-color . ,(frame-parameter frame 'background-color))

               ;; TODO: Perhaps this hard coded value should be given in a hook,
               ;; or in initial-frame-alist?
               (height . ,(if (use-high-dpi-p) 55 70))
               (width  . 81)))
            ((eq system-type 'darwin)
             `((height . 60)
               (width  . 81)))
            (t
             ;; Just make all the windows the same size (hopefully configured in
             ;; .Xdefaults).
             `((height . ,(frame-parameter frame 'height))
               (width  . ,(frame-parameter frame 'width)))))))

    ;; Assuming that the "enableThinThickness" resource is "true", the Motif
    ;; scroll bars will be too fat, so reconfigure them for all frames here if
    ;; we detect Motif.  Ideally, this setting would live in the Xdefaults file
    ;; with the other setting, but it is not possible to determine Emacs's Motif
    ;; support from Xdefaults.
    (when (thin-thickness-p)
      (setq default-frame-alist
            (append default-frame-alist '((scroll-bar-width . 13)))))

    ;; This function runs after the frame has been modified, so apply whatever
    ;; settings needed from `default-frame-alist' here.
    (modify-frame-parameters frame default-frame-alist)))

(defun exec-on-first-gui (frame)
  "Execute a hook in `after-make-frame-functions' on FRAME one time.  After this
execution, the hook removes itself."
  (unless (terminal-frame-p frame)
    (remove-hook 'after-make-frame-functions #'exec-on-first-gui)
    (custom-configure-on-first-gui frame)))

;; -----------------------------------------------------------------------------
;; Font-lock Customization:
;;
;; Customizing `font-lock-mode' is complicated.  Each frame is assumed to have a
;; "light" or "dark" background (see `dark-background-p'), and the faces must be
;; set according to each frame accordingly during the `window-setup-hook'.
;; There is a list of generic faces with their settings in `bg-dark-faces' and
;; `bg-light-faces'.  The functions below apply them on a per-version basis,
;; referencing the "faces-version-*" variables, which specify that it's OK to
;; modify a face for the named version (or above).
;;
;; After this, there are mode-specific faces, (e.g., `ediff-faces') that have an
;; association list with keys `light' and `dark' that will only be added to the
;; global "bg-*-faces" variables when that mode is loaded for the first time.
;; -----------------------------------------------------------------------------

(defun customize-font-lock ()
  "Set up syntax highlighting."

  ;; Yes, we're explicitly requiring this, and no it's not being done in the
  ;; `provide-customized-features' section.  This is because
  ;; `customize-font-lock' is loaded on-demand with the `window-setup-hook'.
  (require 'font-lock)

  ;; By default, this face is UNREADABLE.
  (version-if (or (and (= 20 emacs-major-version) (< 2 emacs-minor-version))
                  (< 20 emacs-major-version))
    ;; TODO: `make-face' seems to indicate a logic error here...
    (make-face 'sh-heredoc)
    (make-face 'font-lock-doc-face))

  ;; Go all out with the font colors
  (setq font-lock-maximum-decoration t)

  ;; Syntax highlighting -- I just find syntax highlighting annoying on the
  ;; Windows terminal, so disable it by default there.
  ;;
  ;; NOTE: This is kind of a bug in that `global-font-lock mode' will not be
  ;; enabled on Windows Emacs GUIs (should I start a server there, and then
  ;; launch a GUI frame) because we've already disabled it on the TTY.  There
  ;; doesn't seem to be a means of disabling `font-lock-mode' on specific
  ;; frames instead of specific buffers.
  (global-font-lock-mode
   (if (and (eq system-type 'windows-nt) (terminal-frame-p)) -1 1))

  (customize-font-lock-on-frame (selected-frame))
  (add-hook 'after-make-frame-functions #'customize-font-lock-on-frame)

  ;; NOTE: `desktop-read' must come late, so we have it here.  See
  ;; `custom-configure-desktop' for rationale, and for where everything gets set
  ;; for this invocation.
  (unless-running-xemacs (if (getenv "EMACS_DESKTOP_DIR") (desktop-read))))

(defun add-doxygen-comment-style ()
  "Add a new Doxygen comment style, and apply it to C/C++ files.

By default, this method installs a hook, `set-doxygen-style-hook', to
`c-mode-hook' and `c++-mode-hook'.  These may be removed if `c-default-style'
refers to the 'doxygen comment style, as they will do extra work."

  ;; Only add these constants after `cc-mode' has loaded, or they won't be valid
  ;; expressions.  For example `c-doc-markup-face-name' will not exist.  See
  ;; `font-lock-keywords' for what the elements in this constant should contain.
  (defconst doxygen-font-lock-doc-comments
    (let ((symbol "[a-zA-Z0-9_\.\*\-\:]+"))
      `(;; HTML tags:
        ("</?\\sw\\(\\sw\\|\\s \\|[=\n\r*.:]\\|\"[^\"]*\"\\|'[^']*'\\)*>"
         0 ,c-doc-markup-face-name prepend nil)
        ;; `symbol`, 'symbol', and ``symbol``:
        (,(concat
           "\\(?:\'" symbol "\'\\|\`" symbol "\`\\|\`\`" symbol "\`\`\\)")
         0 ,c-doc-markup-face-name prepend nil)
        ;; Special case for "@param[in,out] var", as I use it so much:
        ;;
        ;; NOTE that matching the "[in,out]" does not use the ? operation, but
        ;; instead matches "\b".  This is because when this clause matches 1
        ;; time, everything comes out OK, but matching 0 times kills syntax
        ;; highlighting for the entire rest of the file.  So instead of matching
        ;; 0 or 1, we match either what we're looking for, or a meaningless
        ;; empty character.
        (,(concat
           "[\\@]param"
           "\\(?:[[:space:]]*\\[\\(?1:in\\|out\\|in,out\\)\\]\\|\\(?1:\\b\\)\\)"
           "[[:space:]]+\\(?2:" symbol "\\)")
         (0 ,c-doc-markup-face-name prepend nil)
         (1 font-lock-type-face prepend nil)
         (2 font-lock-variable-name-face prepend nil))
        ;; Do "@tparam type" template specifications:
        (,(concat "[\\@]tparam[[:space:]]+\\(" symbol "\\)")
         (0 ,c-doc-markup-face-name prepend nil)
         (1 font-lock-type-face prepend nil))
        ;; Highlight headings (only on their own lines):
        ("^[/* ]*\\(#+\\)[[:space:]]+\\([^#]+\\)\\(#+\\)[/* ]*$"
         (1 ,c-doc-markup-face-name prepend nil)
         (2 font-lock-string-face prepend nil)
         (3 ,c-doc-markup-face-name prepend nil))
        ;; Special command with either words or non-word command names:
        ("[\\@]\\(?:[a-z]+\\|[[:punct:]]+\\)"
         0 ,c-doc-markup-face-name prepend nil)
        ;; #some_c_symbol.some_member:
        ("#[a-zA-Z0-9_\.\:]+" 0 ,c-doc-markup-face-name prepend nil)))
    "A table of regexps that identify keywords in Doxygen comment text.")

  ;; Matches (across multiple lines):
  ;;   /** Doxygen comments */
  ;;   /*! Doxygen comments */
  ;;   /// Doxygen comments
  ;; Doesn't match:
  ;;   /*******/
  ;;   /////////
  (defconst doxygen-font-lock-keywords
    `((,#'(lambda (limit)
            (c-font-lock-doc-comments
             "/\\(//[^/]\\|\\*[\\*!][^\\*!]\\)"
             limit doxygen-font-lock-doc-comments))))
    "A regular expression that identifies Doxygen-specific comments.")

  ;; For those `c-default-style' variants that already have 'doxygen set, you
  ;; can safely remove these hooks, as they will only do extra work.
  (defun set-doxygen-style-hook ()
    "Install 'doxygen as the default doc style for C and C++."
    (setq c-doc-comment-style 'doxygen)
    (c-setup-doc-comment-style))

  (add-hook 'c-mode-hook   #'set-doxygen-style-hook)
  (add-hook 'c++-mode-hook #'set-doxygen-style-hook))

(defun customize-font-lock-on-frame (frame)
  "Customize the color settings on a per-frame basis.

The FRAME parameter specifies which frame will be altered."
  ;; For some reason Emacs does not propagate the environment from one frame
  ;; to the next.  On Emacs client windows, that means the environment where
  ;; the client was started makes it to the initial frame, but no frames
  ;; created thereafter (think: a console session, where we check `COLORFGBG'
  ;; for settings, and get the value from the default environment only).  In
  ;; this callback, the `selected-frame' is the frame that is behind the
  ;; current frame.  For the first frame, the environment will be set in the
  ;; frame-parameters, so no action is needed.
  (if (null (frame-parameter frame 'environment))
    (modify-frame-parameters
     frame
     `((environment . ,(frame-parameter (selected-frame) 'environment)))))

  ;; TODO: There is a bug here.  When `frameset-restore' restores a frame with a
  ;; `background-mode' that doesn't match the default, this function applies the
  ;; wrong color scheme.  It seems the frame parameters are not correct when
  ;; this hook runs.
  (when (< 255 (compat-display-color-cells frame))
    (when (terminal-frame-p frame)
      (if (eq system-type 'darwin)
          ;; TODO: Should consolidate this into `dark-background-p'?
          (modify-frame-parameters
           frame `((background-mode . ,(get-macos-terminal-bg-mode))))
        (set-bg-mode-from-colorfgbg frame)))

    ;; Give me some nice pretty colors...
    (update-emacs-font-lock-faces
     (if (dark-background-p frame) bg-dark-faces bg-light-faces)
     ;; There's no good hook for frame creation in XEmacs, seemingly, so just
     ;; set the font-lock settings universally (by passing NIL for the frame).
     ;; For GNU Emacs, we can proceed normally.
     (if-running-xemacs nil frame))))

(defun modify-face-compat (face fg bg bold-p italic-p underline-p frame)
  "Modify a FACE on a specific FRAME in a backward-compatible way.

This function behaves similarly to the interactive function, `modify-face',
\(which is not preferred for non-interactive use).  It would be best to use
`set-face-attributes', but this function is not present until after Emacs 20.
The parameters FG and BG specify the face names for foreground and background
colors, and they may be NIL.  BOLD-P, ITALIC-P, and UNDERLINE-P enable or
disable boldness, italics, and underlines (respectively), and the special
symbol `ign' does nothing."

  (if-running-xemacs
    (progn ;; XEmacs doesn't like setting colors to NIL.
      (unless (or (null fg) (eq fg 'ign)) (set-face-foreground face fg frame))
      (unless (or (null bg) (eq bg 'ign)) (set-face-background face bg frame)))
    (progn
      (unless (eq fg 'ign) (set-face-foreground face fg frame))
      (unless (eq bg 'ign) (set-face-background face bg frame))))
  (unless-running-xemacs
    (unless (eq bold-p 'ign) (set-face-bold face bold-p frame))
    (unless (eq italic-p 'ign) (set-face-italic face italic-p frame)))
  (unless (eq underline-p 'ign)
    (set-face-underline face underline-p frame)))

(defun merge-font-lock-settings (settings-alist)
  "Merge a list of faces for into `bg-light-faces' and `bg-dark-faces'.

The SETTINGS-ALIST should contain an association list with zero or more elements
with the key `light' or `dark'."
  (setq bg-dark-faces  (append bg-dark-faces
                               (cdr (assq 'dark settings-alist)))
        bg-light-faces (append bg-light-faces
                               (cdr (assq 'light settings-alist))))

  ;; Refresh the current frame.  We may wish to make this driven by a parameter.
  (customize-font-lock-on-frame (selected-frame)))

(defun modify-font-lock-faces (faces-alist faces-to-modify which-frame)
  "Run `modify-faces' on all FACES-TO-MODIFY using the FACES-ALIST.

It is safe for a key to be specified in FACES-TO-MODIFY that is not present in
the FACES-ALIST.  In this case, the function ignores the key.  The WHICH-FRAME
parameter specifies the frame whose faces will be altered."

  (mapc #'(lambda (face-to-modify)
            (let ((params-for-modify-face (assq face-to-modify faces-alist)))
              (unless (null params-for-modify-face)
                (apply #'modify-face-compat
                       (append params-for-modify-face
                               (list which-frame))))))
        faces-to-modify))

(defun update-emacs-font-lock-faces (faces-alist which-frame)
  "Given a color scheme defined by FACES-ALIST (see `bg-dark-faces' and
`bg-light-faces' for examples), update the colors used across Emacs.  The
WHICH-FRAME parameter specifies the frame whose faces will be altered."

  (modify-font-lock-faces faces-alist faces-all-versions which-frame)

  (version-when (< 19 emacs-major-version)
    (modify-font-lock-faces faces-alist faces-version-20 which-frame)
    (version-if (and (= 20 emacs-major-version) (< 2 emacs-minor-version))
      (modify-font-lock-faces faces-alist faces-version-20-2 which-frame))
    (version-when (< 20 emacs-major-version)
      (modify-font-lock-faces faces-alist faces-version-21 which-frame)
      (version-when (< 21 emacs-major-version)
        (modify-font-lock-faces faces-alist faces-version-22 which-frame)
        (version-when (< 24 emacs-major-version)
          (modify-font-lock-faces faces-alist faces-version-25 which-frame)
          (version-when (< 27 emacs-major-version) ; Technically 28.1...
            (modify-font-lock-faces faces-alist
                                    faces-version-28 which-frame)))))))

;; -----------------------------------------------------------------------------
;; Location-specific development environment configurations:
;; -----------------------------------------------------------------------------

(defun configure-ccs-dev-env ()
  "Look for Northeastern CCS Emacs environment and configure accordingly."

  ;; The CCS .emacs file defines this, but Emacs whines when byte-compiling.
  (eval-when-compile (defvar ps-printer-name))

  (defalias 'run-lisp #'inferior-lisp
    "This is just another way to run the Lisp interpreter.

The Northeastern CCS Lisp courses frequently refer to `run-lisp', so I will make
a similar alias to avoid bucking the trend.")

  (setq inferior-lisp-program "acl")
  (setq printer-name          "escher")
  (setq ps-printer-name       "escher")

  ;; Set up my mail preferences
  (setq rmail-file-name       "~/mail/mbisson.ccs.neu.edu"))

(defun configure-palm-dev-env ()
  "Configure Emacs to develop for Palm Hollywood (Foleo)."

  (setq-default tab-width        4
                indent-tabs-mode nil)

  ;; xcscope-mode is conditionally included, so silence the errors for when it's
  ;; not present.
  (eval-when-compile (defvar cscope-database-file))

  (setq cscope-database-file "/home/mbisson/ws/src/cscope.out")
  (setq compile-command      "make -k -j3 -C /home/mbisson/ws/mail/main")
  (setenv "HM_DEBUG_LEVEL" "0")

  ;; Shell environment crap
  (when (string= (getenv "HOLLY_ARCH") "i386-linux")
    ;; We might need this for debugging (only on Intel simulator)...
    (setenv "HX_APP_PATH"    (getenv "HOLLY_RESULT_ROOT"))
    (setenv "HM_SAMSDIR"     (concat (getenv "HOLLY_RESULT_ROOT")
                                     "/sams"))
    (setenv "LD_LIBRARY_PATH"
            (concat (getenv "LD_LIBRARY_PATH")                       ":"
                    (concat (getenv "HOLLY_SYSTEM_ROOT") "/usr/lib") ":"
                    (concat (getenv "HOLLY_RESULT_ROOT") "/fpi")     ":"
                    (concat (getenv "HOLLY_RESULT_ROOT") "/lib")))

    ;; We're debugging on the device.
    (setq gud-gdb-command-name
          "/opt/holly/toolroot/arm-linux/bin/gdb --annotate=3")))

(defun configure-vmware-dev-env ()
  "Configure Emacs to develop VMware code."

  ;; I usually use an "instance" ID with dev environments, so there's no need to
  ;; repeat similar information by adding "%N" to the title.
  (setq server-title-mark (if (daemonp) " <>" " []"))

  ;; VMware's coding style is sufficiently unique to warrant its own C style
  ;; definition.
  (let ((vmware-c-style
         `((c-basic-offset . 3)         ; Three-space indent
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
               (topmost-intro-cont   . 0))))))
    ;; Must match guards around `add-doxygen-comment-style'.  It's initialized
    ;; lazily, so there's no other way than this manual coordination.
    (version-if (< 22 emacs-major-version)
      (setq vmware-c-style
            (append vmware-c-style '((c-doc-comment-style . doxygen)))))
    (c-add-style "vmware-c-c++-engineering-manual" vmware-c-style))

  (let ((vmware-style-hook
         #'(lambda () (c-set-style "vmware-c-c++-engineering-manual"))))
    (add-hook 'c-mode-hook   vmware-style-hook)
    (add-hook 'c++-mode-hook vmware-style-hook)
    (add-hook 'java-mode-hook
              #'(lambda () (setq c-basic-offset 3)))
    (add-hook 'protobuf-mode-hook
              #'(lambda () (setq c-basic-offset 3))))

  ;; Remove the redundant doxygen style hook, as it will be overridden by
  ;; `c-add-style'.  This needs to be done in `c-initialization-hook', as that's
  ;; where `add-doxygen-comment-style' adds them.  Add the hook the the end of
  ;; the list.
  (version-if (< 22 emacs-major-version)
    (add-hook 'c-initialization-hook
              #'(lambda ()
                  (remove-hook 'c-mode-hook   #'set-doxygen-style-hook)
                  (remove-hook 'c++-mode-hook #'set-doxygen-style-hook))
              t))

  ;; Git mode is slow when loading up a desktop with a hundred files, and I
  ;; never use it.  Also, the space it consumes in the mode-line generally hides
  ;; the clock.
  (setq vc-ignore-dir-regexp
        (format "\\(?:%s\\)\\|\\(?:%s\\)" vc-ignore-dir-regexp "/dbc/"))

  ;; We are only using a certain few compilers, so clean this up.
  (setq compilation-error-regexp-alist '(gcc-include gnu msft))
  (add-hook
   'compilation-filter-hook
   #'(lambda ()
     (ansi-color-apply-on-region compilation-filter-start (point-max))))

  ;; Since VMware now requires Kerberos authentication for home directory
  ;; access, make Emacs renew the Kerberos ticket every 12 hours.  The function
  ;; returns a timer, but I'm not bothering to cancel anything just now.
  (run-at-time t (* 12 60 60) #'renew-kerberos-ticket "VMWARE.COM")

  (let ((srcdir (getenv "VMWARE_SRCDIR")))
    (if (and srcdir (file-exists-p srcdir))
        (setq compile-command
              (concat "cd " srcdir
                      " && source " (getenv "SSH_ENV")
                      " && iscons PRODUCT=esx esx-all 2>&1"
                      " | grep -v \"depends on deprecated target '@\""
                      " | grep -v tmpfs.inc.sh"))))

  (let ((srcdir (getenv "DR_SRCDIR_UNIX")))
    (if (and srcdir (file-exists-p srcdir))
        (setq compile-command (concat "make -C " srcdir " server")))))

;; -----------------------------------------------------------------------------
;; Functions for keyboard macros:
;; -----------------------------------------------------------------------------

(defun set-key-bindings ()
  "Bind various useful function to various key sequences."

  (global-set-key "\M-+" #'insert-date)

  ;; I can't believe this isn't mapped by default, but...
  (global-set-key "\M-p" #'goto-line)

  (global-set-key [end]  #'end-of-buffer)
  (global-set-key [home] #'beginning-of-buffer)

  ;; Horizontal scroll by page is nice, but some finer-grained control is
  ;; better.
  (unless-running-xemacs
    (global-set-key [C-S-next]  #'(lambda () (interactive) (scroll-left 1 t)))
    (global-set-key [C-S-prior] #'(lambda () (interactive) (scroll-right 1 t))))

  (eval-after-load 'compile '(global-set-key "\C-c\C-r" #'recompile))

  ;; I use this function often, and not all terminals allow C-M-%.
  (define-key ctl-x-5-map "%" #'query-replace-regexp)

  ;; It's really annoying to have 10 windows open and fat-finger C-x 1, closing
  ;; them all.  Changing this behavior to ask for confirmation if there are four
  ;; windows or more.
  (global-set-key "\C-x1"
   #'(lambda (&optional confirmed)
     "Asks for confirmation before running `delete-other-windows' if there are
     four windows or more."
     (interactive (list (if (< 3 (count-windows))
                            (y-or-n-p "Really close all windows? ")
                          t)))
     (when confirmed (delete-other-windows))))

  ;; Sometimes I want a new frame with a different color scheme (e.g., when
  ;; exporting "htmlized" text from `org-mode').
  (define-key ctl-x-5-map "3" #'make-reversed-frame)

  ;; It helps to go backwards sometimes.
  (global-set-key "\C-xp"
   #'(lambda (&optional count)
     "Select another window in reverse cyclic ordering of windows."
     (interactive "p")
     (other-window (- (or count 1)))))
  (global-set-key "\C-x9" #'delete-other-windows-vertically)

  ;; Intuitively, frame switching seems backwards on the terminal to me.
  (define-key ctl-x-5-map "o" #'backward-other-frame)
  (define-key ctl-x-5-map "p" #'other-frame)

  ;; Add custom commands to the Buffer Menu.
  (define-key Buffer-menu-mode-map "R" #'Buffer-menu-revert-unmodified-buffers)

  (global-set-key "\C-c\C-v"
   #'(lambda ()
     "Copy the buffer's full path into the kill-ring."
     (interactive)
     (when buffer-file-name (kill-new (file-truename buffer-file-name)))))

  ;; XXX: These used to be required, but now Emacs and the console have figured
  ;; out how to play nice.  Re-enable these (with an appropriate conditional)
  ;; when we encounter another terminal / keyboard issue.
;;(set-key-bindings-term)
  (set-key-bindings-xwin))

(defun set-key-bindings-term ()
  "Set key bindings to make Emacs work well on the terminal."

  ;; Make sure there is no confusion about delete characters
  (global-set-key "\C-h" #'delete-backward-char)
  (global-set-key "\C-?" #'delete-char))

(defun set-key-bindings-xwin ()
  "Set key bindings that are specific to graphical Emacs instances."

  (global-set-key [delete]      #'delete-char)
  (global-set-key [kp-delete]   #'delete-char)
  (unless-running-xemacs
    (global-set-key [C-delete]    #'kill-word)
    (global-set-key [C-kp-delete] #'kill-word)
    (global-set-key [C-tab]       #'other-frame)
    (global-set-key [C-S-tab]     #'(lambda() (interactive) (other-frame -1)))))

;; -----------------------------------------------------------------------------
;; "Advice" for existing functions:
;;
;; NOTE that using "preactivate" tends to break when using a "compatible" Emacs
;; initialization (i.e., when `lock-dotfile-version' is T).  This is because the
;; older Emacs will compile the advice in such a way that it may not function on
;; newer builds.  If you need to compile this advice, specify the "compile"
;; flag, taking the one-time hit in the current Emacs session.  Hopefully these
;; advice definitions only really refer to already compiled functions elsewhere.
;; -----------------------------------------------------------------------------

(defvar hidden-minor-mode-lighters
  '(abbrev-mode eldoc-mode global-whitespace-mode)
  "Minor modes that need not be shown in the mode-line.")

(defadvice add-minor-mode (after hide-minor-mode-lighters activate compile)
  "Remove minor-mode noise from the mode-line."

  ;; Remove unwanted minor-mode lighters (text that appears in the mode-line for
  ;; active minor-modes) from the association list.  These are added when
  ;; `add-minor-mode' executes.  Maybe of them are basically useless settings
  ;; that I either don't use, or assume are active, and they just end up pushing
  ;; important information off the mode-line, like the clock.
  (mapc #'(lambda (to-remove-minor-mode)
            (setq minor-mode-alist
                  (assq-delete-all to-remove-minor-mode minor-mode-alist)))
        hidden-minor-mode-lighters))

;; We use `eval-after-load' here because `ad-handle-definition' prints a
;; (harmless, but) noticeable message on Emacs start-up about how `start-server'
;; was re-defined.
(eval-after-load 'server
  '(defadvice server-start (after tell-server-start activate compile)
     "Alter the (default) frame titles when the Emacs server status changes."
     (on-server-state-change)))

(defadvice gud-display-line (after my-gud-highlight activate compile)
  "Highlight current line.

The highlight will apply to all debuggers that `gud-mode' supports.  This
highlight is present in only one buffer at a time (naturally, as the debugger
only looks at one stack frame in one thread at a time).  The highlight is
defined as an overlay by `gud-overlay', which is created lazily.  The overlay
will be removed by advising the `gud-sentinel' function, which clears out
`gud-overlay-arrow-position' when the debugging process exits, or when the GUD
buffer is killed.

Note that all this logic will break down if GUD ever supports multiple debugging
sessions in a single Emacs.  Currently the GUD code is written with lots of
global variables, so you can technically start two debuggers, but there is odd
behavior and contention around those global variables."

  (if (null gud-overlay)
      ;; Lazily create this overlay.
      (let ((ov (make-overlay (point-min) (point-min))))
        (overlay-put ov 'face 'gdb-selection)
        (setq gud-overlay ov)))

  (let* ((ov gud-overlay)
         (bf (gud-find-file true-file)))
    (with-current-buffer bf ; Formerly (save-excursion (set-buffer bf) ...)
      (move-overlay ov
                    (line-beginning-position)
                    (+ (line-end-position) 1)
                    (current-buffer)))))

(defadvice gud-sentinel (after my-gud-sentinel activate compile)
  "Remove the highlight when the sentinel detects a dead process."

  (if (null gud-overlay-arrow-position)
      (delete-overlay gud-overlay)))

;; -----------------------------------------------------------------------------
;; Misc. Hooks and Functions:
;;
;; This is basically everything else.  It is organized further into subgroups
;; like "CLI switches" and "Interactive Functions".
;; -----------------------------------------------------------------------------

;; =========================================================
;; Functions invoked by command-line switches:
;; =========================================================

(defun command-line-diff (_switch)
  "Enter (the more graphical) `ediff' from the command-line.

_SWITCH contains the switch string that invoked this function if it was called
from the command-line switch handler."

  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(defun command-line-instance-id (_switch)
  "Allow us to `name' our Emacs instance.

This is not strict, nor does it need to be unique.  The main   purpose of this
is for when I have more than one Emacs   environment running.  This way, I can
put a friendly name in the title for easy identification.

_SWITCH contains the switch string that invoked this function if it was called
from the command-line switch handler."
  (let ((emacs-instance-id (pop command-line-args-left)))
    (when emacs-instance-id
      (setq emacs-title-format
            (concat "(" emacs-instance-id ") " emacs-title-format))
      (set-emacs-title-format emacs-title-format))))

(defun command-line-merge (_switch)
  "Enter a graphical `ediff' merge (with ancestor) from the command line.

_SWITCH contains the switch string that invoked this function if it was called
from the command-line switch handler."

  (let ((base (pop command-line-args-left))
        (sccs (pop command-line-args-left))
        (mine (pop command-line-args-left))
        (merg (pop command-line-args-left)))
    (ediff-merge-with-ancestor sccs mine base () merg)))

;; =========================================================
;; Interactive Functions:
;; =========================================================

(defun backward-other-frame (&optional count)
  "Select another frame in reverse cyclic ordering of windows.

Optionally, COUNT may be given to skip more than one frame at a time."
  (interactive "p")
  (other-frame (- (if (null count) 1 count))))

(defun insert-date (&optional _arg)
  "Insert a date as Matt likes into a file."

  ;; This function exists because I want to map M-+ so that it enters a time
  ;; into my file.
  (interactive "P")
  (insert (format-time-string "%A, %e %B %Y" (current-time))))

(defun make-cli-frame ()
  "Create a new frame to have one large window, covering 2/3 of the frame width,
with 2 smaller windows (stacked vertically) to the right of it.  The idea is to
have a large space for a CLI, with two smaller windows for notes, etc."
  (interactive)
  (reset-to-cli-frame t))

(defun reset-to-cli-frame (&optional make-new-frame tgt-frame-width)
  "Re-organize a frame to have one large window, covering 2/3 of the frame
width, with 2 smaller windows (stacked vertically) to the right of it.  The idea
is to have a large space for a CLI, with two smaller windows for notes, etc.

If MAKE-NEW-FRAME is unset, the currently selected frame will be reorganized.
If TGT-FRAME-WIDTH is unset, a window large enough to fit 240 columns will be
 used."

  (interactive)

  (unless tgt-frame-width (setq tgt-frame-width (ideal-frame-width 3)))

  (when (terminal-frame-p)
    (when (/= tgt-frame-width (frame-parameter (selected-frame) 'width))
      (user-error
       (format "Frame width must be %d on TTY to use this layout"
               tgt-frame-width))))

  (let* ((frame (if make-new-frame (make-frame) (selected-frame)))
         (terminal-frame-p (terminal-frame-p frame)))
    (unless terminal-frame-p
      (set-frame-size frame tgt-frame-width (frame-parameter frame 'height)))

    (if make-new-frame (select-frame frame) (delete-other-windows))

    ;; Split-window takes the scrollbar (etc) into account (only on the GUI??),
    ;; so we must as well.  Use a negative number to explicitly specify the
    ;; right side window's size.
    (split-window (selected-window)
                  (* -1 (+ ideal-window-columns 1
                           (if terminal-frame-p 0 scroll-bar-fudge)))
                  t)

    (other-window 1) ;; Split the smaller window vertically.
    (split-window)))

(defun make-reversed-frame ()
  "Return a newly created frame where the background and foreground colors are
the reverse of the currently selected frame.  The frame must not be a terminal
frame."
  (interactive)

  (let* ((frame (selected-frame))
         (bg-mode (frame-parameter frame 'background-mode)))
    ;; This check is mostly because some terminals don't like to have their
    ;; background changed, but also because some terminals rely on EVs to
    ;; interrogate the background mode.
    (if (terminal-frame-p frame)
        (user-error "Current display must not be a terminal"))

    ;; This is not OK...
    (if (null bg-mode) (error "Current background mode is unknown"))

    (setq frame
          (make-frame
           `((foreground-color . ,(frame-parameter frame 'background-color))
             (background-color . ,(frame-parameter frame 'foreground-color))
             (background-mode  . ,(if (eq 'dark bg-mode) 'light 'dark)))))
    (select-frame frame)))

(defun print-features-list ()
  "Print a sorted list of the currently loaded features.

This is something that I use enough during the course of Emacs initialization
optimization that I have implemented it as an interactive function.  Simply
displaying `features' (by default) only shows a handful of the features, and
they are symbols, so they are not sorted alphabetically."
  (interactive)
  (message "Current feature set: %s"
           (sort (mapcar #'symbol-name features) #'string<)))

(defun renew-kerberos-ticket (krb5-domain)
  "Renew the Kerberos ticket for the given domain, given by KRB5-DOMAIN.

On a system where crontab is not available, Emacs can do it for us.  The only
reason I'd even have a session running for an extended period of time is because
of Emacs anyway, so I may as well renew it here, and then there doesn't need to
be a crontab on every machine."

  (interactive (list (read-from-minibuffer "Kerberos domain: " "VMWARE.COM")))

  (let ((command `("/usr/bin/kinit"
                   ,(concat (getenv "USER") "@" krb5-domain)
                    "-k" "-t" ,(concat (getenv "HOME") "/.krb5/"
                                       krb5-domain ".keytab"))))
  (process-send-eof ; Send EOF just in case some accident happens, and asks for
                    ; user input.
   (make-process
    :name "kinit"

    ;; Display command output in the *Messages* buffer.
    :filter
    #'(lambda (proc string)
        (dolist (out-line (split-string string "\n"))
          (unless (string= out-line "")
            (message "%s: %s" (process-name proc) out-line))))

    ;; Usually, kinit produces no output (on success), so note the return
    ;; status, and execution time for the user to make sense of what's going on.
    :sentinel
    #'(lambda (proc _event)
        (unless (process-live-p proc)
          (message "%s: completed at %s with status %d"
                   (process-name proc)
                   (format-time-string "%FT%T%z" (current-time))
                   (process-exit-status proc))
          (delete-process proc)))
    :command command))))

(defun restore-last-desktop-frameset ()
  "Restore the last frameset encountered by `desktop-read'."
  (interactive)

  (if (null last-restored-frameset)
      (user-error "No desktop was loaded, or there was no frameset present"))

  ;; TODO: We could probably configure this with interactive parameters to keep
  ;; frames, reuse them, and so on.
  (frameset-restore last-restored-frameset :reuse-frames nil :cleanup-frames nil
                    :force-display nil :force-onscreen t))

(defun set-active-frame-width-for-parallel-windows (window-count)
  "Set the width of the frame to allow WINDOW-COUNT parallel windows to have a
uniform `ideal-window-columns' column width"

  (interactive
   (list (read-number
          (format "Enter desired width (in %d column windows): "
                  ideal-window-columns))))

  (when (> 1 window-count) (user-error "Window count less than 1"))
  ;; Note: ignoring "too many windows" problems for now.

  (set-frame-size
   (selected-frame)
   (ideal-frame-width window-count)
   (frame-parameter (selected-frame) 'height))
  (balance-windows))

(defun sort-buffers ()
  "Re-order the buffers alphabetically by their path."

  (interactive)
  (dolist (cur (buffer-list-sorted-by-path))
    (bury-buffer cur))
  (if (compat-called-interactively-p) (list-buffers)))

(defun term-named (name-prefix program)
  "Start a terminal-emulator in a new buffer, prefixed by NAME-PREFIX.  The
buffer is in Term mode; see `term-mode' for the commands to use in that buffer.

\\<term-raw-map>Type \\[switch-to-buffer] to switch to another buffer."
  (interactive
   (progn
     (require 'term)
     (list (read-from-minibuffer "Prefix (may be empty): ")
           (read-from-minibuffer "Run program: "
                                 (or explicit-shell-file-name
                                     (getenv "ESHELL" (selected-frame))
                                     (getenv "SHELL" (selected-frame))
                                     "/bin/sh")))))

  (let ((term-name
         (if (string= "" name-prefix)
             "terminal"
           (format "terminal-%s" name-prefix))))
    (set-buffer (make-term term-name program))
    (term-mode)
    (term-char-mode)
    (switch-to-buffer (format "*%s*" term-name))))

;; Up to (at least) Emacs v29, term-mode hangs when pasting via XTerm.  See bug
;; 49253 for details.  I'm re-binding the offending event handler to fix prior
;; versions of this function.
(defun term--xterm-paste-fixed (event)
   "Insert the text pasted in an XTerm bracketed paste operation."
   (interactive "e")
   (term-send-raw-string (nth 1 event)))

(defun update-custom-autoloads ()
  "Generate the autoload file for my custom Emacs packages.

The filename of this definition file is defined by `custom-loaddefs-file'."
  (interactive)

  (require 'autoload)
  (let ((updated-something nil))
    (mapc #'(lambda (file)
              (when (file-exists-p file)
                (update-file-autoloads file t custom-loaddefs-file)
                (setq updated-something t)))
          known-elisp-files)
    (if updated-something
        (message "Updated autoloads in '%s'" custom-loaddefs-file)
      (warn "No files found to update in autoloads"))))

;; =========================================================
;; The Kitchen Sink (TKS):
;; =========================================================

(defun adjust-font-for-hires (font-name)
  "Detect a high resolution screen, and scale FONT-NAME as needed."
  (if (not (use-high-dpi-p)) font-name ;; TODO: Check for high-res...
    (let ((spec (font-spec :name font-name)))
      (font-put spec :size (round (* 2.5 11)))
      ;; Assuming this is used for default-frame-alist (or something), convert
      ;; the font-spec back into an X font descriptor string.
      (font-xlfd-name spec))))

(defun buffer-list-sorted-by-path ()
  "Gather a list of buffers, ordered by their file name (see ‘sort-buffers’)."

  (sort (buffer-list)
        #'(lambda (o1 o2)
            (let ((buf-file1 (buffer-file-name o1))
                  (buf-file2 (buffer-file-name o2)))
              (cond
               ((and (null buf-file1) (null buf-file2))
                (string<
                 (downcase (buffer-name o1))
                 (downcase (buffer-name o2))))
               ((null buf-file1) nil)
               ((null buf-file2) t)
               (t (string< buf-file1 buf-file2)))))))

(defun Buffer-menu-revert-unmodified-buffers ()
  "Update all marked buffers that have not been modified."
  (interactive)
  (save-excursion
    (let ((buffers (mapcan #'(lambda (buf)
                               (if (not (buffer-modified-p buf)) `(,buf)))
                           (Buffer-menu-marked-buffers))))
      (dolist (buf buffers buffers)
        (set-buffer buf)
        (revert-buffer :ignore-auto :noconfirm :preserve-modes)))))

(defun dark-background-p (frame)
  "Determine if Emacs considers the background color to be `dark'.

As different frames have different backgrounds, the FRAME parameter specifies
which frame to check."
  (eq 'dark (cdr (assq 'background-mode (frame-parameters frame)))))

(defun desktop-restore-fileless-buffer (_buffer-filename
                                        buffer-name
                                        _buffer-misc)
  "Restore a buffer to the desktop that is not backed by a file.

This method is like `desktop-restore-file-buffer', and so BUFFER-FILENAME
specifies a file (which will be ignored, and should be NIL), BUFFER-NAME
specifies the name of the buffer that will be created, and BUFFER-MISC is any
additional data."
  ;; This variable only exists when the desktop is actively being processed, so
  ;; ignore warnings about it being a free variable.
  (eval-when-compile (defvar desktop-buffer-major-mode))

  (let ((buf (get-buffer-create buffer-name)))
    (switch-to-buffer buf)
    (unless (eq major-mode desktop-buffer-major-mode)
      (funcall desktop-buffer-major-mode))
    buf))

(defun find-first-defined-font (default-font-name font-names &optional display)
  "Search the provided list of font name (strings, named in FONT-NAMES),
returning the name of the first font that Emacs can find on this system.  If it
finds no fonts, it uses the DEFAULT-FONT-NAME.  This uses the specified DISPLAY,
or the current frame's display, if none was specified."

  (if (null font-names) default-font-name

    (let ((display         (or display (selected-frame)))
          (font-name       (car font-names))
          (rest-font-names (cdr font-names)))
      (if (compat-font-exists-p font-name display) font-name ;; <- Found it.
        (if (null rest-font-names) default-font-name ;; <- Found nothing...
          ;; Keep searching...
          (find-first-defined-font
           default-font-name
           rest-font-names
           display))))))

(defun ideal-frame-width (window-count)
  "Compute the width in `columns' that a frame must be to accommodate
WINDOW-COUNT windows horizontally.

Note that on a TTY, columns is exact, but on a GUI, the columns is whatever
number it takes to make the actual windows inside the frame be the correct
width."

  ;; We compute the number of chars for all columns, plus one "\" column for
  ;; when lines wrap, plus window-count columns (minus one) for the divider
  ;; between windows.  On a GUI, this divider column is given an approximate
  ;; value (5), and on TTY, it's always a single character.
  (+ (* ideal-window-columns window-count)
     window-count
     (* (if (terminal-frame-p) 1 scroll-bar-fudge) (- window-count 1))))

(defun get-macos-terminal-bg-mode ()
  "Retrieves the background `mode' for the Terminal application on MacOS."

  ;; Note: "window 1" is merely the Terminal window closest to the foreground.
  ;; There is a chance, I suppose, that Emacs could start up, move to the
  ;; background, and analyze the wrong window (also only a problem if the other
  ;; terminal is a different color).
  (let* ((osascript
          (concat "osascript -e "
                  "'Tell application \"Terminal\" to get background color of "
                  "window 1'"))
         (rgb-list (mapcar
                    'string-to-number
                    (split-string (shell-command-to-string osascript) ","))))
    ;; Assuming 65536 values per R, G, and B, this is 98304=(R+B+G)/2.
    (if (< (apply '+ rgb-list) 98304) 'dark 'light)))

(defun on-org-clock-in ()
  "Execute operations via the `org-clock-in-hook'.

The main purpose of this is to set a timer for some amount of time after
starting a clock that I could not possibly still be `clocked in.'  The timer
will automatically clock me out."
  (let ((i-died
         #'(lambda ()
             (message "I can see you died while clocked in; clocking you out")
             (org-clock-out))))
    (setq org-i-died-clock-timer (run-at-time "8 hours" nil i-died))))

(defun on-org-clock-out ()
  "Execute operations via the `org-clock-out-hook'.

When clocking out, this makes sure to remove the timer set in
`on-org-clock-in', and reset the global state."
  (unless (null org-i-died-clock-timer)
    (cancel-timer org-i-died-clock-timer)
    (setq org-i-died-clock-timer nil)))

(defun on-server-state-change ()
  "Modifies Emacs when the state of the server changes.

This function can be thought of as a hook for `server-mode'.  Currently, it
changes the title of the running Emacs instance to make it obvious that you are
using Emacs server (rather than a stand-alone instance.  See `server-title-mark'
for configuration of this behavior."

  ;; First, strip off the server marking, as `server-start' runs when the server
  ;; start, or exits.  This also helps avoid adding multiple markers when
  ;; something silly happens like multiple start invocations.
  (if (string-match (regexp-quote server-title-mark) emacs-title-format)
      (setq emacs-title-format (replace-match "" t t emacs-title-format)))

  ;; TODO: This only works once.  If the server name changes, the above
  ;; `string-match' needs to know the old server name (so it can be removed).
  ;; Temporarily, we are stomping on the user-configured setting to make sure
  ;; that works, but the %N is now gone...
  (if (string-match "\\b%N\\b" server-title-mark)
      (setq server-title-mark
            (replace-match server-name t t server-title-mark)))

  (if server-process
      (setq emacs-title-format (concat emacs-title-format server-title-mark)))

  ;; TODO: This doesn't update when the server stops until something (anything)
  ;; changes, for example, changing buffers.  Don't know why.
  (set-emacs-title-format emacs-title-format))

(defun set-bg-mode-from-colorfgbg (frame)
  "Set the FRAME background-mode based on the `COLORFGBG' environment variable.

Only XTerm seems to properly inform Emacs what its color scheme is.  For other
terminals, we can check this COLORFGBG environment variable.  Using EVs are
dicey, and a last resort.

This function is only reliable when FRAME is a terminal frame.  Otherwise, UI
frames started from emacsclient in daemon-based processes will report
environment variables for the terminal that may not match the actual color
scheme of the Emacs UI (e.g., when set via Xdefaults)."
  ;; First check the frame's environment, then check the process environment.
  ;; If there is no frame environment, the current frame is on the same terminal
  ;; window as the one that provided `process-environment'.
  (let ((color-fgbg (or (getenv "COLORFGBG" frame) (getenv "COLORFGBG"))))
    (when (and color-fgbg
               ;; Only doing this for Konsole at the moment.
               (or (getenv "KONSOLE_PROFILE_NAME" frame)
                   (getenv "KONSOLE_PROFILE_NAME")))
      (let ((fg-color-16 (string-to-number
                          (car (split-string color-fgbg ";" )))))
        (modify-frame-parameters
         frame
         `((background-mode . ,(if (< 8 fg-color-16) 'dark 'light))))))))

(defun set-emacs-title-format (title-format)
  "Set the title for Emacs frames (iconized or not).

The parameter TITLE-FORMAT should be specified as in `frame-title-format'."

  (setq frame-title-format title-format
        icon-title-format  title-format))

(defun setup-desktop-restore-transient-buffer (buf-major-mode
                                               &optional
                                               init-hook
                                               desktop-restore-func)
  "Enables `desktop-read' to restore buffers with no file.

The desktop restoration process does not typically save or restore buffers that
are not associated with an underlying file.  This can be annoying for certain
desktops where the same layout is desired, for example, if the compilation
buffer should always be on frame 0, top left window.  This function enables such
restoration in a generic manner, so any buffers that apply can be saved and
restored.

The BUF-MAJOR-MODE parameter should specify the major mode of the buffer being
processed.  INIT-HOOK optionally specifies the name of the hook that
BUF-MAJOR-MODE runs during initialization (this is needed for the saving
process).  If unspecified, it will be generated from BUF-MAJOR-MODE and '-hook'.
DESKTOP-RESTORE-FUNC specifies the function that will be run during desktop
restoration, and it is optional.  It should have the same behavior as
`desktop-restore-file-buffer'."

;; TODO!  In Emacs 21 and below, this must be done with `desktop-buffer-handler'
;; instead.  This shouldn't be too hard.
(version-when (< 21 emacs-major-version)
  (let ((mode-hook (or init-hook
                       (intern (concat (symbol-name buf-major-mode) "-hook")))))
    (add-hook mode-hook #'(lambda () (setq desktop-save-buffer t)))
    (add-to-list 'desktop-buffer-mode-handlers
                 (cons buf-major-mode
                       (or desktop-restore-func
                           #'desktop-restore-fileless-buffer)))))
) ;; End TODO

(defun use-high-dpi-p ()
  "Determine if Emacs should be `high-DPI' on the current montitor."
  (version-when (and (< 23 emacs-major-version) (eq system-type 'windows-nt))
    ;; The monitor geometry is (x, y, width, height) in pixels, so ask for the
    ;; 3rd (height) zero-based index, and compare it against a "low-res"
    ;; 1920x1200 type of monitor.  Do not do this before Emacs 23, since the
    ;; dependent font-spec utilities don't exist yet.
    ;;
    ;; Only Windows requires modification, as MacOS ("Darwin") handles this
    ;; automatically, and I modify settings under Xresources for X Windows.
    (> (nth 3 (alist-get 'geometry (frame-monitor-attributes))) 1200)))

;; -----------------------------------------------------------------------------
;; GO CONFIGURE!!
;; -----------------------------------------------------------------------------

;; Setting `file-name-handler-alist' to NIL seems to shave about 500ms of init
;; times (average, where dot-files are over NFS across the USA).  This is
;; because every .el/.elc file load will have to run through these regular
;; expressions.  I have disabled this in the start-up environment for some
;; optimization.
;;
;; Setting `gc-cons-threshold' prevents GC from happening very often during
;; start-up.  The default is 800,000 bytes, and I've set it to 16MB before GC
;; occurs.
(let ((file-name-handler-alist nil)
      (gc-cons-threshold (* 16 1024 1024)))
  (custom-configure-emacs))

;;; emacs.el ends here
