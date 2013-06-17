
;; emacs.el
;; This is my Emacs initialization script.  It works with Emacs 19-24
;; and greater.
;; Matt Bisson	5/18/2013

;; -----------------------------------------------------------------------------
;; Top-level configuration routines:
;; -----------------------------------------------------------------------------

(defun custom-configure-emacs ()
  "This is the full configuration/customization function for Emacs."

  (provide-customized-fetures)

  (set-key-bindings)

  ;; Add some file types to interpret
  (associate-file-types)

  ;; Set some configuration variables
  (setq
   ;; Some C specific modes
   c-backslash-column           45
   c-default-style              "bsd"
   c-basic-offset               (if (getenv "VMWARE_CODE") 3 4)
   c-tab-always-indent          nil
   tab-width                    4
   compilation-scroll-output    t

   ;; Longer selections tend to make the mode-line too long (with battery)
;  display-time-24hr-format     t
;  display-time-day-and-date    t
;  display-time-format          "%a-%Y/%m/%d-%H:%M"
   display-time-format          "%a/%H:%M"

   ;; I've already read it
   inhibit-startup-message      t

   ;; Don't like the initial stuff in my scratch buffer
   initial-scratch-message      nil

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

  ;; Set everything up for us to use a desktop (saved session) if asked.
  (let ((env-dt-dir (getenv "EMACS_DESKTOP_DIR")))
    (if (and env-dt-dir
             (file-exists-p env-dt-dir)
             (not (member "--no-desktop" command-line-args)))
        (desktop-change-dir env-dt-dir)))

  ;; Some additional major customizations based on display capabilities:
  (if (not terminal-frame)
      (custom-configure-for-xwindows)
      (custom-configure-for-terminal))

  (if (< 19 emacs-major-version) (custom-configure-emacs-20)))

(defun custom-configure-emacs-20 ()
  "Customizations that are only applicable to Emacs 20 and above."

  (show-paren-mode 1)

  ;; TODO: Can't seem to do this any other way...
  (if (getenv "USE_HOLLY")
      (custom-set-variables '(tab-width        4))
      (custom-set-variables '(indent-tabs-mode nil)))

  (if (< 20 emacs-major-version)
      (custom-configure-emacs-21)
      (display-battery)))

(defun custom-configure-emacs-21 ()
  "Customizations that are only applicable to Emacs 21 and above."

  ;; Restore functionality s.t. down adds lines to the end of the file
  (setq next-line-add-newlines t)

  (blink-cursor-mode -1)
  (tool-bar-mode -1)

  (condition-case nil
      (display-battery-mode 1)
    (error nil)) ; Ignore errors if AC powered!

  (if (< 21 emacs-major-version) (custom-configure-emacs-22)))

(defun custom-configure-emacs-22 ()
  "Customizations that are only applicable to Emacs 22 and above."

  ;; Org-mode only exists in version 22 and above.
  (setq
   auto-mode-alist       (append '(("\\.org$" . org-mode)) auto-mode-alist)
   org-startup-truncated nil))

;; -----------------------------------------------------------------------------
;; Font coloring for dark Emacs backgrounds:
;; -----------------------------------------------------------------------------

(defun bg-dark-font-lock-faces ()
  "Set font-lock faces for Emacs when the background is dark."

  (make-face-italic    'font-lock-comment-face       nil t)
  (set-face-foreground 'font-lock-comment-face       "Navy")
  (make-face-bold      'font-lock-keyword-face       nil t)
  (set-face-foreground 'font-lock-keyword-face       "MediumAquamarine")
  (set-face-foreground 'font-lock-type-face          "Goldenrod")
  (set-face-foreground 'font-lock-variable-name-face "Salmon")

  (if (< 19 emacs-major-version) (bg-dark-font-lock-faces-20)))

(defun bg-dark-font-lock-faces-20 ()
  "Set font-lock faces for Emacs 20+ when the background is dark."

  (if (and (= 20 emacs-major-version) (< 2 emacs-minor-version))
      (set-face-foreground 'font-lock-constant-face "Coral")
      (set-face-foreground 'sh-heredoc              "Chocolate"))

  (make-face-bold      'font-lock-builtin-face       nil t)
  (set-face-foreground 'font-lock-builtin-face       "DodgerBlue4")
  (make-face-italic    'font-lock-function-name-face nil t)
  (set-face-foreground 'font-lock-function-name-face "OrangeRed2")
  (set-face-foreground 'font-lock-string-face        "Chocolate")
  (set-face-background 'gdb-selection                "MidnightBlue")
  (set-face-background 'highlight                    "CadetBlue")
  (set-face-background 'region                       "Firebrick")

  (if (< 20 emacs-major-version) (bg-dark-font-lock-faces-21)))

(defun bg-dark-font-lock-faces-21 ()
  "Set font-lock faces for Emacs 21+ when the background is dark."

  (make-face-italic    'font-lock-doc-face           nil t)
  (set-face-foreground 'font-lock-doc-face           "Navy"))

;; -----------------------------------------------------------------------------
;; Font coloring for light Emacs backgrounds:
;; -----------------------------------------------------------------------------

(defun bg-light-font-lock-faces ()
  "Set font-lock faces for Emacs when the background is light."

  (make-face-italic    'font-lock-comment-face       nil t)
  (set-face-foreground 'font-lock-comment-face       "DarkGreen")
  (make-face-bold      'font-lock-keyword-face       nil t)
  (set-face-foreground 'font-lock-keyword-face       "FireBrick")
  (set-face-foreground 'font-lock-type-face          "Maroon")
  (set-face-foreground 'font-lock-variable-name-face "SteelBlue")

  (if (< 19 emacs-major-version) (bg-light-font-lock-faces-20)))

(defun bg-light-font-lock-faces-20 ()
  "Set font-lock faces for Emacs 20+ when the background is light."

  (if (and (= 20 emacs-major-version) (< 2 emacs-minor-version))
      (set-face-foreground 'font-lock-constant-face "Burlywood4")
      (set-face-foreground 'sh-heredoc              "Chocolate"))

  (make-face-bold      'font-lock-builtin-face       nil t)
  (set-face-foreground 'font-lock-builtin-face       "DodgerBlue4")
  (make-face-italic    'font-lock-function-name-face nil t)
  (set-face-foreground 'font-lock-function-name-face "OrangeRed2")
  (set-face-foreground 'font-lock-string-face        "Chocolate")
  (set-face-background 'gdb-selection                "DarkSeaGreen3")
  (set-face-background 'highlight                    "CadetBlue")
  (set-face-background 'region                       "LightSteelBlue3")

  (if (< 20 emacs-major-version) (bg-light-font-lock-faces-21)))

(defun bg-light-font-lock-faces-21 ()
  "Set font-lock faces for Emacs 21+ when the background is light."

  (make-face-italic    'font-lock-doc-face nil t)
  (set-face-foreground 'font-lock-doc-face "DarkGreen"))

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

This is not strict, nor does it need to be unique.  The main purpose of this is for when I have more than one Emacs environment running.  This way, I can put a friendly name in the title for easy identification."

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

  (load-file "/ccs/etc/dotfiles/.emacs")
  (setq inferior-lisp-program "acl")
  (setq printer-name          "escher")
  (setq ps-printer-name       "escher")

  ;; Set up my mail preferences
  (setq rmail-file-name       "~/mail/mbisson.ccs.neu.edu"))

(defun configure-palm-dev-env ()
  "Configure Emacs to develop for Palm Hollywood (Foleo)."

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

(defun configure-vmware-dev-env ()
  "Configure Emacs to develop VMware code."

  (let ((srcdir (getenv "DR_SRCDIR_UNIX")))
    (if (and srcdir (file-exists-p srcdir))
        (setq compile-command (concat "make -C " srcdir " server")))))

;; -----------------------------------------------------------------------------
;; External package loaders:
;; -----------------------------------------------------------------------------

(defun provide-customized-fetures ()
  "Load external packages and those that are not enabled by default."

  (if (< 19 emacs-major-version) (provide-customized-fetures-20))

  ;; All this ccs stuff happens if the dotfile exists...
  (if (file-exists-p "/ccs/etc/dotfiles/.emacs")
      (configure-ccs-dev-env)
      ;; TODO: Illogical location for this...
      (setq inferior-lisp-program "sbcl --noinform"))

  ;; Set up my Palm development environment
  (if (getenv "USE_HOLLY") (configure-palm-dev-env))

  (if (getenv "VMWARE_CODE") (configure-vmware-dev-env)))

(defun provide-customized-fetures-20 ()
  "Load features that only work with Emacs 20 and above."

  ;; Enable wheelmouse support by default
  (if (not terminal-frame) (load "mwheel" t))

  ;; Load my MUD/MOO stuff
  (if (file-exists-p "~/elisp/mud.elc") (load-file "~/elisp/mud.elc"))

  ;; Load CSCOPE stuff
  (if (file-exists-p "~/elisp/xcscope.elc") (load-file "~/elisp/xcscope.elc"))

  ;; Load Batch-script stuff
  (if (file-exists-p "~/elisp/dos.elc") (load-file "~/elisp/dos.elc"))

  ;; Load GTAGS stuff
  (if (file-exists-p "~/elisp/gtags.elc") (load-file "~/elisp/gtags.elc"))

  (if (file-exists-p "~/elisp/xgtags.elc")
      (progn
        (load-file "~/elisp/xgtags.elc")
        (add-hook    'c-mode-common-hook (lambda () (xgtags-mode 1)))
        (remove-hook 'c-mode-hook        'cscope:hook)
        (remove-hook 'c++-mode-hook      'cscope:hook)))

  ;; I customized SQL mode
  (if (file-exists-p "~/elisp/sql.elc") (load-file "~/elisp/sql.elc"))

  ;; Get fancy with wiki content
  (if (file-exists-p "~/elisp/media-wiki.elc")
      (load-file "~/elisp/media-wiki.elc"))

  ;; Perforce is a horrible version control system -- it has an Emacs mode
  (if (file-exists-p "~/elisp/p4.elc") (load-file "~/elisp/p4.elc"))) ;; 2 slow

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

  (global-set-key "\C-h" 'delete-backward-char)
  (global-set-key "\C-?" 'delete-char))

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
             ("\\.r$"       . c-mode)
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
             ("\\.S$"       . sh-mode)
             ("\\.sh$"      . sh-mode)
             ("\\.txt$"     . text-mode))
           auto-mode-alist)))

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

  ;; I just find syntax highlighting annoying on the terminal
  ;; TODO: XEmacs?!?
  (if (not running-xemacs) (global-font-lock-mode -1)))

(defun custom-configure-for-xwindows ()
  "Configure settings that only apply to Emacs when run in (X) Windows."

  (setq
   ;; Why was this turned off in Emacs 24.1?
   mouse-drag-copy-region       t

   ;; Quite an assumption, but there's really no uniform way to tell (even the
   ;; Emacs docs say this).
   focus-follows-mouse          (eq system-type 'windows-nt)

   ;; Let's set up some universal window (frame) attributes.
   default-frame-alist
   (cons
    (cons 'cursor-type 'box)

    (if (eq system-type 'windows-nt)
        (list
         ;; Preserve the background at configuration time for future
         ;; frames.  Because there's no .Xdefaults, we rely on
         ;; command-line arguments.
         (cons 'background-color
               (frame-parameter (selected-frame) 'background-color))

         ; TODO: Make font selection dynamically select the first one that is
         ;       available.
         ;         Family: outline-consolas
         ;          Width: normal
         ;         Height: 90
         ;         Weight: normal
         ;          Slant: normal
         ;     Foreground: SystemWindowText
         ;     Background: SystemWindow
         ;      Underline: nil
         ;       Overline: nil
         ; Strike-through: nil
         ;            Box: nil
         ;        Inverse: nil
         ;        Stipple: nil
         ;        Inherit: unspecified
         (cons 'font
               ;; Lucida Console 8 (thinner)
;;             "-*-Lucida Console-*-*-*-*-11-*-*-*-c-*-iso8859-1"
               ;; Consolas 11
               "-*-Consolas-*-r-*-*-12-90-*-*-c-*-iso8859-1"
               )
         (cons 'height 70)
         (cons 'width  81))
        (list
         (if (eq system-type 'cygwin)
             (cons 'font
;;                 "8x13"
                   "Consolas 9"
                   )
             (cons 'font
;;                 "-Misc-Fixed-normal-normal-normal-*-13-*-*-*-c-*-iso10646-1"
                   "8x13"
;;                 "Nimbus Mono L 10"
                   ))
         (cons 'height (frame-parameter (selected-frame) 'height))
         (cons 'width  (frame-parameter (selected-frame) 'width))))))

  ;; Print the name of the visited file in the title of the window...
  (set-emacs-title-format "%b")

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
      (if (eq 'light
              (cdr (assq 'background-mode (frame-parameters (selected-frame)))))
          (bg-light-font-lock-faces)
        (bg-dark-font-lock-faces))))

(defun gud-kill-buffer ()
  "Get rid of the GDB highlight.  This should be added as a hook for when the
GDB buffer goes away."

  (if (eq major-mode 'gud-mode) (delete-overlay gud-overlay)))

(defun insert-date (&optional arg)
  "This function exists because I want to map M-+ so that it enters a time into
my file."

  (interactive "P")
  (insert (format-time-string "%A, %e %B %Y" (current-time))))

(defun set-emacs-title-format (title-format)
  "Set the title for Emacs frames (iconized or not)."

  (setq frame-title-format title-format
        icon-title-format  title-format))

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
