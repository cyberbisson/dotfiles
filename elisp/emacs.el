
;;.emacs
;; This is my emacs initialization script.  It works with Emacs v19 & v20
;; and greater.
;; Matt Bisson	8/16/2000

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; I've only got these files compiled for emacs 20+
(if (< 19 emacs-major-version)
    (progn
        ;; Enable wheelmouse support by default
        (load "mwheel" t)

        ;; Load my MUD/MOO stuff
        (if (file-exists-p "~/elisp/mud.elc")
            (load-file "~/elisp/mud.elc")
        )

        ;; Load CSCOPE stuff
        (if (file-exists-p "~/elisp/xcscope.elc")
            (load-file "~/elisp/xcscope.elc")
        )

        ;; I customized SQL mode
        (if (file-exists-p "~/elisp/sql.elc")
            (load-file "~/elisp/sql.elc")
        )

        ;; Perforce is a horrible version control system -- it has an emacs mode
        ;(if (file-exists-p "~/elisp/p4.elc")
        ;    (load-file "~/elisp/p4.elc")
        ;) ;; 2 slow
    )
)

;; All this ccs stuff happens if the dotfile exists...
(if (file-exists-p "/ccs/etc/dotfiles/.emacs")
    (progn (load-file "/ccs/etc/dotfiles/.emacs")
        (defalias 'run-lisp 'inferior-lisp)
        (setq inferior-lisp-program "acl")
        (setq printer-name          "escher")
        (setq ps-printer-name       "escher")

        ;; Set up my mail preferences
        (setq rmail-file-name       "~/mail/mbisson.ccs.neu.edu")
    )
)

;; Set up my Palm development environment
(if (getenv "USE_HOLLY")
    (progn
        (setq cscope-database-file "/home/mbisson/ws/src/cscope.out")
        (setq compile-command      "make -k -j3 -C /home/mbisson/ws/mail/main")

        ;; Shell environment crap
        (if (string= (getenv "HOLLY_ARCH") "i386-linux")
            ;; We might need this for debugging (only on Intel simulator)...
            (progn
                (setenv "HX_APP_PATH"    (getenv "HOLLY_RESULT_ROOT"))
;               (setenv "HM_DEBUG_LEVEL" "1")
                (setenv "HM_SAMSDIR"     (concat (getenv "HOLLY_RESULT_ROOT")
                                                 "/sams"))
                (setenv "LD_LIBRARY_PATH"
                    (concat (getenv "LD_LIBRARY_PATH")                       ":"
                            (concat (getenv "HOLLY_SYSTEM_ROOT") "/usr/lib") ":"
                            (concat (getenv "HOLLY_RESULT_ROOT") "/fpi")     ":"
                            (concat (getenv "HOLLY_RESULT_ROOT") "/lib")
                    )
                )
            )
            ;; We're debugging on the device.
            (setq gud-gdb-command-name
                  "/opt/holly/toolroot/arm-linux/bin/gdb --annotate=3")
        )
    )
)

;; Set some configuration variables
(setq
    ;; There are no hooks for fundamental mode...
    default-major-mode 'text-mode
    initial-major-mode 'text-mode
    startup-major-mode 'text-mode

    ;; Add some file types to interpret
    auto-mode-alist (append '(("\\.C$"       . c++-mode)
                              ("\\.cc$"      . c++-mode)
                              ("\\.H$"       . c++-mode)
                              ("\\.h$"       . c++-mode)
                              ("\\.hpp$"     . c++-mode)
                              ("\\.i$"       . c++-mode)
                              ("\\.rc$"      . c++-mode)
                              ("[Mm]akefile" . makefile-mode)
                              ("\\.M$"       . nroff-mode)
                              ("\\.ms$"      . nroff-mode)
                              ("sendmail.cf" . sh-mode) ;; Don't ask...
                              ("\\.bas$"     . basic-mode)
                              ("\\.vb$"      . basic-mode)
                              ("\\.vbs$"     . basic-mode)
                              ("\\.bat$"     . batch-mode)
                              ("\\.cmd$"     . batch-mode)
                              ("\\.csh$"     . sh-mode)
                              ("\\.ksh$"     . sh-mode)
                              ("\\.S$"       . sh-mode)
                              ("\\.sh$"      . sh-mode)
                              ("\\.txt$"     . text-mode)
                            )
                            auto-mode-alist)

    ;; Some C specific modes
    c-backslash-column           45
    c-default-style              "bsd"
    c-basic-offset               4
    c-tab-always-indent          nil
    tab-width                    4
    compilation-scroll-output    t

    ;; Eehh... Why not?
    display-time-24hr-format     t

    ;; Go all out with the font colors
    font-lock-maximum-decoration t

    ;; I've already read it
    inhibit-startup-message      t

    ;; Only Enter and C-g exit the search
    search-exit-option           nil

    ;; Print the name of the visited file in the title of the window...
    frame-title-format           "%b"
)

;; Org-mode only exists in version 22 and above.
(if (< 21 emacs-major-version)
    (setq auto-mode-alist (append '(("\\.org$" . org-mode)) auto-mode-alist))
)

;; Syntax highlighting
(if (not running-xemacs)
    (global-font-lock-mode t)
)

;; Show selected marked area
;(transient-mark-mode nil)

;; I want to know what column and line I am in
(column-number-mode t)
(line-number-mode   t)

;; Show me the time, so I can tell how bored I am
(display-time)

;; Let's set up some universal window attributes.  Valid attributes (as of v20)
;; include:  auto-lower, auto-raise, background-color, background-mode,
;; border-color, border-width, buffer-predicate, cursor-color, cursor-type
;; display, display-type, font, foreground-color, height, icon-name, icon-type
;; internal-border-width, left, menu-bar-lines, modeline, mouse-color, name,
;; scroll-bar-width, title, top, unsplittable, vertical-scroll-bars, visibility,
;; width, window-id.
(if (not terminal-frame)
    (setq default-frame-alist
        (cons
            (cons 'cursor-type 'box)

            ;; Preserve backgound colors for all subsequent frames
            (if (eq system-type 'windows-nt)
                (list
                    (cons 'background-color (frame-parameter (selected-frame)
                                             'background-color))
                    (cons 'font ;; Lucida Console 8 thinner
                         "-*-Lucida Console-*-*-*-*-11-*-*-*-c-*-iso8859-1")
                    (cons 'height 70)
                    (cons 'width  81)
                )
                (list
                    (cons 'height (frame-parameter (selected-frame) 'height))
                    (cons 'width  (frame-parameter (selected-frame) 'width))
                )
            )
        )
    )
)

;; If we are at emacs-20, then we can set some variables this way
(if (< 19 emacs-major-version)
    (progn
        (show-paren-mode t)

        ;; Can't seem to do this any other way...
        (if (getenv "USE_HOLLY")
            (custom-set-variables '(tab-width        4))
            (custom-set-variables '(indent-tabs-mode nil))
        )

        (custom-set-variables
            '(blink-cursor-mode nil)
        )

        (if (< 20 emacs-major-version)
            (progn
                (global-set-key [end]  'end-of-buffer)
                (global-set-key [home] 'beginning-of-buffer)
                (global-set-key "\C-c\C-r" 'recompile)
                (tool-bar-mode nil)
                (setq next-line-add-newlines t)

                ;; Yes, emacs will do it all...
                (if (< 21 emacs-major-version)
                    (display-battery-mode)
                    (display-battery)
                )
            )
        )
    )
)

;; Make sure there is no confusion about delete characters
(if terminal-frame
    (progn
        (global-set-key "\C-h"        'delete-backward-char)
        (global-set-key "\C-?"        'delete-char)
    )
    (progn
        ;; The next command won't work unless we create "font-lock" faces
        (load-library "font-lock")
        (global-set-key [delete]      'delete-char)
        (global-set-key [kp-delete]   'delete-char)
        (global-set-key [C-delete]    'kill-word)
        (global-set-key [C-kp-delete] 'kill-word)
    )
)

;; I want to map M-+ so that it enters a time into my file.
(defun insert-date (&optional arg)
    (interactive "P")
    (insert (format-time-string "%A, %e %B %Y" (current-time)))
)

(global-set-key "\M-+" 'insert-date)

;; I can't believe this isn't mapped by default, but...
(global-set-key "\M-p" 'goto-line)

;; Add color to the current GUD line
(make-face 'gdb-selection) 

;; Overlay variable for GUD highlighting
(defvar gud-overlay
    (let* ((ov (make-overlay (point-min) (point-min))))
        (overlay-put ov 'face 'gdb-selection)
        ov
    )

    "Overlay variable for GUD highlighting."
)

;; Highlight current line
(defadvice gud-display-line (after my-gud-highlight act)
    "Highlight current line."
    (let* ((ov gud-overlay)
           (bf (gud-find-file true-file)))
        (save-excursion
            (set-buffer bf)
            (move-overlay ov
                (line-beginning-position)
                (+ (line-end-position) 1)
                (current-buffer))
        )
    )
)

;; Get rid of highlight when the buffer goes away
(defun gud-kill-buffer ()
    (if (eq major-mode 'gud-mode) (delete-overlay gud-overlay))
)

(add-hook 'kill-buffer-hook 'gud-kill-buffer)

;; Give me some nice pretty colors...
(if (not terminal-frame)
(if (eq 'light (cdr (assq 'background-mode (frame-parameters
                                            (selected-frame)))))

    ;; Set colors for a light background
    (progn
        (make-face-italic    'font-lock-comment-face       nil t)
        (set-face-foreground 'font-lock-comment-face       "DarkGreen")
        (make-face-bold      'font-lock-keyword-face       nil t)
        (set-face-foreground 'font-lock-keyword-face       "FireBrick")
        (set-face-foreground 'font-lock-type-face          "Maroon")
        (set-face-foreground 'font-lock-variable-name-face "SteelBlue")

        ;; We can set colors differently in a newer emacs version
        (if (< 19 emacs-major-version) (progn
            (if (and (eq 20 emacs-major-version)
                     (<   2 emacs-minor-version)) ; And why?
                (set-face-foreground 'font-lock-constant-face "Burlywood4")
            )
            (make-face-bold      'font-lock-builtin-face       nil t)
            (set-face-foreground 'font-lock-builtin-face       "DodgerBlue4")
            (make-face-italic    'font-lock-function-name-face nil t)
            (set-face-foreground 'font-lock-function-name-face "OrangeRed2")
            (set-face-foreground 'font-lock-string-face        "Chocolate")
            (set-face-background 'gdb-selection                "DarkSeaGreen3")
            (set-face-background 'highlight                    "CadetBlue")
            (set-face-background 'region                       "LightSteelBlue3"))
        )
    )

    ;; Set colors for a dark background
    (progn
        (make-face-italic    'font-lock-comment-face       nil t)
        (set-face-foreground 'font-lock-comment-face       "Navy")
        (make-face-bold      'font-lock-keyword-face       nil t)
        (set-face-foreground 'font-lock-keyword-face       "MediumAquamarine")
        (set-face-foreground 'font-lock-type-face          "Goldenrod")
        (set-face-foreground 'font-lock-variable-name-face "Salmon")

        ;; We can set colors differently in a newer emacs version
        (if (< 19 emacs-major-version) (progn
            (if (and (eq 20 emacs-major-version)
                     (<   2 emacs-minor-version)) ; And why?
                (set-face-foreground 'font-lock-constant-face "Coral")
            )
            (make-face-bold      'font-lock-builtin-face       nil t)
            (set-face-foreground 'font-lock-builtin-face       "DodgerBlue4")
            (make-face-italic    'font-lock-function-name-face nil t)
            (set-face-foreground 'font-lock-function-name-face "OrangeRed2")
            (set-face-foreground 'font-lock-string-face        "Chocolate")
            (set-face-background 'gdb-selection                "MidnightBlue")
            (set-face-background 'highlight                    "CadetBlue")
            (set-face-background 'region                       "Firebrick"))
        )
    )
))
