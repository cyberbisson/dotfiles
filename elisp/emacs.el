
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

        ;; Load Batch-script stuff
        (if (file-exists-p "~/elisp/dos.elc")
            (progn
                (load-file "~/elisp/dos.elc")

                ;; TODO: What if it exists, but we didn't load it here?
                (setq auto-mode-alist
                     (append '(("\\.bat$" . dos-mode) ("\\.cmd$" . dos-mode))
                             auto-mode-alist))
            )
        )

        ;; Load GTAGS stuff
        (if (file-exists-p "~/elisp/gtags.elc")
            (load-file "~/elisp/gtags.elc")
        )
        (if (file-exists-p "~/elisp/xgtags.elc")
            (progn
                (load-file "~/elisp/xgtags.elc")
                (add-hook    'c-mode-common-hook (lambda () (xgtags-mode 1)))
                (remove-hook 'c-mode-hook        'cscope:hook)
                (remove-hook 'c++-mode-hook      'cscope:hook)
            )
        )

        ;; I customized SQL mode
        (if (file-exists-p "~/elisp/sql.elc")
            (load-file "~/elisp/sql.elc")
        )

        ;; Get fancy with wiki content
        (if (file-exists-p "~/elisp/media-wiki.elc")
            (progn
                (load-file "~/elisp/media-wiki.elc")
                (setq auto-mode-alist
                     (append '(("\\.wiki$" . mediawiki-mode)) auto-mode-alist))
            )
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
    (progn
        (defalias 'run-lisp 'inferior-lisp)
        (setq inferior-lisp-program "sbcl --noinform")
    )
)

;; Set up my Palm development environment
(if (getenv "USE_HOLLY")
    (progn
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
                              ("\\.r$"       . c-mode)
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
                              ("\\.txt$"     . text-mode)
                              ;; Should check if this is even loaded...
                            )
                            auto-mode-alist)

    ;; Some C specific modes
    c-backslash-column           45
    c-default-style              "bsd"
    c-basic-offset               (if (eq nil (getenv "VMWARE_CODE")) 4 3)
    c-tab-always-indent          nil
    tab-width                    4
    compilation-scroll-output    t

    ;; Eehh... Why not?
    display-time-24hr-format     t

    ;; Go all out with the font colors
    font-lock-maximum-decoration t

    ;; I've already read it
    inhibit-startup-message      t

    ;; Don't like the initial stuff in my scratch buffer
    initial-scratch-message      nil

    ;; Why was this turned off in emacs 24.1?
    mouse-drag-copy-region       t

    ;; Only Enter and C-g exit the search
    search-exit-option           nil

    ;; Print the name of the visited file in the title of the window...
    frame-title-format           "%b"
)

;; Org-mode only exists in version 22 and above.
(if (< 21 emacs-major-version)
    (progn
        (setq auto-mode-alist (append '(("\\.org$" . org-mode)) auto-mode-alist))
        (setq org-startup-truncated nil)
        (setq initial-scratch-message nil)
    )
)

;; Syntax highlighting
(if (not running-xemacs)
    (global-font-lock-mode 1)
)

;; Show selected marked area
(transient-mark-mode -1)

;; Get rid of shift moving the mark around...
(setq shift-select-mode nil)

;; I want to know what column and line I am in
(column-number-mode 1)
(line-number-mode   1)

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
                        ;; Lucida Console 8 thinner
;;                      "-*-Lucida Console-*-*-*-*-11-*-*-*-c-*-iso8859-1"
                        ;; Consolas 11
                        "-*-Consolas-*-r-*-*-12-90-*-*-c-*-iso8859-1"
                        )
                    (cons 'height 70)
                    (cons 'width  81)
                )
                (list
                    (if (eq system-type 'cygwin)
                        (cons 'font
;;                          "8x13"
                            "Consolas 9"
                            )
                        (cons 'font
;;                          "-Misc-Fixed-normal-normal-normal-*-13-*-*-*-c-*-iso10646-1"
                            "8x13"
;;                          "Nimbus Mono L 10"
                            ))
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
                (tool-bar-mode -1)
                (setq next-line-add-newlines t)

                ;; Yes, emacs will do it all...
                (if (< 21 emacs-major-version)
                    (display-battery-mode 1)
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
        (global-set-key [C-tab]       'other-frame)
        (global-set-key [C-S-tab]     (lambda () (interactive) (other-frame -1)))
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

;; By default, this face is UNREADABLE.
(if (or (and (eq 20 emacs-major-version)
             (<   2 emacs-minor-version)) ; And why?
        (< 20 emacs-major-version))
    (make-face 'sh-heredoc)
    (make-face 'font-lock-doc-face)
)

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
                (set-face-foreground 'sh-heredoc              "Chocolate")
            )
            (make-face-italic    'font-lock-doc-face           nil t)
            (set-face-foreground 'font-lock-doc-face           "DarkGreen")
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
                (set-face-foreground 'sh-heredoc              "Chocolate")
            )
            (make-face-italic    'font-lock-doc-face           nil t)
            (set-face-foreground 'font-lock-doc-face           "Navy")
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

;; Set everything up for us to use a desktop (saved session) if asked.
(let ((env-dt-dir (getenv "EMACS_DESKTOP_DIR")))
    (if (and env-dt-dir
             (file-exists-p env-dt-dir)
             (not (member "--no-desktop" command-line-args)))
        (progn
            (desktop-change-dir env-dt-dir)
;           (desktop-save-mode 1)
        )
    )
)

;; It's stupid that this is not the default behavior
(let ((env-server-name (getenv "EMACS_SERVER_FILE")))
    (setq server-name env-server-name)
)

;;
;; Provide some nice GUI tools from the Emacs command-line for diff and merge
;;

(defun command-line-diff (switch)
    "Enter a graphical ediff from the command-line"
    (let ((file1 (pop command-line-args-left))
          (file2 (pop command-line-args-left)))
        (ediff file1 file2)
    )
)

(defun command-line-merge (switch)
    "Enter a graphical ediff merge (with ancestor) from the command line"
    (let ((base (pop command-line-args-left))
          (sccs (pop command-line-args-left))
          (mine (pop command-line-args-left))
          (merg (pop command-line-args-left)))
        (ediff-merge-with-ancestor sccs mine base () merg)
    )
)

(add-to-list 'command-switch-alist '("-diff"  . command-line-diff))
(add-to-list 'command-switch-alist '("-merge" . command-line-merge))

;; Quite an assumption, but there's really no uniform way to tell (even the
;; Emacs docs say this).
(setq focus-follows-mouse (eq system-type 'windows-nt))
