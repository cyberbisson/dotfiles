;;; MUD YOUR EMACS!
;;; $Id: mud.el,v 1.1 2005/11/27 19:10:49 mbisson Exp $
;;; Waider, Started April '96
;;;
;;; Easy Mud Access Client System???
;;;
;;; The current version of mud.el is available at
;;; <a href="http://www.waider.ie/hacks/emacs/mud.el">this</a>
;;; location, unless I've changed jobs. If all else fails, try mail to
;;; <a href="mailto:mud@waider.ie">mud@waider.ie</a>
;;;
;;; FIXME LIST
;;; Multiple input mode:
;;; 1. Doesn't set up buffers properly -> Does now, bar some minor stupidity
;;; 2. Easier switching from one mud to the other - menu?
;;; 3. Indication of which mud is the default
;;; 4. Case-insensitive mud names
;;; 5. Fill function should figure out what it's done to the buffer
;;;    size, then traverse the regexps list and hack all the offsets
;;;    accordingly.
;;;
;;; General
;;; More / commands and menu options?
;;; Use font-lock for highlighting? Not sure this is a good idea.

;;; Hacks dfan has inflicted on this version:
;;;  - Command history (alpha quality)
;;;  - Backspace stops at the beginning of the line
;;;  - If point is at the end of the output buffer, keep putting it
;;;    at the end of the buffer when we get more input.
;;;  - User variable for whether URLs are shown in the minibuffer.
;;;  - Topics are colorized uniquely.

;;;
;;; Firewall handler
;;;
;;; Very minimal. Works for the type of firewall where you telnet to a
;;; gateway machine and then telnet out from there; if anyone requires
;;; more firewall support I may hack it in. Maybe. Set the two
;;; variables below to activate firewall climbing.
;;;
;;; This hasn't been expanded since we installed masquerading instead
;;; of fwtk stuff. If you really need something changed here, mail
;;; me at
;;; <a href="mailto:waider@waider.ie">waider@waider.ie</a>
;;;
;;; gw-host is a quoted string, either the FQDN or the IP
;;; gw-port is a number. I dunno what happens if you quote it :)
;;;
(defvar gw-host nil "*Name of your firewall host")
(defvar gw-port nil "*Port to connect to on the firewall host")
(defvar gw-prompt nil "*Firewall's prompt string.")

;;;
;;; Version of open-network-stream that firewalls if the gw-host and
;;; gw-port variables are set.
;;;
(defun mud-open-network-stream( name buffer host port )
  ;; Do clever host stuff here to automagically determine firewall
  ;; requirements?
  (let (proc)
    (if (and gw-host gw-port)
        (progn
          (message "Using firewall to connect.")
          (setq proc (open-network-stream name buffer gw-host gw-port))
          (if proc
              ;; Prod the gateway, if necessary.
              (save-excursion
                (message "Connected to firewall.")
                ;;(process-send-string proc magic-string)
                (message (concat "Connecting to " host ", port " port))
                (process-send-string
                 proc (concat "telnet " host " " port "\r\n"))
                ;; Wait for Connected to... here
                ;;(process-send-string proc "")
                ;;(process-send-string proc "set mode char\r\n")
                )))
      ;;; ELSE no firewalling required
      (setq proc (open-network-stream name buffer host port))
      (message "Connected to host."))
    proc))

;;;
;;; mud.el proper
;;;

;;; Things needed here: (a) Move everything to properties?
;;;                     (b) Add a 'mud-type' flag.
;;;
;;;                 Name        Host                     Port  Login
(defvar mud-list '((Nerdsholm ("boutell.com"             4096  t))
                   (NewtMUD   ("tapaboy.ma.ultranet.com" 4096))
                   )
  "*List of lists, containing Mud name, then (\"host\" port login pass).
Login and pass can be omitted; if login is t, mud-default-user and
mud-default-pass will be used.

Once you've defined a Mud here, you can set various features for that
mud by doing (put 'MUDNAME 'feature value). The Properties menu will
give you a list of currently-set properties and their values,
excluding properties for buffers, windows and the mud process.")

(defvar mud-default 'Nerdsholm "*default mud to log in to.")

;;; Note: all mud-default-* variables map to mud-* properties on the
;;; mud symbols. So, for example, if mud-user is an undefined property
;;; for Nerdsholm, it gets set to mud-default-user.

;;; Auto-Login support
(defvar mud-default-user nil "*default username to connect with.")
(defvar mud-default-pass nil "*default password for mud-default-user.")

;;; Mud feature switches - these are global defaults; local versions
;;; are set using properties.
(defvar mud-default-logging nil "*Should we log mud output in a file?")
(defvar mud-default-page-beep t "*Should the mud make noise about pages?")
(defvar mud-default-max-buffer 4096 "*Maximum size of a mud buffer.")
(defvar mud-default-pong nil "*Should we react to pings from other users?")
(defvar mud-default-keep-visible t
  "*Should the output buffer pop up on receiving mud text?")
(defvar mud-default-quote ": quotes: " "*String to precede file quotes with")
(defvar mud-default-show-urls t "*Should URLs appear in the minibuffer?")

(defvar mud-default-idle-chat 0
  "*Should the client generate messages if you're idle?
Non-nil says to generate idle messages. If set to 't', actual text
will be sent (see mud-idle-messages); if set to '0',
mud-default-idle-noop will be sent.")
(defvar mud-default-idle-time "10 min" "*Default idle timeout")
(defvar mud-idle-messages
  '( "rolls over and snorts quietly."
     "scritches."
     "beables softly."
     "snores gently."
     "coughs."
     "sneezes."
     "downs a beer.";; About the only sensible one!
     "idles." )
  "*List of messages used by idle timer.")
(defvar mud-default-idle-noop "\n" "*Do-nothing string for idle timer.")

(defvar mud-default-input-mud nil
  "*default mud to send data to in single input mode.")
(defvar mud-single-input-mode nil "*Single or Multiple input windows?")

;;; Mud hook stuff

;;; Earlier versions of emacs don't seem to have add-hook or
;;; add-to-list, so we'll fake them if necessary.
;;; This is a little pointless since I don't /have/ an earlier version
;;; to play with.
(or (fboundp 'add-hook)
    (defun add-hook (hook function &optional append)
      "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
      (or (boundp hook) (set hook nil))
      ;; If the hook value is a single function, turn it into a list.
      (let ((old (symbol-value hook)))
        (if (or (not (listp old)) (eq (car old) 'lambda))
            (set hook (list old))))
      (or (if (consp function)
              ;; Clever way to tell whether a given lambda-expression
              ;; is equal to anything in the hook.
              (let ((tail (assoc (cdr function) (symbol-value hook))))
                (equal function tail))
            (memq function (symbol-value hook)))
          (set hook
               (if append
                   (nconc (symbol-value hook) (list function))
                 (cons function (symbol-value hook)))))))

;;; Lawks-a-mussy! 18.59.1 doesn't even have *member*
(or (fboundp 'member)
    (defun member (element list-var)
      (if (listp list-var)
            (let ((list-tmp list-var))
              (catch 'success
                (while (car list-tmp)
                  (if (equal (car list-tmp) element)
                      (throw 'success t)
                    (setq list-tmp (cdr list-tmp))))
                nil))
        (error "arg 2 of member should be a list"))))

(or (fboundp 'add-to-list)
    (defun add-to-list (list-var element)
      "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
      (or (member element (symbol-value list-var))
          (set list-var (cons element (symbol-value list-var))))))

;;; Fake this
(or (fboundp 'facep)
    (defun facep (face)
      nil))

(defun mud-completing-read( prompt table predicate require initial hist)
  (condition-case error
      (completing-read prompt table predicate require initial hist)
    (error
     (completing-read prompt table predicate require initial))))

(defvar mud-placeholders-list nil
  "*DEPRECATED but required temporarily for some internal stuff.")

;;;
;;; All new singing and dancing hooks.
;;;
(defvar mud-default-processing-hooks nil
  "*List of functions to run when the mud generates output.

This replaces 'mud-default-output-hooks'; you can use your old hooks by
adding a second parameter to their parameter lists. The parameters passed to
each hook are the current mud and the output that caused the hooks to be
run.

Most of the fun things you can do should probably be done with the
regexp matcher - see mud-default-regexps-list.")

;;; Add the new hooks in.
(add-hook 'mud-default-processing-hooks 'mud-check-for-regexps)

(defvar mud-default-regexps-list nil
  "*List of vectors, [regexp func last-matched], to run on the mud buffer.

This replaces 'mud-regexps' to perform useful arbitrary regexp
hacking. Each regexp keeps its own placeholder, so you no longer need
to maintain placeholder variables. The function parameters should be
MUD-CURRENT and &optional MATCH-STUFF - the code now passes the entire
match-data structure in as a list, so you can find out what matched
what. The regexp matching will back out from any matches that run up
against an incomplete line, so it should be possible for matches to
work properly even on a laggy connection.

This is sufficiently useful that almost all the hooks in mud.el 2.4
have been replaced with arbitrary regexp stuff.

Known bug: The placeholder code isn't 100% accurate, but should be
sufficiently accurate to not bother you.")

;;; Next two are for special builtin hooks. DON'T remove them, and if
;;; you do, DON'T complain that the mud is broken. I wonder if emacs
;;; allows me to have immutable hook entries?
(add-to-list 'mud-placeholders-list 'mud-last-log)
(add-to-list 'mud-placeholders-list 'mud-last-fill)

;;; Mud hilighting faces
;;; Convention of sorts:
;;; mud-default-X-face, mud-X-url and mud-highlight-X all relate to
;;; highlighting X's. As per guck above, mud-default-X-face gets
;;; mapped to a mud-local mud-X-face. [NEW!]
;;;
;;; FEATURE: some sort of convenience font thing would be nice.
;;; FEATURE: Menu to tweak faces?
;;;
(if (fboundp 'copy-face) ;; 18.59.1 doesn't know from faces.
    (progn
      (defvar mud-default-face nil "*default text face for mud.")
      (copy-face 'default 'mud-default-face)

      (defvar mud-default-page-face nil "*Face to highlight pages with.")
      (copy-face 'default 'mud-default-page-face)
      (set-face-foreground 'mud-default-page-face "blue")

      (defvar mud-default-url-face nil "*Face to highlight urls with.")
      (copy-face 'default 'mud-default-url-face)
      (set-face-foreground 'mud-default-url-face "red")

      (defvar mud-default-whisper-face nil "*Face to highlight whispers with.")
      (copy-face 'default 'mud-default-whisper-face)
      (set-face-foreground 'mud-default-whisper-face "brown")
      ))


(defvar mud-default-topic-color-array
  [
   "medium slate blue"
   "dark green"
   "purple"
   "DarkGoldenRod4"
   "IndianRed4"
   "OliveDrab4"
   "medium blue"
   "yellow4"
   ]
  "An array of colors to be used for topics.")

;;; Mud highlighting regexps
(defvar mud-whisper-regexp
  "^\\(You whisper\\|\\S-+ whispers,\\) .*\\([\r\n]    .*\\)*"
  "Regular expression that matches 'whisper traffic'.")
(defvar mud-page-regexp
  "^\\(You paged\\|\\(\\S-+\\) pages:\\|\\(\\S-+\\) is looking for you in\\) \\([^\r\n]*\\).*$"
  "Regular expression that matches 'page traffic'.")
(defvar mud-url-regexp
  "<URL:\\([^>\n]+\\)>\\|\\(\\(file\\|ftp\\|gopher\\|http\\|https\\|s?news\\|wais\\|www\\)://[^ \t\n\f\r\"<>|()]*[^ \t\n\f\r\"<>|.!?(){}]\\)\\|\\(mailto:[^ \t\n\f\r\"<>|()]*[^ \t\n\f\r\"<>|.!?(){}]\\)"
  "Regular expression that matches an absolute URL.");; found in vm :)
(defvar mud-ping-regexp
  "^\\(\\S-+\\) \\(say\\|whisper\\|page\\)s\\( to \\S-+\\)?[,:] \"?[Pp][Ii][Nn][Gg].*$"
  "Regular expression that matches a ping request.")
(defvar mud-topic-regexp
  "^\\S-+ .*\\([\r\n]    .*\\)* <\\(\\S-+\\)>$")
(defvar mud-version-regexp
  "^\\(\\S-+\\) whispers, \"version\""
  "Regular expression that matches a version request.")

;;; Here are all the default internal hooks.
(add-to-list 'mud-default-regexps-list
             (vector mud-whisper-regexp 'mud-highlight-whisper))
(add-to-list 'mud-default-regexps-list
             (vector mud-page-regexp 'mud-highlight-page))
(add-to-list 'mud-default-regexps-list
             (vector mud-url-regexp 'mud-highlight-url))
(add-to-list 'mud-default-regexps-list
             (vector mud-ping-regexp 'mud-acknowledge-ping))
(add-to-list 'mud-default-regexps-list
             (vector mud-version-regexp 'mud-tell-version))
(add-to-list 'mud-default-regexps-list
             (vector mud-topic-regexp 'mud-highlight-topic))

;;; Internal use only and all that sort of stuff.
(defvar mud-output-map nil "Keymap for mud output.")
(defvar mud-input-map nil "Keymap for mud input.")
(defvar mud-shared-map nil "Keymap shared between input and output.")
(defvar mud-current-list nil "List of muds we're currently connected to.")
(defvar mud-history-list nil "History list for minibuffer.")

;;;
;;; Start here!
;;;
(defun mud(&optional mud-name)
  "Connect to a mud"
  (interactive)

  (let (mud-bin mud-bout mud-details mud-host mud-port mud-process
                mud-new mud-user mud-pass)

    ;; Find out what mud to log into, if it's not specified. FEATURE:
    ;; allow user to connect to an arbitrary mud by prompting for
    ;; details.
    (or mud-name
        (setq mud-name
              (car (mud-pick-from-list mud-list mud-default))))

    (and (equal mud-name "")
         (setq mud-name (prin1-to-string mud-default)))

    ;; Retrieve the info for this mud.
    (setq mud-details (assoc (car (read-from-string mud-name)) mud-list))

    (setq mud-new (car mud-details))

    ;; happily, nth returns nil for out-of-bounds reads. Unlike aref,
    ;; whine whine.
    (setq mud-host (nth 0 (car (cdr mud-details))))
    (setq mud-port (nth 1 (car (cdr mud-details))))
    (setq mud-user (nth 2 (car (cdr mud-details))))
    (setq mud-pass (nth 3 (car (cdr mud-details))))

    ;; Set up the buffers
    (if mud-single-input-mode
        (setq mud-bin (get-buffer-create "mud.el - ECHO IN"))
      (setq mud-bin (get-buffer-create (concat mud-name " - ECHO IN"))))
    (setq mud-bout (get-buffer-create (concat mud-name " - ECHO OUT")))

    ;; Now set up the windows for the buffers (leaves us in the input buffer)
    (put mud-new 'mud-bin mud-bin)
    (put mud-new 'mud-bout mud-bout)
    (mud-windows-setup mud-new)
    (setq mud-process (mud-stream-setup mud-name mud-host mud-port))

    (put mud-new 'mud-command-history nil) ; list of previous commands, most recent first
    (put mud-new 'mud-command-to-yank -1)   ; which command we're on in that list

    ;; Save some mud details
    (put mud-new 'mud-process mud-process)
    (put mud-new 'mud-name mud-name)

    ;; Define keys for the output buffer
    ;; keymap to bounce to the input buffer
    (if mud-output-map
        ()
      (setq mud-output-map (make-keymap))
      (suppress-keymap mud-output-map t)
      (let ((i 32))
        (while (<= i 127)
          (define-key mud-output-map (char-to-string i) 'mud-bounce-input)
          (setq i (1+ i)))
        ;; This chokes in TTY mode? FIXME
        (define-key mud-output-map [return] 'mud-bounce-input)))

    (save-excursion (set-buffer mud-bout)
                    (use-local-map mud-output-map)
                    ;; Add a hook for browse-url if it's loaded.
                    (if (fboundp 'browse-url-at-mouse)
                        (local-set-key [mouse-2] 'mud-browse-url-at-mouse)))

    ;; Define keys for the input buffer
    (if mud-input-map
        ()
      (setq mud-input-map (make-keymap))
      (define-key mud-input-map "\r" 'mud-send-input)
      (define-key mud-input-map "\C-cp" 'mud-toggle-page)
      (define-key mud-input-map "\C-c\C-p" 'mud-send-ping)
      (define-key mud-input-map "\C-cl" 'mud-toggle-log)
      (define-key mud-input-map "\M-n" 'mud-grab-next-line)
      (define-key mud-input-map "\M-p" 'mud-grab-prev-line)
      (define-key mud-input-map "\C-?" 'delete-backward-char-maybe)
      (define-key mud-input-map "\C-ci" 'mud-insert-file))

    ;; Initialise properties

    ;; COMPATIBILTY CHECK
    ;; mud-default-output-hooks deprecated in 2.5
    ;; mud-regexps deprecated in 2.5
    ;; current version is $Revision: 1.1 $
    (mud-deprecated mud-new 'mud-default-output-hooks nil)
    (mud-deprecated mud-new 'mud-regexps nil)

    ;; Bits for hooks
    (mud-init-feature mud-new 'mud-keep-visible
                      mud-default-keep-visible)
    (mud-init-feature mud-new 'mud-page-beep mud-default-page-beep)
    (mud-init-feature mud-new 'mud-pong mud-default-pong)
    (mud-init-feature mud-new 'mud-quote-string mud-default-quote)
    (mud-init-feature mud-new 'mud-processing-hooks
                      mud-default-processing-hooks)
    (mud-init-feature mud-new 'mud-regexps-list
                      mud-default-regexps-list)

    ;; Topic coloring. What do you mean, a 'u' in color?
    (mud-init-feature mud-new 'mud-topic-color-array
                      mud-default-topic-color-array)

    ;; Using mud-init-feature on these broke the props-list stuff
    ;; later. I haven't investigated. FIXME.
    (put mud-new 'mud-num-colored-topics 0)
    (put mud-new 'mud-topic-color-alist nil)

    (mud-init-feature mud-new 'mud-show-urls mud-default-show-urls)

    ;; Set up faces
    (if (fboundp 'copy-face)
        (progn
          (mud-init-feature mud-new 'mud-face 'mud-default-face)
          (mud-init-feature mud-new 'mud-page-face 'mud-default-page-face)
          (mud-init-feature mud-new 'mud-url-face 'mud-default-url-face)
          (mud-init-feature mud-new 'mud-whisper-face
                            'mud-default-whisper-face)
          ))

    ;; Idle timer
    (mud-init-feature mud-new 'mud-idle-chat mud-default-idle-chat)
    (mud-init-feature mud-new 'mud-idle-noop mud-default-idle-noop)
    (mud-init-feature mud-new 'mud-idle-time mud-default-idle-time)

    ;; Autologon stuff
    (mud-init-feature mud-new 'mud-user mud-user)
    (mud-init-feature mud-new 'mud-pass mud-pass)
    (put mud-new 'mud-conn nil)

    ;; Log to file
    (mud-init-feature mud-new 'mud-logging mud-default-logging)
    (if (get mud-new 'mud-logging)
        (progn
          (mud-init-feature mud-new 'mud-log-file
                            (mud-make-log-filename mud-name))))

    ;; Set up the idle-timer
    (if (get mud-new 'mud-idle-chat)
        (mud-idle-timer-reset mud-new))

    ;; Menu-bar setup. Kinda hacky, but will do.
    (let ((map (make-sparse-keymap "Mud")))
      (define-key mud-input-map [menu-bar] (make-sparse-keymap))
      (define-key mud-input-map [menu-bar mud] (cons "Mud" map))
      (define-key map [send-ping]
        '("Send a ping" . mud-send-ping))
      (define-key map [toggle-page]
        (cons (concat (if (get mud-new 'mud-page-beep)
                          "Disable"
                        "Enable") " beep-on-page" )
              'mud-toggle-page))
      (define-key map [toggle-log]
        (cons (concat (if (get mud-new 'mud-logging)
                          "Disable"
                        "Enable") " log-to-file" )
              'mud-toggle-log))
      (define-key map [toggle-idle]
        (cons (concat (if (get mud-new 'mud-idle-timer)
                          "Disable"
                        "Enable") " idle messages" )
              'mud-toggle-idle))
      (define-key map [toggle-pong]
        (cons (concat (if (get mud-new 'mud-pong)
                          "Disable"
                        "Enable") " PONG messages" )
              'mud-toggle-pong))
      (define-key map [insert-file]
        '("Quote a file" . mud-insert-file))
      )

    (use-local-map (copy-keymap mud-input-map))

    (message "Building props list...")
    (mud-list-props mud-new)
    (message "Building props list...done")

    ;; Add to 'live' list
    (setq mud-current-list (append (list mud-new) mud-current-list))
    (if mud-single-input-mode
        (setq mud-default-input-mud mud-new))
    ))

;;;
;;; Notify user that the URL has been sent
;;; This is mainly to reassure me that I clicked on the url, since I
;;; have a slow machine.
;;;
(defun mud-browse-url-at-mouse()
  (interactive)
  (if (fboundp 'browse-url-at-mouse)
      (progn
        (message "Sending URL to browse-url...")
        (call-interactively 'browse-url-at-mouse)
        (message "Sending URL to browse-url...done."))))

;;;
;;; Choose a mud from a list, with optional default.
;;;
(defun mud-pick-from-list( mud-list &optional mud-default )
  "Select a mud from LIST and return the string & symbol."
  ;;; FIXME check that default is on list!
  (let ((mud-completion-list (mapcar '(lambda(x) (cons
                                                  (prin1-to-string
                                                   (car x)) (car x)))
                                     mud-list)))
    (assoc (mud-completing-read "Mud: " mud-completion-list nil 0
                            (prin1-to-string mud-default) mud-history-list)
           mud-completion-list)))

;;;
;;; Bounce input to the correct buffer.
;;;
;;; FIXME still has some difficulty with frames in combination with
;;; focus-follows-mouse.
;;;
(defun mud-bounce-input()
  (interactive);; Keymap functions need this, apparently
  (let ((mud-current (mud-get-from-bout mud-current-list (current-buffer)))
        (input (aref (recent-keys) (1- (length (recent-keys)))))
        mud-bin mud-win)

    (if mud-current
        (progn
          (setq mud-bin (get mud-current 'mud-bin))
          (setq mud-win (get-buffer-window mud-bin))

          ;; switch to the right window and stuff what we've got.
          (if (and (windowp mud-win)
                   (window-live-p mud-win)
                   (bufferp mud-bin)
                   (buffer-live-p mud-bin))
              ;; Buffer is on-screen, don't need to get it.
            ;; Else go fishing for window
            (put mud-current 'mud-win nil)
            (mud-windows-setup mud-current))
          (setq mud-bin (get mud-current 'mud-bin))
          (setq mud-win (get-buffer-window mud-bin))
          (select-window mud-win)
          (if (char-or-string-p input)
              (insert input))))))

;;;
;;; Gather any output the mud has to offer
;;;
(defun mud-stream-output(process output)
  "Get anything the mud said since we last looked"
  (let* ((cur (selected-window))
         (mud-current (mud-get-from-process mud-current-list process))
         (mud-logging (get mud-current 'mud-logging))
         (keep-visible (get mud-current 'mud-keep-visible))
         (bout (get mud-current 'mud-bout))
         (win (get-buffer-window bout t))
         (mud-processing-hooks (get mud-current
                                    'mud-processing-hooks))
         (mud-face (get mud-current 'mud-face))
         (at-end-of-buf nil))           ; point at end of bout?

    (save-excursion
      (set-buffer bout)
      (save-excursion                   ; to save current point within
                                        ; bout as well
        (setq at-end-of-buf (= (point) (point-max)))
        (goto-char (point-max))
        (let ((start (point)))
          (and buffer-read-only
               (toggle-read-only nil))
          (insert output)
          (and (facep 'mud-face)
               (put-text-property start (point-max) 'face 'mud-face)))

        ;; PROCESS OUTPUT FROM MUD
        ;; Run hooks for output. This is covered with an error handler
        ;; and a save-excursion so you can't screw up too badly.
        ;;
        ;; However, I've discovered that /I/ can screw up badly...
        (if (condition-case error
                (save-excursion
                  (and mud-processing-hooks
                       (run-hook-with-args 'mud-processing-hooks
                                           mud-current output))
                  t)
              ;;(error (message (prin1-to-string error))))
              (error nil))
            ;; hooks ran successfully
            ()

          ;; whoops!
          ;; FEATURE should be a bit more specific here, if
          ;; possible. What broke and how did it break.
          (mud-whine mud-current "One of your hook functions is broken."))

        ;; Special hooks. These are always called, where as stuff on the
        ;; hooks list is optional and can be removed if desired.

        ;; Auto Login
        (if (and (get mud-current 'mud-user)
                 (not (get mud-current 'mud-conn)))
            (mud-check-logon mud-current))

        ;; NB This should be the last thing called as it destroys
        ;; essential information about the way the line arrived.
        (mud-fill-lines mud-current)

        ;; However, we do like to log /after/ the buffer has been pretty-printed
        (if mud-logging
            (mud-log-to-file mud-current))

        ;; And then resize the thing. This is REALLY the last thing to
        ;; be called.
        (mud-check-buffer-size mud-current)

        ;; END OF PROCESSING

        ;; Pretend we didn't modify the buffer and make damn sure YOU can't.
        (toggle-read-only t)
        (set-buffer-modified-p nil)))

    ;; display the mud output buffer if required
    (if keep-visible
        (or win
            (setq win (display-buffer bout nil))))

    ;; scroll window to bottom if
    ;;   (a) it's visible
    ;;   (b) we're not in it right now
    ;;  (otherwise scrollback is awkward and ugly)
    (if (and win (not (equal cur win)))
        (progn
          (save-excursion
            (select-window win)
            (goto-char (point-max))
            (recenter -1)
            (select-window cur)))
      ;; Also, if point was already at the end of the window, we update
      ;; it to still be so.  If I (dfan) had my way, this would
      ;; actually be only thing causing the window to scroll.
      (if at-end-of-buf
          (let ((cur-buf (current-buffer)))
            ;; Don't use save-excursion because that will undo the
            ;; very action we're trying to perform if we're already in
            ;; bout!
            (set-buffer bout)
            (goto-char (point-max))
            (set-buffer cur-buf))))
    ))


;;;
;;; Log the mud output to a file
;;;
(defun mud-log-to-file( mud-current )
  "Log mud output to the mud's logfile."
  (let ((mud-log-file (get mud-current 'mud-log-file))
        (mud-last-log (get mud-current 'mud-last-log))
        (mud-logging  (get mud-current 'mud-logging)))
    (if mud-logging
        (progn
          (write-region (or mud-last-log 1) (point-max) mud-log-file t 'silent)
          (put mud-current 'mud-last-log (point-max))))))

;;;
;;; Try logging onto the mud once we get a prompt
;;;
;;; FEATURE: Make this work for more than PerlMUD.
(defun mud-check-logon( mud-current )
  "Try to log onto the mud automatically."
  (let ((mud-user (get mud-current 'mud-user))
        (mud-pass (get mud-current 'mud-pass)))
    (if (eq mud-user t)
        (progn
          (setq mud-user mud-default-user)
          (setq mud-pass mud-default-pass)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "connect " nil t)
          (progn
            (mud-send-string mud-current
                             (concat "connect " mud-user " " mud-pass "\n"))
            ;; FIXME should check if we've actually logged in!
            (put mud-current 'mud-conn t))))))

;;;
;;; Keep the windows to a reasonable size, since otherwise emacs seems
;;; to get upset. Dunno why, I don't remember the original mud.el
;;; having problems with that.
;;; It may have something to do with the fact that the wrap-code was
;;; processing an entire buffer, mind you... fixed that.
(defun mud-check-buffer-size( mud-current )
  "Check buffer size and trim if necessary"
  (let ((mud-bin (get mud-current 'mud-bin))
        (mud-bout (get mud-current 'mud-bout))
        (mud-max-buffer (or (get mud-current 'mud-max-buffer) 0)))

    (save-excursion
      (set-buffer mud-bin)
      (if (eq mud-max-buffer 0)
          () ;; do nothing
        (let ((size (point-max)))
          (and (> size mud-max-buffer)
               (goto-char (+ (point-min) (- size mud-max-buffer)))
               (delete-region (point-min) (point)))))

      (set-buffer mud-bout)
      (if (eq mud-max-buffer 0)
          () ;; do nothing
        (let ((delta (- (point-max) mud-max-buffer)))
          (if (> delta 0)
              (progn
                (delete-region (point-min) (+ (point-min) delta))

                ;; Reset the variables
                (if (listp mud-placeholders-list);; should be
                    ;; process it.
                    (let ((m-ph-l mud-placeholders-list))
                      (while (car m-ph-l)
                        (put mud-current (car m-ph-l)
                             (- (or (get mud-current (car m-ph-l)) 0)
                                delta))
                        (setq m-ph-l (cdr m-ph-l))))
                  ;; If it's set and not a list, complain.
                  (if mud-placeholders-list
                      (mud-whine mud-current
                                 "mud-placeholders-list isn't a list."))))))))))

;;;
;;; Send our $0.02 to the mud
;;;
(defun mud-send-input()
  "Send the current line of input to the mud"
  (interactive)
  (beginning-of-line)
  (let ((beg (point))
        (mud-current (mud-get-from-bin mud-current-list (current-buffer)))
        new-string mud-receiver mud-receiver-name slash-command)
    (end-of-line)
    (insert "\n")
    (setq new-string (buffer-substring beg (point)))
    (mud-save-string-to-command-history new-string)
    (put mud-current 'mud-command-to-yank -1)

    ;; handle / commands
    (if (string-match "^/\\(\\S-+\\)\\s-*" new-string)
        (setq mud-receiver-name (substring new-string
                                           (match-beginning 1)
                                           (match-end 1))
              new-string (substring new-string (match-end 1))
              mud-receiver (read-from-string mud-receiver-name)))

    ;; If mud-receiver is unset, try doing something smart with it.
    (if mud-receiver
        ()

      ;; Check for commands
      (if mud-receiver-name
          (progn
            ;; Trim the leading " " and trailing ^j if there is one.
            (if (string-match "^\\s-*\\(.*\\)
" new-string)
                (setq new-string (substring new-string (match-beginning 1)
                                            (match-end 1))))
            (cond

             ;; List muds
             ((string= mud-receiver-name "list")
              (message "Muds: %s" (prin1-to-string mud-current-list)))

             ;; Connect to a new mud
             ((string= mud-receiver-name "join")
              (if (string-match "\\S-+" new-string)
                  (mud new-string)
                (error "Syntax: /join MUDNAME")))

             ;; Change the default input mud
             ((string= mud-receiver-name "input")
              (if (string-match "\\S-+" new-string)
                  (if (read-from-string new-string)
                      (progn
                        (setq mud-default-input-mud (read-from-string
                                                     new-string))
                        (message "Default input now goes to %s." new-string))
                    (if (string-match "?" new-string)
                        (message "Default input goes to %s."
                                 (prin1-to-string mud-default-input-mud))
                      (error "You are not connected to %s."
                             mud-receiver-name)))
                (error "Syntax: /input MUDNAME")))


             ;; If all else fails, tell the user they goofed.
             (t
              (error "/%s is not a valid command, and you are not connected to a mud called \"%s\"." mud-receiver-name (or (not (string= new-string "")) mud-receiver-name)))))

        (if mud-single-input-mode
            (setq mud-receiver mud-default-input-mud)
          (setq mud-receiver (mud-get-from-bin mud-current-list
                                               (current-buffer))))))

    ;; send the string
    (if mud-receiver
        (mud-send-string mud-receiver new-string))))

(defun mud-send-string( mud-current string )
  "Send a string to the specified mud process.
As a side effect, kicks the mud's idle timer."
  (let ((mud-process (get mud-current 'mud-process)))
    (and mud-process
         (process-send-string (get mud-current 'mud-process) string))
    (mud-idle-timer-reset mud-current)))

;;;
;;; Open up a tcp stream to the mud
;;;
(defun mud-stream-setup(mud host port)
  "Set up a tcp stream to the mud"

  ;; See if we have the process already
  ;; This should really check to see if the process has the same parameters
  ;; (host & port at least) as are being passed in
  (if (setq proc (get-process mud))
      (progn
        (or (equal (process-status proc) 'run)
            (progn
              (delete-process proc)
              (setq proc nil)))))

  ;; Set the proc variable to a new stream if necessary
  (setq proc (or proc
                 (mud-open-network-stream mud nil host port)))

  ;; bolt on the output filter
  (set-process-filter proc 'mud-stream-output)
  (set-process-sentinel proc 'mud-sentinel)

  ;; Return the process
  proc)

;;;
;;; Set up the input & output windows for the mud client
;;; Tries to be intelligent about keeping existing windows where they are
;;; and such like. Could do with more such intelligence.
;;;
(defun mud-windows-setup( mud-current )
  "Set up the mud input/output windows from the currently selected window."
  (interactive)
  (let ((wout (get mud-current 'mud-wout))
        (win (get mud-current 'mud-win))
        (bout (get mud-current 'mud-bout))
        (bin (get mud-current 'mud-bin))
        (mud-name (get mud-current 'mud-name))
        mud-wheight
        mud-wheight-diff)

    ;; Check for buffers already visible
    (if (get-buffer-window bout)
        (setq wout (get-buffer-window bout)))

    ;; First, get the mud output window
    (if (and (windowp wout)
             (window-live-p wout))
        (select-window wout)
      (setq wout (selected-window)))

    ;; Add the buffer
    (put mud-current 'mud-wout wout)
    (set-window-buffer wout bout)

    ;; Special case. If the input window is visible, we'll hide it,
    ;; since you tend to get weird window configurations otherwise.
    (if (get-buffer-window bin)
        (setq win (get-buffer-window bin)))

    (if (and (windowp win)
             (window-live-p win))
        ;; Stop wout from being nuked...
        (if (eq win wout)
            ;; In theory, here, we could look for another visible
            ;; mud-output-window, split it in two, and set wout to one
            ;; of the halves, THEN delete win.
            ;; FITNR, maybe :)
            ()
          (delete-window win)))

    ;; Now do the input window
    ;; The input window height = minimum height
    ;; The output window height = current height - minimum height
    (setq mud-wheight (- (window-height wout) window-min-height))
    (if (< mud-wheight window-min-height)
        (progn
          (setq mud-wheight-diff (- window-min-height mud-wheight))
          (shrink-window (- mud-wheight-diff))
          (setq mud-wheight window-min-height)))
    (setq win (split-window wout mud-wheight))
    (select-window win)
    (if (buffer-live-p bin)
        ()
      ;; Oh you CABBAGE. You deleted the input buffer.
      (if mud-single-input-mode
          (setq bin (get-buffer-create "mud.el - ECHO IN"))
        (setq bin (get-buffer-create (concat mud-name " - ECHO IN"))))
      (put mud-current 'mud-bin bin)
      (set-buffer bin)
      (use-local-map (copy-keymap mud-input-map)))
    (set-window-buffer win bin)

    (put mud-current 'mud-win win)))

;;;
;;; Figure out the mud name from the process
;;; Could probably roll these into one function, actually.
;;;
(defun mud-get-from-process( list process )
  "Retrieve the mud symbol name from the process attached to it"
  ;; A little BFI :)
  (if (car list)
      (if (equal process (get (car list) 'mud-process))
          (car list)
        ;; ELSE
        (mud-get-from-process (cdr list) process))
    ;; ELSE doom! doom!
    nil))

;;;
;;; Figure out the mud name from the input-buffer
;;;
(defun mud-get-from-bin( list bin )
  "Retrieve the mud symbol name from the input buffer attached to it"
  (if (car list)
      (if (equal bin (get (car list) 'mud-bin))
          (car list)
        ;; ELSE
        (mud-get-from-bin (cdr list) bin))
    ;; ELSE doom! doom!
    nil))

;;;
;;; Figure out the mud name from the input-buffer
;;;
(defun mud-get-from-bout( list bout )
  "Retrieve the mud symbol name from the input buffer attached to it"
  (if (car list)
      (if (equal bout (get (car list) 'mud-bout))
          (car list)
        ;; ELSE
        (mud-get-from-bout (cdr list) bout))
    ;; ELSE doom! doom!
    nil))

;;;
;;; Process Sentinel to catch closing connection c c c c c!
;;;
(defun mud-sentinel( process msg )
  "Sentinel for mud process."
  (let ((mud-current (mud-get-from-process mud-current-list process))
        mud-win)
    (message (prin1-to-string msg))
    (message
     (concat "Connection to " (prin1-to-string mud-current)
             " closed."))
    (mud-whine mud-current "The mud connection is closed.")

    ;; FEATURE these should be optional
    ;; FEATURE auto-reconnect
    (setq mud-current-list (delete mud-current mud-current-list))

    ;; Are we in single-input-mode?
    (if mud-single-input-mode
        (progn
          ;; This tries to delete the window. If it's the sole window,
          ;; because you've been fiddling with the windows, then it'll
          ;; quietly fail.
          (condition-case error
              (delete-window (get-buffer-window (get mud-current 'mud-bout)))
            (error nil))

          ;; If there's anything left on the list, update the default
          ;; mud and display its window
          (if mud-current-list
              (progn
                (setq mud-default-input-mud (car mud-current-list))
                (setq mud-win
                      (get-buffer-window
                       (get mud-default-input-mud 'mud-bout)))
                (if (and (windowp mud-win)
                         (window-live-p mud-win))
                    ;; Buffer is on-screen, don't need to get it.
                    ()
                  ;; Else go fishing for window
                  (put mud-default-input-mud 'mud-wout nil)
                  (mud-windows-setup mud-default-input-mud))
                (select-window (get-buffer-window
                                (get mud-default-input-mud 'mud-bin)))
                ))

          ;; If that was the last mud, delete the input buffer and its window
          (if (not mud-current-list)
              (progn
                (condition-case error
                    (delete-window
                     (get-buffer-window (get mud-current 'mud-bin)))
                  (error nil))
                (kill-buffer (get mud-current 'mud-bin)))))

      ;; Not in single-input mode: delete the input buffer and its
      ;; window.
      (condition-case error
          (delete-window (get-buffer-window (get mud-current 'mud-bin)))
        (error nil))
      (kill-buffer (get mud-current 'mud-bin)))
    (beep)))

;;;
;;; Stick an error message into the mud output-window.
;;;
(defun mud-whine( mud-current string )
  (let ((mud-bout (get mud-current 'mud-bout)))
    (if mud-bout
        (save-excursion
          (set-buffer mud-bout)
          (goto-char (point-max))
          (let ((readonly buffer-read-only))
            (if readonly
                (toggle-read-only nil))
            (insert (concat "mud.el says, \"" string "\"\n"))
            (if readonly
                (toggle-read-only t)))))))

;;;
;;; And THIS is to fill out the displayed lines, and remove those pesky ^M's.
;;;
;;; FIXME this won't cope properly with a big word that won't wrap in
;;; the middle of a block - it stops wrapping at the big word, instead
;;; of attempting to wrap the rest of the block.
;;;
;;; In typical unix fashion, this bug description is probably longer
;;; than the fix for the bug.
(defun mud-fill-lines( mud-current )
  "Fill buffer line by line."
  (let ((last-fill (get mud-current 'mud-last-fill)))
    (save-excursion
      (or last-fill
          (setq last-fill (point-min)))
      (if (> last-fill (point-max))
          (setq last-fill (point-min)))
      (goto-char last-fill)
      (beginning-of-line)
      (save-excursion
        (while (search-forward "\r" nil t)
          (replace-match "")))
      (while (not (eobp))
        (if (<= (move-to-column (+ 1 fill-column)) fill-column)
            ;; Line is too short to fill, goto next line.
            (forward-line 1)
          ;; Look for a break point in the current line
          (skip-chars-backward "^   \n")
          ;; Are we (a) at the start of the line or (b) at the leading
          ;; whitespace that we inserted last time?
          (if (or (bolp)
                  (save-excursion
                    (let ((here (point)))
                      (beginning-of-line)
                      (skip-chars-forward " ")
                      (if (eq (point) here)
                          t
                        nil))))
              ;; Can't wrap this one
              ()
            ;; Else wrap the line
            (save-excursion (insert "\n    ")))
          (forward-line 1))))
    ;; Save the fill-point.
    (put mud-current 'mud-last-fill (point-max))))


;;; ---------------------------------------------------------------
;;; KEYMAP FUNCTIONS
;;; ---------------------------------------------------------------

;;;
;;; Toggle page-me beeping on and off
;;;
(defun mud-toggle-page()
  "Toggle page-me beeping on and off"
  (interactive)
  (let* ((mud-current (mud-get-from-bin mud-current-list (current-buffer)))
         (paging (get mud-current 'mud-page-beep)))
    (if paging
        (progn
          (put mud-current 'mud-page-beep nil)
          (message "Beeping on page disabled."))
      (put mud-current 'mud-page-beep t)
      (message "Beeping on page enabled."))

    ;; Update the menus
    (mud-update-menu-toggle mud-current
                            [menu-bar mud toggle-page]
                            paging
                            "beep-on-page")))

;;;
;;; Enable/Disable log to file. Uses convert-standard-file to make
;;; sure the filename is valid, since this function has now been
;;; usefully implemented in NTemacs at least.
;;;
(defun mud-toggle-log()
  "Toggle logging on and off."
  (interactive)
  (let* ((mud-current (mud-get-from-bin mud-current-list (current-buffer)))
         (mud-logging (get mud-current 'mud-logging))
         (mud-name (get mud-current 'mud-name))
         mud-log-file)
    (if mud-logging
        (progn
          (put mud-current 'mud-logging nil)
          (message "Log-to-file disabled."))
      (put mud-current 'mud-logging t)
      (mud-init-feature mud-current 'mud-log-file
                        (mud-make-log-filename mud-name))
      (setq mud-log-file
            (convert-standard-filename (or (get mud-current 'mud-log-file)
                                           "gen-mud.log")))
      (save-excursion
        (set-buffer (get mud-current 'mud-bout))
        (put mud-current 'mud-last-log (point)))
      (message "Log-to-file enabled, writing to %s." mud-log-file))

    ;; Update menus
    (mud-update-menu-toggle mud-current
                            [menu-bar mud toggle-log]
                            mud-logging
                            "log-to-file")))
;;;
;;; Toggle idle messaging on and off
;;;
(defun mud-toggle-idle()
  "Toggle idle messaging on and off"
  (interactive)
  (let* ((mud-current (mud-get-from-bin mud-current-list (current-buffer)))
         (idling (get mud-current 'mud-idle-chat)))
    (if idling
        (progn
          (put mud-current 'mud-idle-chat nil)
          (message "Idle messages disabled."))
      (put mud-current 'mud-idle-chat t)
      (message "Idle messages enabled."))

    ;; Update the menus
    (mud-update-menu-toggle mud-current
                            [menu-bar mud toggle-idle]
                            idling
                            "idle messages")))

;;;
;;; Toggle PONG (ping response) on and off
;;;
(defun mud-toggle-pong()
  "Toggle pong messaging on and off"
  (interactive)
  (let* ((mud-current (mud-get-from-bin mud-current-list (current-buffer)))
         (ponging (get mud-current 'mud-pong)))
    (if ponging
        (progn
          (put mud-current 'mud-pong nil)
          (message "Pong messages disabled."))
      (put mud-current 'mud-pong t)
      (message "Pong messages enabled."))

    ;; Update the menus
    (mud-update-menu-toggle mud-current
                            [menu-bar mud toggle-pong]
                            ponging
                            "PONG messages")))

;;;
;;; Quote a file into the mud
;;;
(defun mud-insert-file() "Quote a file on the mud."
  (interactive)
  (let* ((mud-current (mud-get-from-bin mud-current-list (current-buffer)))
         (mud-quote-string (get mud-current 'mud-quote-string))
         last-line)
    (save-excursion
      (set-buffer (get-buffer-create " *mud quote buffer*"))
      (erase-buffer)
      ;; No, of course I shouldn't be doing this. And?
      (call-interactively 'insert-file)
      (goto-char (point-min))
      (insert mud-quote-string)
      (end-of-line)
      (insert "\n")
      (mud-send-string mud-current (buffer-substring (point-min)
                                                     (point)))
      (setq last-line (point))
      (while (eq 0 (forward-line 1))
        (insert mud-quote-string)
        (end-of-line)
        (insert "\n")
        (mud-send-string mud-current (buffer-substring last-line
                                                       (point)))
        (setq last-line (point))))))

;;;
;;; Say ping with the current time attached.
;;;
(defun mud-send-ping() "Send a ping to the mud!"
  (interactive)
  (let* ((mud-current (mud-get-from-bin mud-current-list (current-buffer))))
    (mud-send-string mud-current (concat "say ping "
                                         (current-time-string)
                                         "\n"))))

(defun delete-backward-char-maybe ()
  "delete-backward-char if we're not at the beginning of the line."
  (interactive)
  (if (not (= (current-column) 0))
      (delete-backward-char 1)))


;;; ----------------------------------------------------------------
;;; HOOK FUNCTIONS
;;; ----------------------------------------------------------------

;;; Yay! New hook functions, much more fun to work with.
;;;
;;; 1. Don't mess with the ^M characters. They're used by the
;;;    line-filling code.
;;;
;;; 2. You're getting raw mud output here. It's not
;;;    pretty-printed. This means you can play with it before the
;;;    line-filling code lays hands on it.
;;;
;;; 3. If you're going to move the point or switch to another buffer,
;;;    use a save-excursion to ensure that any hook functions called
;;;    after yours start at the same point as you did. There's a
;;;    save-excursion around the entire of the run-hooks call, but
;;;    that's not quite good enough.
;;;
;;; 4. If you need to maintain a 'last-place-we-did-this' variable,
;;;    you'll need to add it as a symbol to the
;;;    mud-placeholders-list. See the head of the file for how to do
;;;    this.
;;;
;;; 5. You get two parameters, mud-current and the text that caused
;;;    the hook to be called. You can get everything else from
;;;    mud-current; see below for examples.
;;;
;;; 6. You can report errors using (mud-whine mud-current "text"),
;;;    which will appear in your output buffer as
;;;    mud.el says, "text"
;;;    which I think is really cute.
;;;
;;; 7. If you screw up your code, you'll get a message like the above
;;;    when the hook throws an error, but the mud should keep going.
;;;
;;; 8. The arbitrary regexp matcher is good enough that you should be
;;;    able to use that for 99% of your hookage needs. It contains
;;;    built-in tracking of "last place we got a match", so you don't
;;;    have to worry about that. It also hands you the entire
;;;    match-data structure. See below for examples; almost all of
;;;    mud.el's original hook functionality has been folded into
;;;    regexp stuff.

;;;
;;; New regexp-matching function, designed to be smarter.
;;;
(defun mud-check-for-regexps( mud-current output )
  "Check mud-regexps-list for a match, and act appropriately."
  (let ((mud-regexps-list (get mud-current 'mud-regexps-list)))

    (if mud-regexps-list
        (progn
          (if (listp mud-regexps-list)
              (let ((mud-tmp-list mud-regexps-list)
                    mud-regexps-list)
                (while (car mud-tmp-list)
                  (setq new-vec (mud-process-regexp mud-current
                                                    (car mud-tmp-list)
                                                    output))
                  (and (vectorp new-vec)
                       (setq mud-regexps-list
                             (append mud-regexps-list (list new-vec))))
                  (setq mud-tmp-list (cdr mud-tmp-list)))
                (put mud-current 'mud-regexps-list mud-regexps-list))
            (message "mud-regexps-list is not a list!"))))))

;;;
;;; Handle a single regexp from the above function.
;;;
(defun mud-process-regexp( mud-current regexp-vector output)
  "Check mud-current for a single regexp and do the stuff."
  (let ((mud-max-buffer (or (get mud-current 'mud-max-buffer)
                            (save-excursion
                              (set-buffer (get mud-current 'mud-bout))
                              (- (point-max) (point-min)))))
        regexp function last-match start maybe-end end)
    (if (vectorp regexp-vector)
        (progn
          (setq regexp (aref regexp-vector 0)
                function (aref regexp-vector 1))

          (if (< (length regexp-vector) 3)
            (setq regexp-vector (vconcat regexp-vector
                                         (vector mud-max-buffer))))
          (setq last-match (+ (aref regexp-vector 2) (length output)))

          ;; last-match is the number of characters from the end of
          ;; the buffer that we found the previous match. We've just
          ;; added OUTPUT to the buffer, so we need to add its length
          ;; to last-match. This saves having to maintain variables in
          ;; a seperate list/function like the original hooks do.

          ;; Now, check if last-match is still within the buffer. If
          ;; not, change it.
          (setq start (- (point-max) last-match))
          (or (< (point-min) start)
              (setq start (point-min)))

          (save-excursion
            (goto-char start)
            (setq maybe-end start
                  end start)
            (while (re-search-forward regexp nil t)
              (setq maybe-end end
                    end (match-end 0))
              (funcall function mud-current (match-data)))

            ;; if the regexp ran right up to the end of the buffer,
            ;; check that the line it matched was complete. This
            ;; allows regexps to track across updates properly,
            ;; e.g. during laggy networks where half a URL appears in
            ;; one update and the rest of the URL in the next.
            (if (= (point-max) end)
                (if (string= (substring output -1) "")
                    ()
                  (setq end maybe-end)))

            ;; reset the vector pointer
            (setq last-match (- (point-max) end))
            (aset regexp-vector 2 last-match)
            regexp-vector))
      (message "regexp-vector ain't a vector!")
      nil)))

;;;
;;; Highlight a whisper
;;;
(defun mud-highlight-whisper( mud-current &optional match-stuff )
  "Highlight whispers when found."
  (let ((start (nth 0 match-stuff))
        (end (nth 1 match-stuff))
        (mud-whisper-face (get mud-current 'mud-whisper-face)))
    (if (and start
             end
             (facep mud-whisper-face))
        (put-text-property start end 'face mud-whisper-face))))

;;;
;;; Highlight something in a topic
;;;
(defun mud-highlight-topic (mud-current &optional match-stuff)
  "Highlight things in topics when found."
  (let ((start (nth 0 match-stuff))
        (end (nth 1 match-stuff))
        (topic-start (nth 4 match-stuff))
        (topic-end (nth 5 match-stuff))
        (mud-topic-face (get mud-current 'mud-topic-face)))
    (if (and start end)
        ;; Send it off
        (mud-color-topic mud-current (buffer-substring topic-start topic-end) start end))))


;;;
;;; Highlight a page, and possibly put a message in the minibuffer.
;;; Also beep if mud-page-beep is set.
;;;
(defun mud-highlight-page( mud-current &optional match-stuff )
  "Highlight pages when found."
  (let ((start (nth 0 match-stuff))
        (end (nth 1 match-stuff))
        (userpage-start (nth 4 match-stuff))
        (userpage-end (nth 5 match-stuff))
        (locpage-start (nth 6 match-stuff))
        (locpage-end (nth 7 match-stuff))
        (pagetext-start (nth 8 match-stuff))
        (pagetext-end (nth 9 match-stuff))
        (mud-page-beep (get mud-current 'mud-page-beep))
        (mud-page-face (get mud-current 'mud-page-face))
        )
    (if (and start
             end
             (facep mud-page-face))
        (put-text-property start end 'face mud-page-face))
    (if (and userpage-end userpage-start)
        (message "%s has paged you. (%s)"
                 (buffer-substring userpage-end userpage-start)
                 (prin1-to-string mud-current)))
    (if (and locpage-end locpage-start)
        (message "%s is looking for you in %s (%s)"
                 (buffer-substring locpage-end locpage-start)
                 (if (and pagetext-end pagetext-start)
                     (buffer-substring pagetext-start pagetext-end)
                   "?")
                 (prin1-to-string mud-current)))
    (if mud-page-beep
        (beep))))

;;;
;;; Highlight a URL and display it in the minibuffer.
;;;
(defun mud-highlight-url( mud-current &optional match-stuff)
  "Highlight a URL in the buffer"
  (let ((last 0)
        url n
        (mud-url-face (get mud-current 'mud-url-face)))
    (setq n 0)
    (while (null (nth n match-stuff))
      (setq n (+ 2 n)))
    (setq url (buffer-substring (nth n match-stuff)
                                (nth (+ n 1) match-stuff)))
    (if (facep mud-url-face)
         (put-text-property (nth n match-stuff) (nth (+ n 1) match-stuff)
                            'face mud-url-face))
    (while (string-match "%" url last)
      (setq last (+ 1 (match-end 0)))
      (setq url (replace-match "%%" nil t url)))
    (if (get mud-current 'mud-show-urls)
        (message (concat "URL: " url)))))

;;;
;;; Acknowledge a PING message, if we're answering them.
;;;
(defun mud-acknowledge-ping( mud-current &optional match-stuff )
  "Acknowledge a PING message."
  (let ((victim-start (nth 2 match-stuff))
        (victim-end (nth 3 match-stuff))
        (method-start (nth 4 match-stuff))
        (method-end (nth 5 match-stuff))
        method victim
        (mud-pong (get mud-current 'mud-pong)))
    (if (and mud-pong method-end method-start victim-end victim-start)
        (progn
          (setq method (buffer-substring method-start method-end)
                victim (buffer-substring victim-start victim-end))
          (if (string= method "say")
              (setq victim (concat victim ": "))
            (setq victim (concat victim "=")))
          (mud-send-string mud-current
                           (format "%s %sPONG %s\n" method victim
                                   (current-time-string)))))))

;;;
;;; Respond to a version query
;;;
(defun mud-tell-version( mud-current &optional match-stuff )
  "Report the version of the client to whoever asks."
  (let ((victim-start (nth 2 match-stuff))
        (victim-end (nth 3 match-stuff))
        victim)
    (if (and victim-start victim-end)
        (progn
          (setq victim (buffer-substring victim-start victim-end))
          (message "%s has asked for your mud.el version." victim)
          (mud-send-string mud-current
                           (format "whisper %s=mud.el $Revision: 1.1 $\n" victim))
          ))))


;;; ---------------------------------------------------------------------------
;;; Utility functions
;;; ---------------------------------------------------------------------------

;;;
;;; Make a hook. This is me being /really/ nice.
;;;
(defun mud-create-hook( &optional mud regexp function )
  "Create a new hook for the current mud."
  (interactive)
  (let (mud-current)
    (or mud
        (setq mud (mud-get-from-bout mud-current-list (current-buffer)))
        (setq mud (mud-get-from-bin mud-current-list (current-buffer)))
        (setq mud (cdr (mud-pick-from-list mud-current-list))))

    (catch 'done
      (if (stringp mud)
          (setq mud-current (read-from-string mud))
        (if (symbolp mud)
            (setq mud-current mud)))

      (or mud-current
          (progn
            (message "Sorry, I don't know anything about a mud called %s."
                     (prin1-to-string mud))
            (throw 'done nil)))

      (or regexp
          (setq regexp
                (read-from-minibuffer "Regexp: " nil nil nil regexp-history)))

      ;; Verify regexp
      (condition-case error
          (string-match regexp "TEST")
        (error (message "Your regexp is broken!")
               (setq regexp nil)
               (throw 'done nil)))

      ;; FIXME completing-read on functions.
      (or function
          (setq function (read-from-minibuffer "Function: " nil nil t nil)))

      (or (fboundp function)
          (progn
            (message "Sorry, I don't know any function called %s.\n"
                     (prin1-to-string function))
            (throw 'done nil)))

      (message "When I see \"%s\" in %s, I'll call %s." regexp
               (prin1-to-string mud)
               (prin1-to-string function))

      (let ((mud-regexps-list (get mud-current 'mud-regexps-list))
            (new-regexp (vector regexp function)))
        (add-to-list 'mud-regexps-list new-regexp)
        (put mud-current 'mud-regexps-list mud-regexps-list)))))

;;;
;;; Tweak a font interactively
;;;
(defun mud-tweak-face( &optional mud-current )
  "Tweak a face interactively."
  (interactive)
  (or mud-current
      (setq mud-current (mud-pick-from-list mud-list)))

  ;; bwahhahahah!
  (let* ((faces (mapcar '(lambda(x)
                           (cons (prin1-to-string (car x)) x))
                        (delq nil
                              (let (y)
                                (mapcar '(lambda(x)
                                           (if y
                                               (let ((z (cons y x)))
                                                 (setq y nil)
                                                 (if (facep x)
                                                     z
                                                   nil))
                                             (setq y x)
                                             nil))
                                        (symbol-plist mud-current))))))
         (selected-face-name (mud-completing-read "Face: " faces nil 0 nil))
         (selected-face
          (get mud-current (nth 1 (assoc selected-face-name faces)))))
    (message "selected face colour: %s"
             (prin1-to-string (face-foreground selected-face)))))

;;;
;;; Delete output lines from the buffer. Gagging, basically.
;;;
(defun mud-delete-lines( mud-current )
  "Delete lines output from the mud. Uses match-data to find them."
  (let ((beg (match-beginning 0))
        (end (match-end 0)))
    (delete-region beg end)
    ))

;;;
;;; Initialise a mud feature unless it's already been set.
;;;
(defun mud-init-feature( mud-current featurename value )
  "Set MUD-CURRENT's FEATURENAME property to VALUE unless it's already set."
  (or (member featurename (symbol-plist mud-current))
      (put mud-current featurename value)))

;;;
;;; Handle deprecated symbols nicely.
;;;
(defun mud-deprecated( mud-current oldvar newvar )
  "Check if a deprecated variable is in use.

If you use a deprecated variable, mud.el will tell you the new
variable to use and copy from the old variable to the new."
  (if (boundp oldvar)
      (if newvar
          (progn
            (mud-whine mud-current (concat "Use of " (prin1-to-string oldvar)
                                           " is deprecated. Please use "
                                           (prin1-to-string newvar)
                                           " instead."))
            (mud-fill-lines mud-current)

            ;; YARG! Isn't there a nicer way to do this?
            (set (make-symbol (symbol-name newvar)) (symbol-value
                                                     oldvar)))
        (mud-whine mud-current
                   (concat "Use of " (prin1-to-string oldvar)
                           " is deprecated, and there is no direct"
                           " replacement. Please check the release"
                           " notes for this version of mud.el."))
        (mud-fill-lines mud-current))))

;;;
;;; Create a name for logfiling the specified mud. You can override
;;; this if you like.
;;;
(defun mud-make-log-filename( mud-name )
  "Create a logfile name for MUD-NAME. Customise this if you like.
It uses convert-standard-filename to make sure that the file is valid
for your OS, so it's probably a good idea to do likewise if you modify
this."
  (convert-standard-filename
   (expand-file-name (concat "~/" mud-name ".log"))))

;;;
;;; Update menus when a value is toggled. Aren't I nice?
;;;
(defun mud-update-menu-toggle( mud-current key value text)
  "Update the Mud menu entry in MUD-CURRENT's keymap for KEY.
The menu entry is set to \"Enable\" or \"Disable\" (depending on VALUE)
plus the trailing TEXT."
  (let ((mud-bin (get mud-current 'mud-bin))
        (mud-bout (get mud-current 'mud-bout)))
    (save-excursion
      (set-buffer mud-bin)
      (define-key (current-local-map) key
        (cons (concat (if value
                          "Enable "
                        "Disable ") text)
              (lookup-key (current-local-map) key))))))

;;;
;;; Convenience function for hacking at keymaps, specifically menu
;;; entries.
;;;
(defun mud-define-key( mud-current key func text )
  "Convenience function to define a key for a given mud."
  (let ((mud-bin (get mud-current 'mud-bin)))
    (save-excursion
      (set-buffer mud-bin)
      (let ((map (current-local-map)))
        (define-key map key (if text (cons text func) func))))))

;;;
;;; Make a properties list on the menu. More showing off. Jeez.
;;;
(defun mud-list-props( &optional mud )
  "List properties of the specified mud."
  (interactive)
  (let (mud-current)
    (or mud
        (setq mud (mud-get-from-bout mud-current-list (current-buffer)))
        (setq mud (mud-get-from-bin mud-current-list (current-buffer)))
        (setq mud (cdr (mud-pick-from-list mud-current-list))))

    (if (stringp mud)
        (setq mud-current (read-from-string mud))
      (if (symbolp mud)
          (setq mud-current mud)))

    (let ((proplist (delq nil
                          (let (y)
                            (mapcar '(lambda(x)
                                       (if y
                                           (let ((z (cons y x)))
                                             (setq y nil)
                                             (if (and (not (processp x))
                                                      (not (bufferp x))
                                                      (not (windowp x))
                                                      (not (timerp x)))
                                                 z
                                               nil))
                                         (setq y x)
                                         nil))
                                    (symbol-plist mud-current))))))
      (mud-define-key mud-current [menu-bar mud proplist]
                      (make-sparse-keymap)
                      (format "%s Properties" (prin1-to-string mud-current)))

      ;; Make the menu. The (reverse) is so that mud-name ends up on top.
      (mud-menu-from-list mud-current [proplist] (reverse proplist))
      (mud-define-key mud-current [menu-bar mud proplist update]
                      'mud-list-props
                      "refresh properties"))))

;;;
;;; Make an entry on the Mud menu using a cons list of (NAME . VALUE)
;;; Can handle VALUE being a vector, list or face, in which case it'll
;;; make a submenu. Actually, this is all just showing off :)
;;;
(defun mud-menu-from-list( mud-current key list )
  "Make a menu from a list of cons cells."

  ;; force the key sequence to be a menu entry
  (if (and (>= (length key) 2)
           (equal (aref key 0) 'menu-bar)
           (equal (aref key 1) 'mud))
      ()
    (setq key (vconcat [menu-bar] [mud] key)))

  ;; process the list
  (while (car list)
    (let* ((elt (car list))
           name val)

      ;; Sanity check. I can think of ways to fix this, but I'm not
      ;; coding them right now.
      (if (consp elt)
          (setq name (car elt)
                val (cdr elt))
        (error "mud-menu-from-list list has a non-cons cell in the list."))

      (or val
          (setq val "nil"))

      ;; Add a menu button for the "category"
      (mud-define-key mud-current (vconcat key
                                           (vector name))
                      (make-sparse-keymap) ;; allow submenus
                      (prin1-to-string name))

      ;; how do we display the list entry?
      (cond
       ;; Vectors & Lists get broken down into submenus
       ;; Should probably be sequencep, maybe.
       ((or (listp val)
            (vectorp val))
        (let ((n 0)
              val-list)
          (setq val-list (mapcar '(lambda(x)
                                    (setq n (+ 1 n))(cons n x))
                                 (if (vectorp val)
                                     (append val nil)
                                   val)))
          (mud-menu-from-list mud-current (vconcat key
                                                   (vector name))
                              (reverse val-list))))

       ;; Faces get their vital statistics displayed as a submenu
       ((facep val)
        (let ((face-details
               (list
                (cons 'underline (or (face-underline-p val) "no"))
                (cons 'stipple (or (face-stipple val) "default"))
                (cons 'background (or (face-background val) "default"))
                (cons 'foreground (or (face-foreground val) "default"))
                (cons 'font (or (face-font val) "default"))
                )))
          (mud-menu-from-list mud-current (vconcat key
                                                   (vector name))
                              face-details)))

       ;; Anything else just gets put in as-is.
       (t
        (mud-define-key mud-current (vconcat key
                                             (vector name)
                                             [val])
                        nil
                        (if (stringp val)
                            val
                          (prin1-to-string val)))))

      (setq list (cdr list)))))

;;;
;;; Idle text generator
;;; This is for people whose connections will time out if they don't
;;; periodically send data. Like me, for example.
;;;
(defun mud-send-idle-message(mud-current)
  (let ((verbose (get mud-current 'mud-idle-chat)))
    (if (eq verbose t)
    (let ((tmp-list mud-idle-messages)
          (n 0 ))
      (while (car tmp-list)
        (setq tmp-list (cdr tmp-list)
              n (+ n 1)))
      (let ((idle-string (nth (random n) mud-idle-messages)))
        (mud-send-string mud-current (concat ": " idle-string
                                             "\n"))))
    (mud-send-string mud-current (get mud-current 'mud-idle-noop)))))

;;;
;;; Reset the mud's idle timer
;;;
(defun mud-idle-timer-reset(mud-current)
  (if (symbolp mud-current)
      (let ((mud-idle-timer (get mud-current 'mud-idle-timer))
            (mud-idle-time (or (get mud-current 'mud-idle-time)
                               mud-default-idle-time
                               "30 min")))
        (and mud-idle-timer
             (cancel-timer mud-idle-timer))
        (setq mud-idle-timer
              (run-at-time mud-idle-time nil 'mud-send-idle-message
                           mud-current))
        (put mud-current 'mud-idle-timer mud-idle-timer))))


;;; COMMAND HISTORY
;;;
;;; To do:
;;;  - Put a limit on the size of the command history.
;;;  - Maybe reset the current command number on more things
;;;    than mud-send-input
;;;  - Put the goddamn commands in an array.  I know this is
;;;    lisp, but really now.
;;;  - mud-grab-next-line at the end of the list should just
;;;    erase the line and leave in blank.

(defun mud-save-string-to-command-history (string)
  "Put STRING at the beginning of the command history list."
  (let ((mud-current (mud-get-from-bin mud-current-list (current-buffer))))
    (put mud-current
         'mud-command-history
         (cons string
               (get mud-current 'mud-command-history)))))

(defun mud-grab-prev-line ()
  "Grab the prev line from the command history and insert it."
  (interactive)
  (mud-grab-a-line 1))

(defun mud-grab-next-line ()
  "Grab the next from the command history and insert it."
  (interactive)
  (mud-grab-a-line -1))

(defun mud-grab-a-line (delta)
  "Move delta lines in the history and insert that line, replacing the current one."
  (let* ((mud-current (mud-get-from-bin mud-current-list (current-buffer)))
         (command-history (get mud-current 'mud-command-history))
         (command-number (get mud-current 'mud-command-to-yank))
         command-iterator)
    (setq command-number (+ delta command-number))
    ;; Yuck, but we shouldn't have to move that far
    (setq command-iterator command-number)
    (if (>= command-iterator 0)
        (progn
          (while (> command-iterator 0)
            (setq command-history (cdr-safe command-history))
            (setq command-iterator (1- command-iterator)))
          (if (consp command-history)
              (let ((command (car command-history)))
                (put mud-current 'mud-command-to-yank command-number)
                ;; Erase current line
                (beginning-of-line)
                (let ((beg (point)))
                  (forward-line)
                  (delete-region beg (point)))
                (insert command)
                (delete-backward-char 1)        ; delete the newline, ha ha
                ))))))


;;; COLORING DIFFERENT TOPICS DIFFERENTLY
;;;
;;; coding by dfan, per-mud mods and horrendous indenting by waider
(defun mud-get-color-for-topic (mud-current topic)
  "Return a (COLOR . MARKER) pair associated with TOPIC in MUD-CURRENT."
  (let* ((mud-topic-color-alist (get mud-current
                                     'mud-topic-color-alist))
         (item (assoc topic mud-topic-color-alist)))
    (if item
        (cdr item)
      (mud-get-color-for-new-topic mud-current topic))))

(defun mud-get-color-for-new-topic (mud-current topic)
  "Find a color in MUD-CURRENT for the new topic TOPIC.
Insert it into mud-topic-color-alist and return its pair."
  (let* ((mud-topic-color-array (get mud-current
                                     'mud-topic-color-array))
         (mud-num-colored-topics (get mud-current
                                      'mud-num-colored-topics))
         (mud-topic-color-alist (get mud-current
                                     'mud-topic-color-alist))
         (num-colors (length mud-topic-color-array))
         topic-color)

    ;; Either add a new color or recycle an old one.
    (setq topic-color
          (if (< mud-num-colored-topics num-colors)
              ;; We have room to just make a new one
              (let ((this-pair
                     (cons (aref mud-topic-color-array
                                 mud-num-colored-topics) 0)))
                (setq mud-topic-color-alist
                      (append mud-topic-color-alist
                              (list (cons topic this-pair))))
                (setq mud-num-colored-topics (1+ mud-num-colored-topics))
                this-pair)

            ;; Find the element with the oldest marker, and overwrite it.
            (let ((oldest-color-assoc (cons "" (cons "" -1))))
              (mapcar (function
                       (lambda (x)
                         (if (or (= (cdr (cdr oldest-color-assoc)) -1)
                                 (< (cdr (cdr x))
                                    (cdr (cdr oldest-color-assoc))))
                             (setq oldest-color-assoc x))))
                      mud-topic-color-alist)
              (setcar oldest-color-assoc topic)
              (cdr oldest-color-assoc))))

    ;; Store all the variables we munged
    (put mud-current 'mud-num-colored-topics mud-num-colored-topics)
    (put mud-current 'mud-topic-color-alist mud-topic-color-alist)

    ;; return the color we picked
    topic-color))

(defun mud-color-topic (mud-current topic start end)
  "Colorize the text from START to END according to TOPIC."
  (let ((color-pair (mud-get-color-for-topic mud-current topic)))
    (let ((topic-face (facemenu-get-face
                       (intern (concat "fg:" (car color-pair))))))
      (setcdr color-pair end)           ; update the marker
      (put-text-property start end 'face topic-face))))



;;; NERDSHOLM SPECIFIC STUFF
;;;
;;; If you have to ask, uh, yeah. Whatever. The exit's two doors
;;; down. Don't let the door hit you on the way out.
(defvar mud-bbs-regexp "^%% \\S-+ has posted to the BBS")
(defvar mud-dir-regexp
  "^%% \\S-+ has set an entry in the participants' directory")

(defun mud-Nerdsholm-setup()
  ;; Nerdsholm has a bbs tied to it. Posting to the bbs generates a
  ;; message in the mud:
  ;; %% waider has posted to the BBS re: presentation of topics (usage)

  ;; And there's the participants' directory:
  ;; %% waider has set an entry in the participants' directory
  (put 'Nerdsholm 'mud-bbs-url "http://nerdsholm.boutell.com/bbs.cgi")
  (put 'Nerdsholm 'mud-dir-url "http://nerdsholm.boutell.com/dir.cgi")
  (let ((regexps-list mud-default-regexps-list))
    (add-to-list 'regexps-list
                 (vector mud-bbs-regexp 'mud-highlight-bbs))
    (add-to-list 'regexps-list
                 (vector mud-dir-regexp 'mud-highlight-dir))
    (put 'Nerdsholm 'mud-regexps-list regexps-list)))

;;;
;;; Like URL matching, really
;;;
(defun mud-highlight-bbs( mud-current &optional match-stuff )
  "Stick a URL onto BBS post alerts."
  (let ((mud-bbs-url (get mud-current 'mud-bbs-url)))
    (goto-char (nth 1 match-stuff))
    (insert (format " (%s)" mud-bbs-url))))

(defun mud-highlight-dir( mud-current &optional match-stuff )
  "Stick a URL onto directory post alerts."
  (let ((mud-dir-url (get mud-current 'mud-dir-url)))
    (goto-char (nth 1 match-stuff))
    (insert (format " (%s)" mud-dir-url))))

;; just in case someone wants to require this...
(provide 'mud)
