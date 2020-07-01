;;; output-org.el ------------ Exports org-mode files -*- lexical-binding: t -*-

;; Matt Bisson <mbisson@ccs.neu.edu>
;; Homepage:		https://cyberbisson.com/
;; Keywords:		org export HTML
;; Last Major Edit:	29/06/2019

;;; Commentary:

;; This file streamlines the process of exporting `org-mode' files to a more
;; common format (e.g., HTML) so that it can be executed from the command line.
;; The intention is not to load this into a running Emacs process, but to run in
;; batch mode.
;;
;; The useful entry point for this is `output-org'.  As an example, take a
;; Makefile target:
;;
;; $(OUTPUT_DIR)/%.html: %.org
;; 	emacs --batch -l ~/elisp/output-org \
;; 	   --eval '(output-org "$(dir $<)" "$(dir $@)" "$(notdir $<)")'
;;
;; This translates the file from .org to .html, looking in the source directory,
;; and exporting to the destination.

;;; To-do:

;; - Want some way to post-process the file to add spacing after punctuation.
;; - Need a Confluence friendly syntax highlighting scheme for
;;   `org-html-htmlize-output-type'

;;; Code:

(require 'ox-publish)

;; These shouldn't need explicit loading, but they are not installed on all
;; systems.
(eval-and-compile
  (or (require 'htmlize nil t)
      (load "/usr/share/emacs/site-lisp/htmlize/htmlize" nil t))
  (or (require 'protobuf-mode nil t) (load "~/elisp/protobuf-mode" nil t)))

;; Syntax coloration used in code samples.
(when noninteractive
  (defconst bg-light-faces
    ;; face                         fg              bg  st  b   i   u
    '((font-lock-builtin-face       "DodgerBlue4"   nil nil t   nil nil)
      (font-lock-comment-face       "DarkGreen"     nil nil nil t   nil)
      (font-lock-constant-face      "CadetBlue"     nil nil nil nil nil)
      (font-lock-doc-face           "Green4"        nil nil nil t   nil)
      (font-lock-function-name-face "OrangeRed2"    nil nil nil t   nil)
      (font-lock-keyword-face       "FireBrick"     nil nil t   nil nil)
      (font-lock-string-face        "Chocolate"     nil nil nil nil nil)
      (font-lock-type-face          "Maroon"        nil nil nil nil nil)
      (font-lock-variable-name-face "SteelBlue"     nil nil nil nil nil))
    "The set of `font-lock-mode' faces when the background is light."))

;; -----------------------------------------------------------------------------
;; `confluence' tag based output support:
;;
;; See the `htmlize' package, and specifically the `htmlize-output-type' for
;; background.  The following set of functions enabls a "Confluence" output type
;; for htmlize.  We need this because Confluence only honors part of each output
;; type's styling when pasting code into it.  Essentially:
;;
;; - If you use `inline-css', you get colors, some font mark-up, but no italics.
;; - If you use `font', you get full mark-up, but only black-and-white.
;;
;; As it turns out, Confluence strips out most CSS that relates to bold, italic,
;; and so on, but leaves <b>, <i>, and others in place.  It strips out <font>
;; tags with a color setting, but allows <span> with a style that specifies the
;; color property.  Go figure.
;; -----------------------------------------------------------------------------

(defun htmlize-confluence-body-tag (face-map)
  "Define the <body> tag for a page with custom coloration.  FACE-MAP will be
checked for the `default' face."
  (let ((fstruct (gethash 'default face-map)))
    (format "<body text=\"%s\" bgcolor=\"%s\">"
            (htmlize-fstruct-foreground fstruct)
            (htmlize-fstruct-background fstruct))))

(defun htmlize-confluence-pre-tag (face-map)
  "Define the <pre> tag for a page with custom coloration.  FACE-MAP will be
checked for the `default' face."
  (if htmlize-pre-style
      (let ((fstruct (gethash 'default face-map)))
        (format "<pre text=\"%s\" bgcolor=\"%s\">"
                (htmlize-fstruct-foreground fstruct)
                (htmlize-fstruct-background fstruct)))
    (format "<pre>")))

(defun htmlize-confluence-text-markup (fstruct-list buffer)
  "Perform text markup for FSTRUCT-LIST in BUFFER.

In `confluence' mode, we mostly use the traditional HTML means of altering
presentation: <b> for bold, <u> for underline, and <strike> for strike-through.
For colors, however, we must use inline-css, or confluence does everything in
black and white.

FSTRUCT-LIST is an `htmlize-fstruct' type that describes the font properties for
the text in consideration.  BUFFER is the buffer being written to, and may be
used as an output stream for `printc'.

This function returns a lambda that prints the markup to BUFFER."
  (let* ((merged (htmlize-merge-faces fstruct-list))
         (markup (htmlize-memoize
                  merged
                  `(,(concat
                      (if (htmlize-fstruct-foreground merged)
                          (format "<span style=\"color:%s\">"
                                  (htmlize-fstruct-foreground merged)))
                      (if (htmlize-fstruct-boldp merged)      "<b>")
                      (if (htmlize-fstruct-italicp merged)    "<i>")
                      (if (htmlize-fstruct-underlinep merged) "<u>")
                      (if (htmlize-fstruct-strikep merged)    "<strike>"))
                    .
                    ,(concat
                      (if (htmlize-fstruct-strikep merged)    "</strike>")
                      (if (htmlize-fstruct-underlinep merged) "</u>")
                      (if (htmlize-fstruct-italicp merged)    "</i>")
                      (if (htmlize-fstruct-boldp merged)      "</b>")
                      (if (htmlize-fstruct-foreground merged) "</span>"))))))
    (princ (car markup) buffer)

    ;; Using lexical-binding, MARKUP and BUFFER will be in the closure returned
    ;; by this function.
    (lambda () (princ (cdr markup) buffer))))

;; -----------------------------------------------------------------------------
;; "Public" functions:
;; -----------------------------------------------------------------------------

(defun output-org (in-dir out-dir file-name)
  "Export a .org file specified by FILE-NAME in another format.

The IN-DIR parameter specifies the location of the .org file, and OUT-DIR
specifies where the function places the new file."

  ;; If this is a batch-mode Emacs process, there will be no syntax coloration,
  ;; but we really want this to match what would be generated from an
  ;; interactive Emacs session.
  (when noninteractive (dolist (face-spec bg-light-faces)
                         (apply #'modify-face face-spec)))

  (let ((make-backup-files nil) ; Export process makes backup files.
        (org-publish-project-alist
         `(("makefile-page" :components ("makefile-generated"))
           ("makefile-generated"
            :base-directory ,in-dir
            :base-extension "org"
            :publishing-directory ,out-dir

            ;; Only output exactly what's been asked for.
            :exclude ".*"
            :include (,file-name)
            :recursive nil

            :publishing-function org-html-publish-to-html

            :with-smart-quotes t)))
        ;; This has to be done for Confluence, because the stupid web GUI strips
        ;; out CSS when I paste in source code snippets.
        (org-html-htmlize-output-type 'confluence))

    (org-publish-all t)))

;;; output-org.el ends here
