;;; vmw-c-dev.el --- VMware-specific tooling          -*- lexical-binding: t -*-

;; Matt Bisson <mbisson@ccs.neu.edu>
;; Homepage:		https://cyberbisson.com/
;; Keywords:		development VMware
;; Last Major Edit:	02/07/2024

;;; Commentary:

;; This (Emacs Lisp) module contains utilities that are useful for C/C++
;; software development at VMware.
;;
;; The following code will allow proper expansion of macros:
;;   (require 'vmw-c-dev)
;;   (add-hook 'c-mode-hook   #'vmw-c-macro-initialize)
;;   (add-hook 'c++-mode-hook #'vmw-c-macro-initialize)

;;; BUGS:
;; - Probably this should be a minor mode since we use so many buffer-local
;;   variables.
;; - Currently $VMWARE_SRCDIR has to be defined as the path where your source
;;   tree begins (i.e., bora is found under it)!!
;; - `vmw-insert-file-header' should do different things based on the
;;   prog-mode of the buffer.

;;; Code:

(eval-when-compile
  (require 'cc-mode)
  (require 'cmacexp)
  (require 'grep))

;; -----------------------------------------------------------------------------
;; Variables and constants:
;; -----------------------------------------------------------------------------

;; The first step is to make the normally global preprocessor variables
;; buffer-local, so we can customize them for each language and project.
(make-variable-buffer-local 'c-macro-cppflags)
(make-variable-buffer-local 'c-macro-preprocessor)
(make-variable-buffer-local 'vmw-c-macro-initialized-p)

(defvar vmw-c-macro-initialized-p nil
  "If T, `c-macro-preprocessor' and `c-macro-cppflags' have been set for the
current buffer.")

;; -----------------------------------------------------------------------------
;; "Public" functions:
;; -----------------------------------------------------------------------------

(defun vmw-insert-file-header ()
  "Provide a boiler-plate C/C++ file header for VMware (now Broadcom) sources."

  (interactive)
  (insert (concat "\
/* *****************************************************************************
 * Copyright (c) " (int-to-string (nth 5 (decode-time))) " Broadcom. All Rights Reserved.
 * Broadcom Confidential. The term \"Broadcom\" refers to Broadcom Inc.
 * and/or its subsidiaries.
 * ****************************************************************************/

/**
 * @file
 *    TODO: Brief description of this file.
 */
")))

(defun vmw-c-macro-initialize (&optional force)
  "Update the values used by `c-macro-expand'.

Normally these values are cached (per-buffer), but they can be explicitly
updated with the FORCE flag.

Invoking this function makes the normally global variables, `c-macro-cppflags'
and `c-macro-preprocessor' buffer-local.  This function applies preprocessor
settings dynamically based on the particular buffer's location and language,
which is not possible with global preprocessor settings."

  (interactive)

  ;; TODO: This gets called in the *Macroexpand* buffer, too!  Also, having a
  ;; nil filename should be OK.
  (when buffer-file-name
    (when (or force (not vmw-c-macro-initialized-p))
      (setq c-macro-preprocessor "vmw-cpp")
      (setq c-macro-cppflags
            (concat "--stdin --product=esx --buildtype=obj "
                    "--vmtree=" (getenv "VMWARE_SRCDIR") "/bora "
                    buffer-file-name)))))

;; When ripgrep exists in the path, use it for `grep-find' instead of grep.
;; This must be applied only after loading the grep library.  (NOTE: "40"
;; applies to the position within the '' where the interactive user types the
;; search expression.
(when (executable-find "rg")
  (eval-after-load
      'grep
    '(grep-apply-setting
      'grep-find-command
      '("rg -nH --no-heading --color=always -e '' \"${VMWARE_SRCDIR}\"" . 40))))

(provide 'vmw-c-dev)

;;; vmw-c-dev.el ends here
