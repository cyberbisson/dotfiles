;;; vmw-c-dev.el --- VMware-specific tooling          -*- lexical-binding: t -*-

;; Matt Bisson <mbisson@ccs.neu.edu>
;; Homepage:		https://cyberbisson.com/
;; Keywords:		development VMware
;; Last Major Edit:	07/09/2020

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
;; - Updating the GOBUILD settings doesn't currently force updates on all
;;   buffers that have already cached preprocessor settings.  It only works on
;;   the buffer where the update was first requested, unfortunately.
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

(defvar vmw-c-macro-compcache nil
  "A cache of located GOBUILD components.")

(defvar vmw-c-macro-defs-alist
  `((nil . ("BOOST_FILESYSTEM_VERSION=3"
            "BOOST_SYSTEM_NO_DEPRECATED=1"
            "BOOST_THREAD_DONT_USE_CHRONO=1"
            "GTEST_HAS_TR1_TUPLE=1"
            "GTEST_USE_OWN_TR1_TUPLE=0"
            "USERLEVEL"
            "VMK_ENABLE_ALL_VMKAPIS"
            "VMWARE_BOOST_ASIO_DISABLE_VISIBILITY"
            "VMX86_DEBUG"
            "VMX86_DEVEL"
            "VMX86_LOG"
            "VMX86_SERVER"
            "VMX86_STATS"
            "VM_BOOST_HAS_BROKEN_MARK_COUNT=0"
            "_BSD_SOURCE"
            "_FILE_OFFSET_BITS=64"
            "_FORTIFY_SOURCE=2"
            "_GLIBCXX_USE_CXX11_ABI=0"
            "_LARGEFILE64_SOURCE"
            "_SVID_SOURCE"
            "_XOPEN_SOURCE=700"
            "linux"))
    ;; TODO: lists should cascate instead of first-match.
    ("bora/esx/apps/.*/ipcManager" . ("CUSTOMIZABLE_ENVIRONMENT"
                                      "STATIC_IPC_MANAGER_API"
                                      "STATIC_IPC_MANAGER_CPP_API"))
    ("bora/esx/apps/dp/iofilter" . ("CUSTOMIZABLE_ENVIRONMENT"
                                    "VMK_DEVKIT_HAS_API_VMKAPI_BASE"
                                    "VMK_DEVKIT_USES_PUBLIC_APIS"
                                    "VMK_DEVKIT_USES_BINARY_COMPATIBLE_APIS"
                                    "VMIOF_NAME=vmwarelwd"
                                    "VMIOF_VENDOR=VMW"
                                    "VMIOF_VERSION=xyz"
                                    "VMIOF_API_VERSION=1.0",
                                    "VMIOF_CLASS=replication"
                                    "USE_ROARING"))
    ("bora/esx/apps/dp/" . ("CUSTOMIZABLE_ENVIRONMENT")))
  "An association list that maps a source tree to its preprocessor definitions.

The key for the list is a regular expression, and files will be compared against
that as they are processed.  Note that only the first match will apply to the
file.  The NIL entry applies globally to all buffers.")

(defvar vmw-c-macro-explicit-build-alist
  '((toolchain . "ob-15599702"))
  "Explicitly specifies official build numbers for GOBUILD dependencies.")

(defvar vmw-c-macro-include-alist
  `((nil . ("bora/build/build/version"
            "bora/vmkernel/include/public"
            "bora/vmkernel/include/user"
            "bora/vmkernel/include/user/x86"
            "bora/vmkernel/vsi"
            "bora/vmkernel/vsi/x86"
            "bora/vmkernel/public"
            "bora/vmkernel/public/x86"
            "bora/vmkernel/distribute"
            "bora/vmcore/public"
            "bora/vmcore/public/x86"
            "bora/lib/public"
            "bora/public"
            "bora/vmx/public"
            "bora/build/build/HEADERS/vmkapi-current-all/generic/obj"
            "bora/build/build/HEADERS/lib-vmksysinfo-headers/uw64/obj"
            "bora/build/build/LIBRARIES/vmodl/generic/obj"
            "bora/vim/lib/public"))
    ;; TODO: lists should cascate instead of first-match.
    ("bora/esx/apps/dp/iofilter" . ("bora/esx/apps/dp/include"
                                    "bora/build/build/HEADERS/dp-proto/uw64/obj"
                                    "bora/lib/vmkuser/include/public"))
    ("bora/esx/apps/dp/" . ("bora/esx/apps/dp/include"
                            "bora/build/build/HEADERS/dp-proto/uw64/obj")))
  "An association list that maps a source tree to its include paths.

The key for the list is a regular expression, and files will be compared against
that as they are processed.  Note that only the first match will apply to the
file.  The NIL entry applies globally to all buffers.  GOBUILD paths are added
dynamically after this list.")

;; TODO: Can we cache like buffers so we don't rebuild the exact same settings
;;       every time??
(defvar vmw-c-macro-initialized-p nil
  "If T, `c-macro-preprocessor' and `c-macro-cppflags' have been set on the
current buffer.")

(defvar vmw-c-macro-preprocessor-alist
  `((c-mode . ,(concat "/linux64/usr/bin/x86_64-vmk-linux-gnu-gcc "
                       "-x c -std=gnu11 -O1 -E -C -o - -"))
    (c++-mode . ,(concat "/linux64/usr/bin/x86_64-vmk-linux-gnu-g++ "
                         "-x c++ -std=c++14 -O1 -E -C -o - -")))
  "Defines the preprocessor used for a given `major-mode'.  The toolchain path
will be appended before this string.")

;; -----------------------------------------------------------------------------
;; Utility functions
;; -----------------------------------------------------------------------------

(defun vmw-c-macro-alist-test (file-regex file-name)
  "Execute a test on the VMware macro association lists.

This performs a regular expression search on the associate lists, using the
key (specified by FILE-REGEX), on name of the file being preprocessed, as
specified by FILE-NAME."
  (if file-regex (string-match-p file-regex file-name)))

(defun vmw-c-macro-generate-cpp-flags (file-name file-mode)
  ""
  (let ((defs-list (vmw-c-macro-get-defs file-name))
        (incs-list (vmw-c-macro-get-includes file-name)))
    (append
     (mapcar #'(lambda (def) (concat "-D" def)) defs-list)
     (list
      (concat "--sysroot=" (alist-get 'glibc vmw-c-macro-compcache)
              "/linux64/sysroot")
      (when (eq file-mode 'c++-mode)
        (concat "-include " (getenv "VMWARE_SRCDIR") "/bora/public/vm_cpp11.h"))
      (concat "-isystem " (alist-get 'toolchain vmw-c-macro-compcache)
              "/linux64/usr/lib/gcc/x86_64-vmk-linux-gnu/6.4.0/include")
      ;; Wrong, but the only thing that works for quoted includes??
      (concat "-I" (file-name-directory file-name)))
     (mapcar #'(lambda (inc) (concat "-I" inc)) incs-list))))

(defun vmw-c-macro-get-defs (file-name &optional defs-alist)
  "Get the list of preprocessor definitions applicable to FILE-NAME.

Optionally, DEFS-ALIST specifies custom definitions.  It unspecified, this
defaults to `vmw-c-macro-defs-alist'."
  (let ((defs-alist (or defs-alist vmw-c-macro-defs-alist)))
    (append (alist-get nil defs-alist)
            (alist-get file-name
                       defs-alist
                       nil nil ; No default, no removal.
                       #'vmw-c-macro-alist-test))))

(defun vmw-c-macro-get-includes (file-name &optional include-alist)
  "Provide an include path for FILE-NAME.

Optionally, INCLUDE-ALIST specifies custom definitions.  It unspecified, this
defaults to `vmw-c-macro-include-alist'."
  (if (null vmw-c-macro-compcache) (vmw-c-macro-populate-compcache))

  (let ((include-alist (or include-alist vmw-c-macro-include-alist)))
    (append
     (alist-get nil include-alist)
     (alist-get file-name
                include-alist
                nil nil ; No default, no removal.
                #'vmw-c-macro-alist-test)
     (list (concat (alist-get 'zlib vmw-c-macro-compcache)
                   "/linux64/esx64+gcc6/usr/include")
           (concat (alist-get 'zlib vmw-c-macro-compcache)
                   "linux64/esx64+gcc6/usr/include/minizip")
           (concat (alist-get 'openssl vmw-c-macro-compcache)
                   "/linux64/esx64+gcc6/include")
           (concat (alist-get 'croaring vmw-c-macro-compcache)
                   "/linux-centos8/esx64+glibc217+gcc6/include")
           (concat (alist-get 'boost vmw-c-macro-compcache)
                   "/linux-centos8/include")
           (concat (alist-get 'googletest vmw-c-macro-compcache)
                   "/linux64/esx64+gcc6/include")
           (concat (alist-get 'protobuf vmw-c-macro-compcache)
                   "/linux64/esx64-cayman/include")
           (concat (alist-get 'protobuf-c vmw-c-macro-compcache)
                   "/linux64/esx64-cayman/include")
           (concat (alist-get 'sqlite vmw-c-macro-compcache)
                   "/linux64/esx64+gcc6/include")))))

(defun vmw-c-macro-populate-compcache ()
  "Examine the component cache for include directories.

This function caches the result of its examination in `vmw-c-macro-compcache',
as this operation may take some time."
  (when (not (file-exists-p "/build/mts/sharedcompcache/cayman_esx_toolchain"))
    (error "No ESX toolchain present for CPP flag generation"))

  (setq vmw-c-macro-compcache
        (mapcar
         #'(lambda (pair-in)
             (cons (car pair-in)
                   (vmw-find-first-ob (cdr pair-in)
                                      ;; Explicit build options:
                                      (car pair-in)
                                      vmw-c-macro-explicit-build-alist)))
         '(;; Shared components
           (boost . "/build/mts/sharedcompcache/cayman_boost")
           (glibc . "/build/mts/sharedcompcache/cayman_esx_glibc")
           (openssl . "/build/mts/sharedcompcache/cayman_openssl")
           (toolchain . "/build/mts/sharedcompcache/cayman_esx_toolchain")
           ;; Non-shared components
           (croaring . "cayman_croaring")
           (googletest . "cayman_googletest")
           (protobuf . "cayman_protobuf")
           (protobuf-c . "cayman_protobuf_c")
           (sqlite . "cayman_sqlite")
           (zlib . "cayman_zlib")))))

(defun vmw-find-first-ob (directory-name
                          &optional
                          explicit-build-key
                          explicit-build-alist)
  "Finds the last name under DIRECTORY-NAME that it encounters with a VMware
official build ID.  Note that this finds the first name, and not the 'correct'
name, which would require parsing build files.  The hope is that this is close
enough.

If the supplied directory is not an absolute path, the function looks for the
$VMWARE_SRCDIR variable, and appends that to the beginning to construct a local
component cache directory.

The optional parameters, EXPLICIT-BUILD-KEY and EXPLICIT-BUILD-ALIST allow the
caller to override the dynamic detection with a GOBUILD name, and list of
GOBUILD official build IDs.  If the explicitly specified build does not exist,
the function falls back to automatic detection."

  (let ((my-dirname
         (if (file-name-absolute-p directory-name) directory-name
           (concat (getenv "VMWARE_SRCDIR")
                   "/bora/build/package/COMPONENTS/" directory-name))))
    (if (not (file-exists-p my-dirname))
        (concat "/DUMMY/NOT/FOUND" my-dirname)
      (or
       ;; Either return the explicit build directory, assuming everything is OK.
       (let ((explicit-build (and explicit-build-alist explicit-build-key
                                  (alist-get explicit-build-key
                                             explicit-build-alist))))
         (if explicit-build
             (let ((explicit-dir (concat my-dirname "/" explicit-build)))
               (if (file-exists-p explicit-dir) explicit-dir))))
       ;; ... or return a dynamically discovered directory.
       (car (sort
             ;; Find a list of the directories with "ob-" and a bunch of
             ;; numbers.  Then flip through that list, replacing all elements
             ;; that do not have a (non-dot) file within them with NIL.  Lastly,
             ;; `delq' deletes the NIL entries.
             (delq nil
                   (mapcar #'(lambda (nested-file)
                               (if (directory-files nested-file nil "^[^.]")
                                   nested-file
                                 nil))
                           (directory-files my-dirname t "ob-[0-9]+")))
             ;; Reverse "version-order" sort of the found directories...
             #'(lambda (a b) (string-version-lessp b a))))))))

;; -----------------------------------------------------------------------------
;; "Public" functions:
;; -----------------------------------------------------------------------------

(defun vmw-insert-file-header ()
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

(defun vmw-c-macro-initialize (&optional force force-gobuild-refresh)
  "Update the values used by `c-macro-expand'.

Normally these values are cached (per-buffer), but they can be explicitly
updated with the FORCE flag.  The GOBUILD settings are cached globally, and
those can be updated with FORCE-GOBUILD-REFRESH, which implies FORCE.

Invoking this function makes the normally global variables, `c-macro-cppflags'
and `c-macro-preprocessor' buffer-local.  This function applies preprocessor
settings dynamically based on the particular buffer's location and language,
which is not possible with global preprocessor settings.

See the variables, `vmw-c-macro-defs-alist', `vmw-c-macro-explicit-build-alist',
`vmw-c-macro-include-alist', and `vmw-c-macro-preprocessor-alist' for additional
customization points."

  (interactive)

  ;; TODO: This gets called in the *Macroexpand* buffer, too!  Also, having a
  ;; nil filename should be OK.
  (when buffer-file-name
    (when force-gobuild-refresh
      (message "Forcing refresh of GOBUILD settings")
      (setq vmw-c-macro-compcache nil)
      (setq vmw-c-macro-initialized-p nil))
    (if force (setq vmw-c-macro-initialized-p nil))

    (if (null vmw-c-macro-compcache)
      (vmw-c-macro-populate-compcache))

    (if (not vmw-c-macro-initialized-p)
        (let ((major-mode (if (assq major-mode vmw-c-macro-preprocessor-alist)
                              major-mode 'c++-mode))) ;; Fall back to C++.
          (setq c-macro-cppflags (mapconcat #'identity
                                            (vmw-c-macro-generate-cpp-flags
                                             buffer-file-name major-mode)
                                            " "))
          (setq c-macro-preprocessor
                (concat
                 "cd " (getenv "VMWARE_SRCDIR") " && "
                 (alist-get 'toolchain vmw-c-macro-compcache)
                 (alist-get major-mode vmw-c-macro-preprocessor-alist)))))))

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
