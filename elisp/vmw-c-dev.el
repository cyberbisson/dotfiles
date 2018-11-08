;;; vmw-c-dev.el --- VMware-specific tooling          -*- lexical-binding: t -*-

;; Matt Bisson <mbisson@ccs.neu.edu>
;; Homepage:		https://cyberbisson.com/
;; Keywords:		initialization
;; Last Major Edit:	08/14/2018

;;; Commentary:

;; This (Emacs Lisp) module contains utilities that are useful for C/C++
;; software development at VMware.
;;
;; The following code will allow proper expansion of macros:
;;   (require 'vmw-c-dev)
;;   (vmw-update-cpp-and-flags)
;;   (add-hook 'c-mode-hook   'vmw-set-cmacexp-data)
;;   (add-hook 'c++-mode-hook 'vmw-set-cmacexp-data)

;;; BUGS:
;; - Currently $RP_SRCDIR has to be defined as the path where your source
;;   tree begins (i.e., bora is found under it)!!
;; - `vmw-insert-file-header' should do different things based on the
;;   prog-mode of the buffer.

;;; Code:

(eval-when-compile
  (require 'cc-mode)
  (require 'cmacexp))

;; -----------------------------------------------------------------------------
;; Variables and constants:
;; -----------------------------------------------------------------------------

(defvar vmw-cached-cpp-file nil
  "The discovered filename used for VMware's C/C++ preprocessing.")

(defvar vmw-cached-cpp-flags nil
  "A string representing the preprocessor options that must be passed to
preprocess VMware code.")

(defconst vmw-preproc-defs-list
  '("BOOST_FILESYSTEM_VERSION=3"
    "BOOST_SYSTEM_NO_DEPRECATED=1"
    "BOOST_THREAD_DONT_USE_CHRONO=1"
    "GTEST_HAS_TR1_TUPLE=1"
    "GTEST_USE_OWN_TR1_TUPLE=0"
    "STATIC_REPCOMMON_API"
    "STATIC_REPLICATION_API"
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
    "_XOPEN_SOURCE=600"
    "linux")
  "The list of VMware preprocessor definitions.")

(defvar vmw-added-cc-mode-hooks nil
  "Ensures hooks for `cc-mode' are only added once.")

;; -----------------------------------------------------------------------------
;; Utility functions
;; -----------------------------------------------------------------------------

(defun vmw-find-first-ob (directory-name)
  "Finds the last name under DIRECTORY-NAME that it encounters with a VMware
official build ID.  Note that this finds the first name, and not the 'correct'
name, which would require parsing build files.  The hope is that this is close
enough.

If the supplied directory is not an absolute path, the function looks for the
$RP_SRCDIR variable, and appends that to the beginning to construct a local
component cache directory."

  (let ((my-dirname
         (if (file-name-absolute-p directory-name) directory-name
           (concat (getenv "RP_SRCDIR")
                   "/bora/build/package/COMPONENTS/" directory-name))))
    (if (not (file-exists-p my-dirname))
        (concat "/DUMMY/NOT/FOUND" my-dirname)
      (car (reverse (directory-files my-dirname t "ob-[0-9]+"))))))

(defun vmw-generate-cpp-and-flags ()
  "Creates a list that contains the C/C++ preprocessor command along with all
arguments required to preprocess VMware sources."

  (when (not (file-exists-p "/build/mts/sharedcompcache/cayman_esx_toolchain"))
    (error "No ESX toolchain present for CPP flag generation"))

  (let ((compcache-map
         (mapcar
          (lambda (pair-in)
            (cons (car pair-in) (vmw-find-first-ob (cdr pair-in))))
          '(;; Shared components
            (boost . "/build/mts/sharedcompcache/cayman_boost")
            (glibc . "/build/mts/sharedcompcache/cayman_esx_glibc")
            (openssl . "/build/mts/sharedcompcache/cayman_openssl")
            (toolchain . "/build/mts/sharedcompcache/cayman_esx_toolchain")
            ;; Non-shared components
            (googletest . "cayman_googletest")
            (protobuf . "cayman_protobuf")
            (sqlite . "cayman_sqlite")
            (zlib . "cayman_zlib")))))
    (cons
     (concat
      "cd " (getenv "RP_SRCDIR") " && "
      (alist-get 'toolchain compcache-map)
      "/linux64/usr/bin/x86_64-vmk-linux-gnu-g++ "
      "-x c++ -std=c++14 -O1 -E -C -o - -")
     (vmw-generate-cpp-flags-list compcache-map))))

(defun vmw-generate-cpp-flags-list (compcache-map)
  "Generates the full list of data that should be passed to the preprocessor for
VMware sources.

COMPCACHE-MAP must be in the form of an alist with the required 'compcache'
components mapped to their ID."

  (append
   (mapcar (lambda (def) (concat "-D" def)) vmw-preproc-defs-list)
   (list
    (concat "--sysroot=" (alist-get 'glibc compcache-map) "/linux64/sysroot")
    (concat "-include " (getenv "RP_SRCDIR") "/bora/public/vm_cpp11.h")
    (concat "-isystem " (alist-get 'toolchain compcache-map)
            "/linux64/usr/lib/gcc/x86_64-vmk-linux-gnu/6.4.0/include"))
   (mapcar (lambda (inc) (concat "-I" inc))
           (vmw-preproc-includes-list compcache-map))))

(defun vmw-preproc-includes-list (compcache-map)
  "Return the list of VMware preprocessor include directories.

COMPCACHE-MAP must be in the form of an alist with the required 'compcache'
components mapped to their ID."

  (list
   "bora/build/build/version"
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
   "bora/public"
   "bora/lib/public"
   "bora/build/build/LIBRARIES/vmodl/generic/obj"
   "bora/vim/lib/public"
   (concat (alist-get 'zlib compcache-map) "/linux64/esx64+gcc6/usr/include")
   (concat (alist-get 'zlib compcache-map)
           "linux64/esx64+gcc6/usr/include/minizip")
   (concat (alist-get 'openssl compcache-map) "/linux64/esx64+gcc6/include")
   (concat (alist-get 'boost compcache-map) "/linux64/include")
   (concat (alist-get 'googletest compcache-map) "/linux64/esx64+gcc6/include")
   (concat (alist-get 'protobuf compcache-map) "/linux64/esx64-cayman/include")
   (concat (alist-get 'sqlite compcache-map) "/linux64/esx64+gcc6/include")
   "bora/vim/lib/public"
   "bora/vsan/lib/dp/include"
   "bora/vsan/dp/rpd/include"
   "bora/vsan/lib/dp/include"
   "bora/modules/vmkernel/vsan/include"
   "bora/vsan/lib/hostctl/include"
   "bora/public"
   "bora/build/build/HEADERS/rpIpcProtoDir/uw64/obj"))

;; -----------------------------------------------------------------------------
;; "Public" functions:
;; -----------------------------------------------------------------------------

(defun vmw-c-macro-expand (start end subst)
  "Expand C macros in the region, using the C preprocessor as is done with
`c-macro-expand'.

The only difference between this function and the default is that this function
adds the current directory of the buffer being examined to the include path.
This allows the function to operate successfully on files that use quoted
include directives, relying on local header files.

Noninteractive args are START, END, SUBST.  See also `c-macro-expansion'."

  (interactive "r\nP")

  (let ((c-macro-cppflags (concat c-macro-cppflags " -I" default-directory)))
    (c-macro-expand start end subst)))

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

(defun vmw-set-cmacexp-data ()
  "Update the configuration settings used by `c-macro-expand' based on valued
cached by the `vmw-update-cpp-and-flags' function."
  (interactive)

  (if (and vmw-cached-cpp-file vmw-cached-cpp-flags)
      (setq c-macro-preprocessor vmw-cached-cpp-file
            c-macro-cppflags     vmw-cached-cpp-flags)))

(defun vmw-update-cpp-and-flags ()
  "This function updates (cached) information about where the approprate
compiler is and what parameters should be passed to preprocess VMware sources.
This information is left in the variables, `vmw-cached-cpp-file' and
`vmw-cached-cpp-flags'."
  (interactive)

  (let ((generated-data (vmw-generate-cpp-and-flags)))
    (setq vmw-cached-cpp-file (car generated-data)
          vmw-cached-cpp-flags (mapconcat 'identity (cdr generated-data) " ")))

  (when (not vmw-added-cc-mode-hooks)
    (add-hook 'c-initialization-hook
              (lambda ()
                (define-key c-mode-map   "\C-c\C-e" 'vmw-c-macro-expand)
                (define-key c++-mode-map "\C-c\C-e" 'vmw-c-macro-expand)))
    (setq vmw-added-cc-mode-hooks t))

  t)

(provide 'vmw-c-dev)

;;; vmw-c-dev.el ends here
