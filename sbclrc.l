;; If the first user-processable command-line argument is a filename,
;; disable the debugger, load the file handling shebang-line and quit.
(let ((script (and (second *posix-argv*)
                   (probe-file (second *posix-argv*)))))
  (when script
    ;; Handle shebang-line
    (set-dispatch-macro-character #\# #\!
                                  (lambda (stream char arg)
                                    (declare (ignore char arg))
                                    (read-line stream)))
    ;; Disable debugger
    (setf *invoke-debugger-hook*
          (lambda (condition hook)
            (declare (ignore hook))
            ;; Uncomment to get backtraces on errors
            ;; (sb-debug:backtrace 20)
            (format *error-output* "Error: ~A~%" condition)
            (quit)))
    (load script)
    (quit)))
