(in-package :invistra)

(defmethod argument-to-string (directive)
  (let ((string
          (with-output-to-string (s)
            (if (pretty-print directive)
                (princ (directive-argument directive) s)
                (prin1 (directive-argument directive) s)))))
    (if (> (argument-width directive) 0)
        (subseq string 0 (argument-precision directive))
        string)))

(defun print-arg (directive destination)
  "Print value of DIRECTIVE to DESTINATION stream.

Print sign character and right or left padding.
The value of an argument itself is printed to a string in
`argument-to-string' specialisations."
  (let* ((string (argument-to-string directive))
         (sign (sign-char directive))         
         (length (length string))
         (width  (argument-width directive))
         (pad-char (argument-padchar directive))
         (pad-length (max 0 (- width length))))
    (cond
      ((pad-right-p directive)
       (setf pad-char #\Space)
       (when sign
         (write-char sign destination))
       ;; Print the string in reverse order
       (loop for index downfrom (1- length) to 0
             for c across string
             do  (write-char c destination))
       (loop repeat pad-length
             do (write-char pad-char destination)))
      (t ; pad-left
       (cond ((eql pad-char #\0)
              (when sign
                (write-char sign destination))
              (loop repeat pad-length
                    do (write-char pad-char destination)))
             (t
              (loop repeat pad-length
                    do (write-char pad-char destination))
              (when sign
                (write-char sign destination))))
       ;; Print the string in reverse order
       (loop for index downfrom (1- length) to 0
             for c across string
             do (write-char c destination))))))

;;; common-print-functions.lisp ends here
