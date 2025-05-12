(in-package :invistra)

(defvar *format-parser*)

(defclass parser ()
  ((%base-directive
    :initarg :base-directive :type symbol :accessor base-directive)
   (%case-sensitive-p
    :initarg :case-sensitive :type boolean :initform t :reader case-sensitive-p
    :documentation "Whether directives are case-sensitive or not.")
   (%format-char
    :initarg :format-char :type character :reader format-char
    :documentation "Character starting directives.")
   (%terminals
    :initarg :terminals :type list :accessor parser-terminals
    :documentation "A list of characters that terminates directives.")
   (%flags
    :initarg :flags :type list :accessor parser-flags
    :documentation "A list of flags.

A flag can modify a directives input or how argument prints.")
   (%operators
    :initarg :operators :type list :accessor parser-operators
    :documentation
    "List of operators.

 An operator tells how to use the literal after or before an operator.")))

(defun make-parameter (string position)
  (if (find (aref string position) "123456789")
      (parse-integer string :start position :junk-allowed t)
      (values (aref string position) position)))

(defun directive-string-p (string parser)
  (char= (aref string 0) (format-char parser)))

(defclass literal-directive (directive) ())

(defmethod specialize-directive
    (client (char (eql nil)) (directive literal-directive) end-directive)
  (declare (ignore client char end-directive))
  ;; just a pass through
  directive)

(defmethod interpret-item (client (item literal-directive) &optional args)
  (declare (ignore client args))
  (loop for c across (directive-argument item)
        do (write-char c *destination*)))

;;; parser.lisp ends here
