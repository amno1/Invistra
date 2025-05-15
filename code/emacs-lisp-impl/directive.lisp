(in-package #:invistra)

(defclass elisp-parser (parser) ())

(defclass elisp-directive (directive)
  ((client
    :initarg :client :accessor directive-client)
   (width
    :initarg :width :type integer :initform 0 :accessor argument-width)
   (precision
    :initarg :precision :type integer :accessor argument-precision)
   (prefix
    :initarg :prefix :initform nil :accessor argument-prefix)
   (padding
    :initarg :padding :type integer :initform 0 :accessor argument-padding)
   (padchar
    :initarg :padchar :type (or null character) :initform #\Space :accessor argument-padchar)
   (sign-char
    :initarg :sign-char :type (or null character) :initform nil :accessor sign-char)
   (padright
    :initarg :padright :type boolean :initform nil :accessor pad-right-p)
   (print-sign
    :initarg :print-sign :type boolean :initform nil :accessor print-sign-p)
   (pretty
    :initarg :pretty-print :initform nil :type (or null boolean) :accessor pretty-print)
   (restrict-width
    :initarg :restrict-width :type boolean :initform nil :accessor restrict-width)))

(defmethod parse-directive (tokens control-string parser)
  (let* ((last (array-last control-string))
         (directive (new 'elisp-directive
                         :directive-character last
                         :control-string control-string))
         stack token)
    (while tokens
           (let ((token (pop tokens)))
             (cond
               ((integerp token)
                (push token stack))
               ((eql token #\$)
                (if (integerp (car stack))
                    (setf (directive-argument directive) (pop stack))
                    (error "Invalid format operation '$', ~a" control-string)))
               ((eql token #\#)
                (if (null stack)
                    (setf (argument-prefix directive) t)
                    (error "Invalid format operation '#', ~a" control-string)))
               ((eql token #\+)
                (if (null stack)
                    (unless (find last "csS")
                      (setf (print-sign-p directive) t
                            (sign-char directive) #\+))
                    (error "Invalid format operation '+', ~a" control-string)))
               ((eql token #\-)
                (if (null stack)
                    (setf (slot-value directive 'padright) t)
                    (error "Invalid format operation '-', ~a" control-string)))
               ((eql token #\Space)
                (if (null stack)
                    ;; + has precedence over space in elisp
                    (unless (eql (sign-char directive) #\+)
                      (setf (print-sign-p directive) t
                            (sign-char directive) #\ ))
                    (error "Invalid format operation ' ', ~a" control-string)))
               ((eql token #\0)
                (when (find last "doxXefg")
                  (setf (argument-padchar directive) #\0)))
               ((eql token #\.)
                (if (or (null stack) (integerp (car stack)))
                    (setf (argument-precision directive)
                          (if (integerp (car tokens))
                              (pop tokens)
                              0))
                    (error "Invalid format operation '.', ~a" control-string))))))

    (when stack
      (setf token (pop stack))
      (if (integerp token)
          (setf (argument-width directive) token
                (restrict-width directive) t)
          (error "Invalid format operation ~a, ~a" token control-string)))

    (when stack
      (error "Invalid format operation ~a, ~a" (car stack) control-string))

    (unless (slot-boundp directive 'precision)
      (setf (slot-value directive 'precision)
            (if (find last "efg") 6 0)))
    
    (setf (consume-argument-p directive) t)

    directive))

;;; elisp-directive.lisp ends here
