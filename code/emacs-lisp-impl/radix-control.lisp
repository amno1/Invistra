(in-package #:invistra)

(defclass elisp-radix-directive (elisp-directive)
  ((case
    :initarg :case :initform nil :type symbol :accessor parameter-case)
   (radix
    :initarg :radix :type integer :accessor parameter-radix)))

(defmethod argument-to-string ((parameter elisp-radix-directive))
  (let ((value (directive-argument parameter))
        (radix (parameter-radix parameter)))
    (declare (type fixnum radix))
    ;; (cl:format t "radix: ~a~%" radix)
    (when (minusp value)
      (setf (slot-value parameter 'sign-char) #\-))
    (setf value (if (floatp value) (abs (floor value)) (abs value)))
    (let* ((*print-base* radix)
           (*print-radix* nil)
           (prefix (argument-prefix parameter))
           (print-case (parameter-case parameter))
           (precision (argument-precision parameter)))
      (declare (type fixnum value radix))
      (let ((digit-count (quaviver.math:count-digits radix value)))
        (declare (type fixnum radix value))
        (with-output-to-string (stream)
          (cond
            ((= radix 8)
             (when (<= precision digit-count)
               (princ prefix stream)))
            (t (princ prefix stream)))
          (when (> precision digit-count)
            (loop repeat (- precision digit-count)
                  do (write-char #\0 stream)))
          (princ value stream)          
          (case print-case
            (:upper
             (return-from argument-to-string
               (string-upcase (get-output-stream-string stream))))
            (:lower
             (return-from argument-to-string
               (string-downcase (get-output-stream-string stream))))))))))

(defclass b-elisp-directive (elisp-directive) ())

(defmethod specialize-directive
    (client (char (eql #\b)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive 'b-elisp-directive))

(defmethod interpret-item (client (directive b-elisp-directive) &optional args)
  (declare (ignore client args))
  (change-class directive 'elisp-radix-directive :radix 2 :prefix "")
  (print-arg directive *destination*))

(defclass d-elisp-directive (elisp-directive) ())

(defmethod specialize-directive
    (client (char (eql #\d)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive 'd-elisp-directive))

(defmethod interpret-item (client (directive d-elisp-directive) &optional args)
  (declare (ignore client args))
  (change-class directive 'elisp-radix-directive :prefix "" :radix 10)
  (print-arg directive *destination*))

(defclass o-elisp-directive (elisp-directive) ())

(defmethod specialize-directive
    (client (char (eql #\o)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive 'o-elisp-directive))

(defmethod interpret-item (client (directive o-elisp-directive) &optional args)
  (declare (ignore client args))
  (change-class
   directive 'elisp-radix-directive
   :radix 8
   :prefix (if (and (argument-prefix directive) (/= (directive-argument directive) 0))
            "0" ""))
  (print-arg directive *destination*))

(defclass |x-elisp-directive| (elisp-directive) ())

(defmethod specialize-directive
    (client (char (eql #\x)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive '|x-elisp-directive|))

(defmethod interpret-item (client (directive |x-elisp-directive|) &optional args)
  (declare (ignore client args))
  (change-class
   directive 'elisp-radix-directive
   :radix 16
   :case :lower
   :prefix (if (and (argument-prefix directive) (/= (directive-argument directive) 0))
            "0x" ""))
  (print-arg directive *destination*))

(defclass |X-elisp-directive| (elisp-directive) ())

(defmethod specialize-directive
    (client (char (eql #\X)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive '|X-elisp-directive|))

(defmethod interpret-item (client (directive |X-elisp-directive|) &optional args)
  (declare (ignore client args))
  (change-class
   directive 'elisp-radix-directive
   :radix 16
   :case :upper
   :prefix (if (and (argument-prefix directive) (/= (directive-argument directive) 0))
            "0X" ""))
  ;;(cl:format t "r: ~a~%" (parameter-radix directive))
  (print-arg directive *destination*))
