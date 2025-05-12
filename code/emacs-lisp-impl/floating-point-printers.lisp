;;; Floating-point printers

(in-package #:invistra)

(declaim
 (sb-ext:disable-package-locks *read-default-float-format*))

(setf *read-default-float-format* 'double-float)

(defclass elisp-float-directive (elisp-directive)
  ((exponent
    :initarg :exponent :type (or null integer) :initform 0 :accessor
    argument-exponent)
   (e
    :initarg :e :type (or null integer) :initform 0 :accessor directive-e)
   (k
    :initarg :k :type (or null integer) :initform 0 :accessor directive-k)
   (significand
    :initarg :significand :type (or null integer) :accessor argument-significand)
   (digits
    :initarg :digits :type (or null integer) :accessor argument-digits)
   (exponentchar
    :initarg :exponent-char :type (or null character) :initform nil :accessor exponent-char)
   (overflowchar
    :initarg :overflow-char :type (or null character) :initform nil :accessor overflow-char)))

(defun print-float-arg (directive printer)
;;  (cl:format t "pfa: precision (d): ~a ~%" (argument-precision directive))
  (let ((value (directive-argument directive)))
    (if (or (complexp value)
            (and (floatp value)
                 #+abcl
                 (or (system:float-infinity-p value)
                     (system:float-nan-p value))
                 #+allegro
                 (or (excl:infinityp value)
                     (excl:nanp value))
                 #+ccl
                 (ccl::nan-or-infinity-p value)
                 #+(or clasp cmucl ecl)
                 (or (ext:float-infinity-p value)
                     (ext:float-nan-p value))
                 #+mezzano
                 (or (mezzano.extensions:float-infinity-p value)
                     (mezzano.extensions:float-nan-p value))
                 #+sbcl (or (sb-ext:float-infinity-p value)
                            (sb-ext:float-nan-p value)))
            (not (numberp value)))
        (let ((*print-base* 10)
              (*print-escape* nil)
              (*print-readably* nil))
          (write value :stream *destination*))
        (let ((coerced-value (if (floatp value)
                                 value
                                 (coerce value 'double-float)))
              (client (directive-client directive)))
          (multiple-value-bind (significand exponent sign)
              (quaviver:float-triple client 10 coerced-value)
            (setf (slot-value directive 'sign-char)
                  (cond ((minusp sign) #\-)
                        ((print-sign-p directive) #\+)))
            (funcall printer
                     coerced-value
                     significand
                     exponent
                     (argument-width directive)
                     (argument-precision directive)
                     (directive-k directive)
                     (directive-k directive)
                     (overflow-char directive)
                     (exponent-char directive)))))))

(defun round-away-from-zero (x n)
  (multiple-value-bind (q r)
      (truncate x n)
    (if (>= (* 2 r) n)
        (1+ q)
        q)))

(defun trim-fractional (significand digit-count fractional-position d)
  (declare (type fixnum significand))
  (let ((l (max 0 (- digit-count fractional-position))))
    (cond ((< l d)
           (if (zerop significand)
               (setf fractional-position (- d))
               (setf significand
                     (* significand
                        (expt 10
                              (+ (max 0
                                      (- fractional-position digit-count))
                                 (- d l))))
                     digit-count (quaviver.math:count-digits 10 significand))))
          ((> l d)
           (when (minusp fractional-position)
             (setf fractional-position
                   (max fractional-position (- 1 d))))
           (setf significand (round-away-from-zero significand
                                                   (expt 10 (- l d)))
                 digit-count (quaviver.math:count-digits 10 significand)))))
  (values significand digit-count fractional-position))


;; %f Fixed-format floating point.

(defclass f-elisp-directive (elisp-float-directive) ())

(defmethod argument-to-string ((directive f-elisp-directive))
  (with-output-to-string (stream)
    (let ((*destination* stream))
      (print-float-arg directive #'print-fixed-arg))))

(defun print-fixed-arg (value significand exponent w d k e overflowchar exponentchar)
  (declare (ignore e exponentchar)
           (type fixnum significand))
  (let* ((digit-count (quaviver.math:count-digits 10 significand))
         (fractional-position (if (zerop significand)
                                  0
                                  (+ digit-count k exponent)))
         (leading-zeros 0)
         (my-significand significand))
    (declare (type fixnum my-significand))
    (flet ((compute-width ()
             (+ 1
                leading-zeros
                (max digit-count fractional-position)
                (- (min 0 fractional-position)))))
      (when (and w
                 (null d)
                 (> (compute-width) w))
        (multiple-value-setq (my-significand digit-count fractional-position)
          (trim-fractional my-significand digit-count fractional-position
                           (min (max 0 (- digit-count fractional-position))
                                (max 0
                                     (- w
                                        (max 0 fractional-position)
                                        1)))))
        (when (zerop my-significand)
          (setf fractional-position 1)))
      (when d
        (multiple-value-setq (my-significand digit-count fractional-position)
          (trim-fractional my-significand digit-count fractional-position d)))
      (when (and (>= fractional-position digit-count)
                 (null d)
                 (or (null w)
                     (null overflowchar)
                     (< (compute-width) w)))
        (if (zerop my-significand)
            (decf fractional-position)
            (setf my-significand (* my-significand
                                    (expt 10
                                          (+ fractional-position
                                             (- digit-count)
                                             1)))
                  digit-count (quaviver.math:count-digits 10 my-significand))))
      (when (and (not (plusp fractional-position))
                 (< value (expt 10 (- k)))
                 (or (null w) (null d)
                     (> w (1+ d)))
                 (or (null w)
                     (< (compute-width) w)))
        (setf leading-zeros 1))
      (cond ((or (null w)
                 (null overflowchar)
                 (<= (compute-width) w))
             (cond ((< fractional-position 0)
                    (setf fractional-position (1+ fractional-position)
                          leading-zeros 1))
                   ((= fractional-position 0)
                    (setf leading-zeros 1)))
             (cond ((= d 0)
                    (quaviver:write-digits 10 my-significand *destination*
                                           :leading-zeros leading-zeros))
                   (t
                    (quaviver:write-digits 10 my-significand *destination*
                                           :leading-zeros leading-zeros
                                           :fractional-position fractional-position
                                           :fractional-marker #\.)))             
             nil)
            (t
             (loop repeat w
                   do (write-char overflowchar *destination*))
             t)))))

(defmethod specialize-directive
    (client (char (eql #\f)) directive end-directive)
  (declare (ignore end-directive))
  (change-class
   directive 'f-elisp-directive :client client :k 0
   :overflow-char (when (pad-right-p directive) (argument-padchar directive))))

(defmethod interpret-item (client (directive f-elisp-directive) &optional parameters)
  (declare (ignore parameters))
  (print-arg directive *destination*))

;; (defmethod compile-item (client (directive f-directive) &optional parameters)
;;   `((print-float-arg ,(incless:client-form client)
;;                      (lambda (client value digits exponent sign)
;;                        (print-fixed-arg client value digits exponent sign
;;                                         ,(colon-p directive) ,(at-sign-p directive)
;;                                         ,@parameters)))))


;; %e Exponential floating point.

(defclass e-elisp-directive (elisp-float-directive) nil)

(defmethod specialize-directive
    (client (char (eql #\e)) directive (end-directive t))
  (change-class
   directive 'e-elisp-directive
   :client client :e 2 :k 1 :exponent-char #\e))

(defmethod argument-to-string ((directive e-elisp-directive))
  (with-output-to-string (s)
    (let ((*destination* s))
      (print-float-arg directive #'print-exponent-arg))))

(defun print-exponent-arg (value significand exponent w d e k overflowchar exponentchar)
  (declare (type fixnum significand))
  (let* ((digit-count (quaviver.math:count-digits 10 significand))
         (fractional-position k)
         (leading-zeros 0)
         (my-significand significand)
         (my-exponent (if (zerop significand)
                          0
                          (+ exponent digit-count (- k)))))
    (declare (type fixnum my-exponent))
    (let* ((exp-count (quaviver.math:count-digits 10 (abs my-exponent)))
           (leading-exp-zeros (1+ (- (or e exp-count) exp-count))))
      (flet ((compute-width ()
               (+ 3
                  leading-zeros
                  (max digit-count fractional-position)
                  (- (min 0 fractional-position))
                  leading-exp-zeros
                  exp-count)))
        (when d
          (multiple-value-setq (my-significand digit-count fractional-position)
            (trim-fractional my-significand digit-count fractional-position
                             (cond ((zerop k)
                                    d)
                                   ((plusp k)
                                    (- d k -1))
                                   (t
                                    (+ d k 1))))))
        (when (and w
                   (null d)
                   (> (compute-width) w))
          (multiple-value-setq (my-significand digit-count fractional-position)
            (trim-fractional my-significand digit-count fractional-position
                             (max 0
                                  (- w
                                     (max 0 fractional-position)
                                     3
                                     exp-count)))))
        (when (and (= fractional-position digit-count)
                   (null d)
                   (or (null w)
                       (< (compute-width) w)
                       #+(or)(null d)
                       #+(or)(> w (1+ d))))
          (if (zerop significand)
              (setf fractional-position 1)
              (setf my-significand (* 10 my-significand)
                    digit-count (1+ digit-count))))
        (when (or (zerop significand)
                  (and (not (plusp fractional-position))
                       (or (null w)
                           (< (compute-width) w))))
          (setf leading-zeros 1
                fractional-position (1+ fractional-position)))
        ;;(cl:format t "fp: ~a lz: ~a~%" fractional-position leading-zeros)
        (cond ((or (null w)
                   (null overflowchar)
                   (<= (compute-width) w))
               (quaviver:write-digits 10 my-significand *destination*
                                      :leading-zeros leading-zeros
                                      :fractional-position fractional-position
                                      :fractional-marker #\.)
               (write-char (or exponentchar
                               (if (typep value *read-default-float-format*)
                                   #+abcl #\E #-abcl #\e
                                   (etypecase value
                                     (short-float #+abcl #\S #-abcl #\s)
                                     (single-float #+abcl #\F #-abcl #\f)
                                     (double-float #+abcl #\D #-abcl #\d)
                                     (long-float #+abcl #\L #-abcl #\l))))
                           *destination*)
               (write-char (if (minusp my-exponent) #\- #\+) *destination*)
               (quaviver:write-digits 10 (abs my-exponent) *destination*
                                      :leading-zeros leading-exp-zeros))
              (t
               (loop repeat w
                     do (write-char overflowchar *destination*))))))))

(defmethod interpret-item (client (directive e-elisp-directive) &optional parameters)
  (declare (ignore parameters))
  (print-arg directive *destination*))

;; (defmethod compile-item (client (directive e-directive) &optional parameters)
;;   `((print-float-arg ,(incless:client-form client)
;;                      (lambda (client value digits exponent sign)
;;                        (print-exponent-arg client value digits exponent sign
;;                                            ,(colon-p directive) ,(at-sign-p directive)
;;                                            ,@parameters)))))


;; %g General floating point.

(defclass g-elisp-directive (elisp-float-directive) ())

(defmethod specialize-directive
    ((client t) (char (eql #\g)) directive (end-directive t))
  (let ((value (directive-argument directive))
        (directive (change-class directive 'elisp-float-directive :client client))) 
    (typecase value
      (integer
       (change-class directive 'd-elisp-directive :precision 0))
      (float
       (multiple-value-bind (significand exponent sign)
           (quaviver:float-triple client 10 value)
         (declare (type fixnum significand))
         
         (let*  ((d (argument-precision directive))
                 (digit-count (quaviver.math:count-digits 10 significand)))
           ;; (cl:format t "rv: ~a t: ~a s ~a e ~a sc ~a sign ~a ~%"
           ;;            (car rounded) (type-of significand) significand exponent
           ;;            significand-count sign)
           (setf (slot-value directive 'sign-char)
                 (cond ((minusp sign) #\-)
                       ((print-sign-p directive) #\+)))
           (unless d
             (let ((q (if (minusp exponent)
                          (- digit-count exponent)
                          (max digit-count exponent))))
               (setq d (max q (min exponent 7)))))
           
           (cond
             ((= d 1)
              (change-class directive 'e-elisp-directive :k 1 :precision 0)
              (print-arg directive *destination*))
             ((or (< exponent -4) (>= exponent d))
              (change-class directive 'e-elisp-directive :k 1 :precision 5)
              (print-arg directive *destination*))
             (t
              (change-class directive 'f-elisp-directive :precision (abs exponent))
              (print-arg directive *destination*)))))))))

;; (defmethod compile-item (client (directive g-directive) &optional parameters)
;;   `((print-float-arg ,(incless:client-form client)
;;                      (lambda (client value significand exponent sign)
;;                        (print-general-arg client value significand exponent sign
;;                                           ,(colon-p directive) ,(at-sign-p directive)
;;                                           ,@parameters)))))
