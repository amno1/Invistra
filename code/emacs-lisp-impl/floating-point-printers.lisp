;;; Floating-point printers

(in-package #:invistra)

(declaim (inline ensure-float))
(defun ensure-float (value)
  (coerce value 'double-float))

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
        (let ((client (directive-client directive))
              (value (ensure-float value)))
          (multiple-value-bind (significand exponent sign)
              (quaviver:float-triple client 10 value)
            (setf (slot-value directive 'sign-char)
                  (cond ((minusp sign) #\-)
                        ((print-sign-p directive) #\+)))
            (funcall printer
                     value
                     significand
                     exponent
                     (argument-width directive)
                     (argument-precision directive)
                     (directive-k directive)
                     (directive-k directive)
                     (overflow-char directive)
                     (exponent-char directive)
                     (argument-prefix directive)))))))

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

(defun print-fixed-arg (value significand exponent
                        w d k e overflowchar exponentchar trailing-dot)
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
                                           ;:leading-zeros leading-zeros
                                           ))
                   (t
                    (quaviver:write-digits 10 my-significand *destination*
                                           :leading-zeros leading-zeros
                                           :fractional-position fractional-position
                                           :fractional-marker #\.)))             
             nil)
            (t
             (loop repeat w
                   do (write-char overflowchar *destination*))
             t))
      (when (and trailing-dot (>= fractional-position 0 ))
        (write-char #\. *destination*)))))

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

(defun print-exponent-arg (value significand exponent
                           w d e k overflowchar exponentchar trailing-dot)
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
      (when (= leading-exp-zeros 0) (incf leading-exp-zeros))
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
        (cond ((or (null w)
                   (null overflowchar)
                   (<= (compute-width) w))
               (quaviver:write-digits 10 my-significand *destination*
                                      :leading-zeros leading-zeros
                                      :fractional-position fractional-position
                                      :fractional-marker
                                      (when (or (> d 1) trailing-dot) #\.))
               (write-char (or exponentchar
                               (if (typep value *read-default-float-format*)
                                   #+abcl #\E #-abcl #\e
                                   (etypecase value
                                     (short-float #+abcl #\S #-abcl #\s)
                                     #-sbcl
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

(defclass quaviver-client (quaviver/native:client) ())
(defclass g-elisp-directive (elisp-float-directive) ())

(defvar *quaviver-native-client* (new 'quaviver-client))

(defgeneric digit-count (client value precision)
  (:method (client (value integer) precision)
    (declare (type fixnum value)
             (ignore client))
    (values value value
            (quaviver.math:count-digits 10 value)
            0 precision (if (minusp value) -1 1)))
  (:method (client (value float) precision)
    (declare (type double-float value)
             (ignore precision))
    (multiple-value-bind (significand exponent sign)
        (quaviver:float-triple client 10 value)
      (declare (type fixnum significand))
      (values value significand
              (quaviver.math:count-digits 10 significand)
              0 exponent sign)))
  (:documentation
   "Return number of digits for a VALUE in base 10."))

(defun limit-significand-digits (limit significand digit-count exponent sign)
  (declare (type fixnum significand digit-count exponent sign))
  (let ((client *quaviver-native-client*))
    (multiple-value-bind (s dc fp)
        (trim-fractional significand digit-count 0 (min digit-count limit))
      (declare (type fixnum s dc fp)
               (ignore dc fp))
      (values
       (quaviver:triple-float client 'double-float 10 s exponent sign)
       exponent))))

(defun specialize-elisp-g (client directive value precision exponent decimal-places)
  (let ((k 0))
    (when (eq precision 'shift)
      (setf k 0 precision 1 exponent 0 decimal-places 0))
    (cond
      ((>= exponent precision)
       (cl:format t "e1: ~a ~a~%" precision exponent)
       (change-class directive 'e-elisp-directive
                     :client client
                     :argument value
                     :precision decimal-places
                     :k k))
      ((< exponent -4)
       (cl:format t "e2: ~a ~a~%" precision exponent)
       (change-class directive 'e-elisp-directive
                     :client client
                     :argument value
                     :precision decimal-places
                     :k k))
      (t
       (cl:format t "f: ~a ~a ~a~%" precision exponent decimal-places)
       (change-class directive 'f-elisp-directive
                     :client client
                     :argument value
                     :precision decimal-places
                     :k k)))))

(defmethod specialize-directive
    ((client t) (char (eql #\g)) directive (end-directive t))
  (let ((precision (argument-precision directive))
        (value (directive-argument directive)))
    (declare (type (or symbol fixnum) precision))
    (when (zerop precision)
      (setf precision 1))
    (multiple-value-bind (value significand digit-count prec exponent sign)
        (digit-count *quaviver-native-client* value precision)
      (declare (type fixnum significand digit-count exponent prec sign)
               (ignore prec))
      (when (and (integerp value) (<= digit-count precision))
        (return-from specialize-directive
          (change-class directive 'd-elisp-directive :precision 0)))
      
      (let* ((decimal-places (if (integerp value) 0 (abs exponent)))
             (significant-places (- digit-count decimal-places))
             k)
        (declare (ignore k))
        (cl:format t "B: ~a ~a ~a ~a ~a~%"
                   digit-count precision exponent significant-places decimal-places)
        (when (> significant-places precision)
          (cl:format t ">:~a ~a ~a ~a~%"
                     digit-count significant-places decimal-places exponent)
          (setf 
                value (limit-significand-digits precision significand
                                                significant-places exponent sign)
                exponent (+ exponent significant-places)
                ))
        (cl:format t "A: ~a ~a ~a ~a ~a~%"
                   digit-count precision exponent significant-places decimal-places)
        (cond
          ((>= significant-places precision)
           (cl:format t "1: ~a ~a ~a ~a ~a~%"
                      digit-count precision exponent significant-places decimal-places)
           (setf exponent 0 decimal-places 0))
          ((and (<= digit-count precision (abs exponent)) (<= (abs exponent) precision ))
           (cl:format t "2: ~a ~a ~a ~a ~a~%"
                      digit-count precision exponent significant-places decimal-places)
           (setf exponent 0))
          ((and (> decimal-places (abs exponent) precision 0))
           (cl:format t "3: ~a ~a ~a ~a ~a~%"
                      digit-count precision exponent significant-places
                      decimal-places)
           ;; missusing precision and exponent
           (setf exponent -1 decimal-places 1  precision 'shift))
          ((and (> decimal-places precision) (= (abs exponent) digit-count))
           (cl:format t "3A: ~a ~a ~a ~a ~a~%"
                      digit-count precision exponent significant-places decimal-places)
           (setf exponent 0 decimal-places 0))
          ((and (> decimal-places precision) (= (abs exponent) digit-count))
           (cl:format t "4: ~a ~a ~a ~a ~a~%"
                      digit-count precision exponent significant-places decimal-places)
           (setf decimal-places
                 (min (- precision significant-places)
                      (1- precision)
                      (- digit-count significant-places))
                 exponent (if (> significant-places 0)
                              0
                              exponent)))
          ((and (> significant-places 0) (> decimal-places precision))
           (cl:format t "5: ~a ~a ~a ~a ~a~%"
                      digit-count precision exponent significant-places decimal-places)
           (setf exponent 0 decimal-places (- precision significant-places)))
          ((and (> significant-places 0) (= decimal-places precision))
           (cl:format t "6: ~a ~a ~a ~a ~a~%"
                      digit-count precision exponent significant-places decimal-places)
           (setf exponent 0 decimal-places (- precision significant-places)))
          (t
           (cl:format t "T: ~a ~a ~a ~a ~a~%"
                      digit-count precision exponent significant-places decimal-places)
           ))
        
        (cl:format t "L: ~a ~a ~a ~a ~a~%"
                   digit-count precision exponent significant-places decimal-places)
        (specialize-elisp-g
         client directive value precision exponent decimal-places)))))

;; (cond
;;   ((>= exponent precision)
;;    (setf k 0 pp 0))
;;   ((<= decimal-places precision)
;;    (setf k 1 pp p))
;;   (t
;;    (setf k 1 pp precision)))

;; (multiple-value-bind (value significand digit-count prec exponent sign)
;;         (digit-count *quaviver-native-client* value precision)
;;       (setf exponent (+ significant-places)
;;             precision decimal-places))

;; (defmethod specialize-directive
;;     ((client t) (char (eql #\g)) directive (end-directive t))
;;   (let ((precision (argument-precision directive))
;;         (value (directive-argument directive)))
;;     (declare (type fixnum precision))
;;     (when (zerop precision)
;;       (setf precision 1))
;;     (multiple-value-bind (value significand digit-count prec exponent sign)
;;         (digit-count *quaviver-native-client* value precision)
;;       (declare (type fixnum significand digit-count exponent prec sign)
;;                (ignore prec))
;;       (when (and (integerp value) (<= digit-count precision))
;;         (return-from specialize-directive
;;           (change-class directive 'd-elisp-directive :precision 0)))
      
;;       (let* ((decimal-places (if (integerp value) 0 (abs exponent)))
;;              (significant-places (- digit-count decimal-places)))

;;       (when (> digit-count precision)
;;         (setf value (limit-significand-digits
;;                      precision significand
;;                      significant-places (- decimal-places)
;;                      sign)))
;;     (multiple-value-bind (value significand digit-count prec exponent sign)
;;         (digit-count *quaviver-native-client* value precision)
;;         (setf exponent (+ significant-places))
;;         (let ((p (1- precision))
;;               (dd (abs (- digit-count precision)))
;;               k pp)
;;           (cond
;;             ((>= exponent precision)
;;              (cl:format t "1: ~a ~a ~a ~a ~a ~a~%"
;;                         significant-places decimal-places digit-count exponent
;;                         precision dd)
;;              (cond
;;                ((>= dd precision)
;;                 (setf k 0 pp 0))
;;                ((<= decimal-places precision)
;;                 (setf k 1 pp p))
;;                (t
;;                 (setf k 1 pp precision)))
;;              (return-from specialize-directive               
;;                   (change-class directive 'e-elisp-directive
;;                                 :argument value
;;                                 :client client
;;                                 :precision pp
;;                                 :k k)))
;;             ((< exponent -4)
;;              (cl:format t "2: ~a ~a ~a ~a ~a~%"
;;                         significant-places decimal-places digit-count exponent precision)
;;              (return-from specialize-directive
;;                (change-class directive 'e-elisp-directive
;;                              :argument value
;;                              :client client
;;                              :precision (1- precision)
;;                              :k 0)))
;;             (t
;;              (cond
;;                ((<= digit-count decimal-places precision)
;;                   (cl:format t "first: ~a ~a ~a ~a ~a ~a~%"
;;                              significant-places decimal-places digit-count exponent
;;                              precision dd)
;;                 (return-from specialize-directive
;;                   (change-class directive 'f-elisp-directive
;;                                 :argument value
;;                                 :client client
;;                                 :precision decimal-places
;;                                 :k 0
;;                                 )))
;;                (t
;;                 (cl:format t "3: ~a ~a ~a ~a ~a ~a~%"
;;                            significant-places decimal-places digit-count exponent
;;                            precision dd)
;;                 (cond
;;                   ((< digit-count precision)
;;                    (setf k 0 pp (if (> decimal-places 0)
;;                                     decimal-places
;;                                     (abs (- precision significant-places)))))                    
;;                   (t
;;                    (setf k 0 pp (abs (- precision significant-places)))))
;;                 (return-from specialize-directive
;;                   (change-class directive 'f-elisp-directive
;;                                 :argument value
;;                                 :client client
;;                                 :precision pp
;;                                 :k k
;;                                 ))))))))))))

;; (cond
;;   ((= exponent precision significant-places)
   
;;    (return-from specialize-directive
;;      (change-class directive 'f-elisp-directive
;;                    :argument value
;;                    :client client
;;                    :precision 0
;;                    :k 0)))
;;   (t
;;    (return-from specialize-directive               
;;      (change-class directive 'e-elisp-directive
;;                    :argument value
;;                    :client client
;;                    :precision pp
;;                    :k k))))

;; (cond
;;   ((= significant-places (1- precision)) 0)
;;   ((< decimal-places precision)  decimal-places)
;;   ((= decimal-places precision) (1- precision))
;;   (t 0))
