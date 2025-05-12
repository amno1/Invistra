(in-package #:invistra)

(defclass elisp-general-directive (elisp-directive)
  ((restrict-width
    :initarg :restrict-width :type boolean :initform nil :accessor restrict-width)))

(defclass |s-elisp-directive| (elisp-general-directive) ())

(defmethod specialize-directive
    (client (char (eql #\s)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive '|s-elisp-directive| :pretty-print t))

(defmethod interpret-item (client (directive |s-elisp-directive|) &optional args)
  (declare (ignore args))
  (let ((arg (directive-argument directive)))
    (typecase arg
      (character
       (setf (directive-argument directive) (char-code arg)))))
  (print-arg directive *destination*))

(defclass |S-elisp-directive| (elisp-general-directive) ())

(defmethod specialize-directive
    (client (char (eql #\S)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive '|S-elisp-directive|))

(defmethod interpret-item (client (directive |S-elisp-directive|) &optional args)
  (declare (ignore args))
  (let ((arg (directive-argument directive)))
    (when (characterp arg)
     (setf (directive-argument directive) (char-code arg))))
  (print-arg directive *destination*))

(defclass c-elisp-directive (elisp-general-directive) ())

(defmethod specialize-directive
    (client (char (eql #\c)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive 'c-elisp-directive :sign-char nil))

(defmethod interpret-item (client (directive c-elisp-directive) &optional args)
  (declare (ignore args))
  (let ((arg (directive-argument directive)))
    (when (integerp arg)
     (setf (directive-argument directive)           
           (code-char arg)))
    (setf (pretty-print directive) t))
  (print-arg directive *destination*))

;;; basic-output.lisp ends here
