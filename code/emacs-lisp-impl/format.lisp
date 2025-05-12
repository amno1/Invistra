(in-package #:invistra)

(defvar *destination*)

(defun format (client control &rest args)
  (with-output-to-string (stream)
    (let ((parser *format-parser*)
          (*destination* stream))
      (dolist (item (split-control-string control parser args))
        (interpret-item client
                        (specialize-directive client
                                              (directive-character item)
                                              item nil) stream)))))
