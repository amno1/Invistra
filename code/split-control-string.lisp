(cl:in-package #:invistra)

(declaim (inline directive-string-p))
(defun directive-string-p (string parser)
  (char= (aref string 0) (format-char parser)))

(defun make-directive (string parser)
  "Split a directive STRING into its tokens.

It tokenizes STRING into literals, operators and flags as defined for PARSER."
  (unless (directive-string-p string parser)
    (return-from make-directive (new 'literal-directive :argument string)))
  (when (eql (array-last string) (format-char parser))
    (return-from make-directive (new 'literal-directive
                                     :argument (string (format-char parser)))))
  (loop for position from 1 to (1- (length string))
        with start
        with tokens
        with directive
        do
           (multiple-value-bind (param pos) (make-parameter string position)
             (push param tokens)
             (setf position pos start pos)
             (when (integerp param) (decf position)))
        finally
           (when (> (1- position) start)
             (push (subseq string start position) tokens))
           (setf directive (parse-directive (nreverse tokens) string parser))
           (return directive)))

;;; Split a control string into its components.  Each component is
;;; either a string to be printed as it is, or a directive.  The list
;;; of components will never contain two consecutive strings.
(defun split-control-string (string parser args)
  "Split entire control string of a format directive.

It splits STRING into literal strings to be printed as they are, and directive
  strings according to direcives for PARSER which has to be parsed further."
  (loop for ch across string
        with arg-id = 0
        with position = 0
        with last-end = 0
        with strings = nil
        with directives = nil
        with directive-start = nil
        with format-char = (format-char parser)
        do           
           (unless (case-sensitive-p parser)
             (setf ch (char-upcase ch)))
           (cond
             ((and (null directive-start) (eql ch format-char))
              (setf directive-start position)
              (when (> directive-start last-end)
                (push (subseq string last-end directive-start) strings)))
             ((and directive-start (member ch (parser-terminals parser)))
              (push (subseq string directive-start (1+ position)) strings)
              (setf directive-start nil last-end (1+ position))))
           (incf position)
        finally
           (when (> position last-end)
             (push (subseq string last-end position) strings))
           (setf strings (nreverse strings))
           (dolist (string strings)
             (let ((directive (make-directive string parser)))
               (when (consume-argument-p directive)
                 (setf (directive-argument directive)
                       (if (directive-argument directive)
                           (nth (1- (directive-argument directive)) args)
                           (nth arg-id args))
                       arg-id (1+ arg-id)))
               (push directive directives)))
           (return (nreverse directives))))
