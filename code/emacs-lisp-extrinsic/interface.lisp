(in-package #:invistra-emacs-lisp-extrinsic)

(defclass elisp-parser (invistra:parser) ())

(invistra:define-interface (incless-extrinsic:*client*
                            incless-extrinsic:extrinsic-client)
  (let ((parser
          (make-instance
           'elisp-parser
           :base-directive 'elisp-directive
           :format-char #\%
           :operators (list #\$ #\.)
           :flags (list #\# #\+ #\- #\0 #\ )
           :terminals (list #\% #\b #\c #\d #\e #\f #\g #\o #\s #\S #\x
                            #\X))))
    (setf invistra:*format-parser* parser)))

(initialize-invistra)
