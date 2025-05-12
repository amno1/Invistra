(cl:in-package #:asdf-user)

(defsystem "invistra-emacs-lisp-extrinsic"
  :description "System for loading Invistra extrinsically into an implementation."
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on ("invistra"
               "invistra-emacs-lisp"
               "inravina-extrinsic")
  :in-order-to ((asdf:test-op (asdf:test-op "invistra-extrinsic/test")))
  :components ((:module code
                :pathname "code/emacs-lisp-extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))

(defsystem :invistra-emacs-lisp-extrinsic-test 
  :description "Tests for Emacs Lisp format function" 
  :author "Arthur Miller <arthur.miller@live.com>" 
  :licence "GPLv3" 
  :version "0.0.1"
  :depends-on ("invistra-emacs-lisp-extrinsic" "parachute")
  :components ((:module "code/emacs-lisp-extrinsic/test"
                :serial t
                :components
                ((:file "packages")
                 (:file "tools")
                 (:file "tests")))))
