(cl:in-package #:asdf-user)

(defsystem "invistra-elisp-extrinsic"
  :description "System for loading Invistra Elisp extrinsically into an implementation."
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :depends-on ("invistra"
               "inravina-extrinsic")
  :components ((:module code
                :pathname "code/elisp-extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
