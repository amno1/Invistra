(cl:in-package #:asdf-user)

(defsystem "invistra-emacs-lisp"
  :description "A portable and extensible Common Lisp FORMAT implementation"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on ("acclimation"
               "incless"
               "invistra"
               "quaviver/native"
               (:feature (:not :sicl) "inravina")
               "nontrivial-gray-streams")
  :components ((:module "code/emacs-lisp-impl"
                :serial t
                :components ((:file "directive")
                             (:file "format")
                             (:file "formatter")
                             (:file "radix-control")
                             (:file "floating-point-printers")
                             (:file "basic-output")
                             (:file "interface")))))
