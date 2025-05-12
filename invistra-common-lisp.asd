(cl:in-package #:asdf-user)

(defsystem :invistra-common-lisp
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
               (:feature (:not :sicl) "inravina")
               "nontrivial-gray-streams")
  :components ((:module code/common-lisp-impl
                :serial t
                :components ((:file "format")
                             (:file "formatter")
                             (:file "basic-output")
                             (:file "radix-control")
                             (:file "floating-point-printers")
                             (:file "printer-operations")
                             (:file "pretty-printer-operations")
                             (:file "layout-control")
                             (:file "parse-control-string")
                             (:file "control-flow-operations")
                             (:file "miscellaneous-operations")
                             (:file "miscellaneous-pseudo-operations")
                             (:file "condition-reporters-en")
                             (:file "interface")
                             ))))
