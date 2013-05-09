;;;; track-best.asd

(asdf:defsystem #:track-best
  :description "Macros/functions for tracking the best items.  See the README.md for more details."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20130509"
  :license "Free"
  :components ((:static-file "README.md")
               (:module "src"
                :serial t
                :components ((:file "package")
                             (:file "types")
                             (:file "globals")
                             (:file "insert")
                             (:file "methods")
                             (:file "track-best")))))
