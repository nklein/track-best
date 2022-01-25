;;;; track-best.asd

(asdf:defsystem #:track-best
  :description "Macros/functions for tracking the best items.  See the README.md for more details."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20220124"
  :license "Free"
  :in-order-to ((asdf:test-op (asdf:test-op :track-best/tests)))
  :components ((:static-file "README.md")
               (:module "src"
                :serial t
                :components ((:file "package")
                             (:file "types")
                             (:file "globals")
                             (:file "insert")
                             (:file "methods")
                             (:file "track-best")))))

(asdf:defsystem #:track-best/tests
  :description "Tests for the track-best library."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20220124"
  :license "Free"
  :depends-on ((:version #:track-best "0.1.20220124") #:nst)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "basics" :depends-on ("package"))
                             (:file "examples" :depends-on ("package")))))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :track-best/tests :run-tests)))
