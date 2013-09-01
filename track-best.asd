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

(asdf:defsystem #:track-best-tests
  :description "Tests for the track-best library."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20130509"
  :license "Free"
  :depends-on (#:track-best #:nst)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "basics" :depends-on ("package"))
                             (:file "examples" :depends-on ("package"))))))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :track-best))))
  (asdf:load-system :track-best-tests)
  (funcall (find-symbol (symbol-name :run-tests) :track-best-tests)))
