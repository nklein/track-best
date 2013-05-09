;;;; track-best.asd

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
