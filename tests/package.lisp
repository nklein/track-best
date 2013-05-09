;;;; package.lisp

(defpackage #:track-best-tests
  (:use #:cl #:track-best)
  (:export #:run-tests))

(in-package :track-best-tests)

(defun run-tests ()
  (nst:nst-cmd :run-package :track-best-tests))
