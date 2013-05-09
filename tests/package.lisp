;;;; package.lisp

(defpackage #:track-best-tests
  (:use #:cl #:track-best)
  (:export #:run-tests))

(in-package :track-best-tests)

(defun run-tests ()
  (let ((*print-pretty* t))
    (nst:nst-cmd :run-package :track-best-tests)))
