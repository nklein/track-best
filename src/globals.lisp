;;;; globals.lisp

(in-package #:track-best)

;;; Global used to hold the default best-tracker.  This will be
;;; bound with dynamic-extent in various places so that most of
;;; the functions can be invoked without having to pass in an
;;; explicit tracker.
(defparameter *current-best-tracker* nil
  "The current best-tracker instance.")
