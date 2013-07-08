;;;; types.lisp

(in-package #:track-best)

;;; Struct used to track a set of tied items
(defstruct best-item-list
  (size    1 :type (integer 1 *))
  (score 0.0 :read-only t)
  (items nil))

;;; deftype for order-by-fn functions
(deftype order-by-fn ()
  '(function (t t) boolean))

;;; Struct used to track the best items collected.
(defstruct best-tracker
  (best nil :type list)                 ; best-item-list elements (worst->best)
  (size 0 :type (integer 0 *))
  (keep 1 :read-only t)
  (keep-ties nil :read-only t)
  (order-by-fn #'> :type order-by-fn :read-only t))
