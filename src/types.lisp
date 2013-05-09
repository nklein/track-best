;;;; types.lisp

(in-package #:track-best)

;;; Struct used to track a single, collected item.
(defstruct best-item
  (score 0.0 :read-only t)
  (item nil :read-only t))

;;; deftype for array of best-item instances
(deftype best-item-array (&optional (length '*))
  "Type for an array of BEST-ITEM instances."
  `(array best-item (,length)))

;;; deftype for order-by-fn functions
(deftype order-by-fn ()
  '(function (t t) boolean))

;;; Struct used to track the best items collected.
(defstruct best-tracker
  (best (make-array 0 :element-type 'best-item
                      :adjustable t)
        :type best-item-array)
  (keep 1 :read-only t)
  (order-by-fn #'> :type order-by-fn :read-only t))
