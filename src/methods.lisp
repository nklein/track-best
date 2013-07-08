;;;; methods.lisp

(in-package #:track-best)

;;; track method
(defun track (item score &optional (tracker *current-best-tracker*))
  "Add the ITEM with SCORE to the TRACKER."
  (check-type tracker best-tracker)
  (insert-tracked-item tracker item score))

;;; map-best method
(defun map-best (fn &optional (tracker *current-best-tracker*))
  (check-type fn function)
  (check-type tracker best-tracker)
  (nreverse (loop :for best :in (best-tracker-best tracker)
               :nconcing (loop :with score = (best-item-list-score best)
                            :for item :in (best-item-list-items best)
                            :collecting (funcall fn item score)))))
