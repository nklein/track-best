;;;; methods.lisp

(in-package #:track-best)

;;; track method
(defun track (item score &optional (tracker *current-best-tracker*))
  "Add the ITEM with SCORE to the TRACKER."
  (check-type tracker best-tracker)
  (with-slots (best keep) tracker
    (let ((cur-size (array-total-size best))
          (tracked-item (make-best-item :score score
                                        :item item)))
      (cond
        ((< cur-size keep)
         (adjust-array best (1+ cur-size) :initial-element tracked-item)
         (insert-tracked-item tracker tracked-item t))

        (t (insert-tracked-item tracker tracked-item))))))

;;; map-best method
(defun map-best (fn &optional (tracker *current-best-tracker*))
  (check-type fn function)
  (check-type tracker best-tracker)
  (map 'list #'(lambda (bi)
                 (funcall fn (best-item-item bi) (best-item-score bi)))
       (best-tracker-best tracker)))
