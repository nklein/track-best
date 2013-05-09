;;;; insert.lisp

(in-package #:track-best)

(defun insert-tracked-item (tracker item &optional always)
  "Insert the ITEM into the TRACKER.  If ALWAYS is true, then do it even though it is not bigger than the last thing in the array."
  (with-slots (best order-by-fn) tracker
    (let ((size  (array-total-size best))
          (score (best-item-score item)))
      (labels ((ti> (n)
                 "Returns whether tracked-item is bigger than (aref best n)"
                 (funcall order-by-fn
                          score
                          (best-item-score (aref best n))))

               (make-room (at)
                 "Moves everything from AT to then end of BEST down one."
                 (loop :for index :from (1- size) :above at
                    :do (setf (aref best index)
                              (aref best (1- index))))
                 at)

               (insert-tracked-item (low high)
                 "Binary search between LOW and HIGH for a place for the
                TRACKED-ITEM.  Insert it once a place is found."
                 (let ((mid (truncate (/ (+ low high) 2))))
                   (cond
                     ((= low high) (setf (aref best (make-room low)) item))

                     ((ti> mid) (insert-tracked-item low mid))

                     (t  (insert-tracked-item (1+ mid) high))))))

        (when (or always
                  (ti> (1- size)))
          (insert-tracked-item 0 (1- size)))))))
