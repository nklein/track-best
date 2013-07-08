;;;; insert.lisp

(in-package #:track-best)

(defun insert-tracked-item (tracker item score)
  "Insert the ITEM with the given SCORE into the TRACKER."
  (with-slots (best size keep keep-ties order-by-fn) tracker
    (labels ((t< (score best)
               (funcall order-by-fn (best-item-list-score best) score))
             (t> (score best)
               (funcall order-by-fn score (best-item-list-score best)))

             (insertable? ()
               (or (< size keep)
                   (t> score (first best))
                   (and keep-ties (not (t< score (first best))))))

             (new-best-item-list ()
               (make-best-item-list :size 1
                                    :score score
                                    :items (list item)))

             (insert-tracked-item (best-list)
               (cond
                 ((null best-list)
                  (incf size)
                  (list (new-best-item-list)))
                 (t (destructuring-bind (best . rest) best-list
                      (cond
                        ((t< score best)
                         (incf size)
                         (list* (new-best-item-list) best-list))
                        ((t> score best)
                         (let ((tail (insert-tracked-item rest)))
                           (if (eq tail rest)
                               best-list
                               (list* best tail))))
                        (t (incf size)
                           (push item (best-item-list-items best))
                           (incf (best-item-list-size best))
                           best-list))))))

             (trim-excess (best-list)
               (destructuring-bind (best . rest) best-list
                 (let ((cur-size (best-item-list-size best)))
                   (cond
                     ((and keep-ties (<= keep (- size cur-size)))
                      (decf size cur-size)
                      rest)
                     ((and (not keep-ties) (< keep size))
                      (decf size)
                      (cond
                        ((< 1 cur-size)
                         (decf (best-item-list-size best))
                         (setf (best-item-list-items best)
                               (rest (best-item-list-items best)))
                         best-list)
                        (t rest)))
                     (t best-list))))))

      (when (insertable?)
        (setf best (trim-excess (insert-tracked-item best))))))
  tracker)
