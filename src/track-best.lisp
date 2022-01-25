;;;; track-best.lisp

(in-package #:track-best)

(defmacro with-track-best ((&key (name nil)
                                 (keep 1)
                                 (keep-ties nil)
                                 (order-by-fn '#'>)
                                 (always-return-list nil)
                                 (return-best t))
                           &body body)
  (let ((items (gensym "ITEMS-"))
        (scores (gensym "SCORES-")))
    `(let* ((*current-best-tracker* (make-best-tracker
                                     :keep ,keep
                                     :keep-ties ,keep-ties
                                     :order-by-fn ,order-by-fn))
            ,@(when name
                `((,name *current-best-tracker*))))
       ,@body
       ,@(when return-best
           `((let ((,items (map-best #'(lambda (i s)
                                         (declare (ignore s))
                                         i)))
                   (,scores (map-best #'(lambda (i s)
                                          (declare (ignore i))
                                          s))))
               (cond
                 ;; If there were no items, return that
                 ((null ,items)
                  (values nil nil))

                 ;; If we only ended up with one item and we don't
                 ;; always need to return a list, just return the
                 ;; first item and score.
                 ((and (not ,always-return-list)
                       (null (rest ,items)))
                  (values (first ,items) (first ,scores)))

                 ;; Otherwise, return all of the items and scores.
                 (t (values ,items ,scores)))))))))
