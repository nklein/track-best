;;; examples.lisp

(in-package :track-best-tests)

(nst:def-test-group basics ()
  (nst:def-test nominal-case (:values (:equal -10) (:equal 10))
    (with-track-best ()
      (dolist (v '(1 3 5 -10 -5 -4 6 7))
        (track v (abs v)))))

  (nst:def-test no-results-case (:values (:equal nil) (:equal nil))
    (with-track-best ()))

  (nst:def-test always-return-list (:values (:equalp '(-10)) (:equalp '(10)))
    (with-track-best (:always-return-list t)
      (dolist (v '(1 3 5 -10 -5 -4 6 7))
        (track v (abs v)))))

  (nst:def-test no-return-best (:equal :done)
    (with-track-best (:return-best nil)
      :done))

  (nst:def-test named-trackers (:values (:equal 42) (:equal 7))
    (with-track-best (:name outer)
      (let ((inner (with-track-best ()
                     (dolist (v '(1 3 5 -10 -5 -4 6 7))
                       (track v (abs v))
                       (track (- (* v v) v) v outer)))))
        (track inner (/ inner 2)))))

  (nst:def-test order-by-fn (:values  (:equal 1) (:equal 1))
    (with-track-best (:order-by-fn #'<)
      (dolist (v '(1 3 5 -10 -5 -4 6 7))
        (track v (abs v)))))

  (nst:def-test involved-order-by-fn (:values (:equal #C(1 1)) (:equal 2))
    (labels ((most-north-then-east (a b)
               (or (> (realpart a) (realpart b))
                   (and (= (realpart a) (realpart b))
                        (> (imagpart a) (imagpart b)))))
             (score (a)
               (+ (abs (realpart a)) (abs (imagpart a)))))
      (with-track-best (:order-by-fn #'most-north-then-east)
        (dolist (c '(#C(1 0) #C(0 1) #C(1 1) #C(-1 0)))
          (track c (score c))))))

  (nst:def-test keep-multiple (:values (:equalp '(-10 7 6))
                                       (:equalp '( 10 7 6)))
    (with-track-best (:keep 3)
      (dolist (v '(1 3 5 -10 -5 -4 6 7))
        (track v (abs v)))))

  (nst:def-test map-best (:equalp '((-10 -100) (7 49) (6 36)))
    (with-track-best (:keep 3 :return-best nil)
      (dolist (v '(1 3 5 -10 -5 -4 6 7))
        (track v (abs v)))
      (map-best #'(lambda (item score)
                    (list item (* item score)))))))
