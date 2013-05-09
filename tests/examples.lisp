;;; examples.lisp

(in-package :track-best-tests)

(nst:def-fixtures test-vertexes ()
  (+octohedron-vertexes+ '(#( 1  0  0)
                           #(-1  0  0)
                           #( 0  1  0)
                           #( 0 -1  0)
                           #( 0  0  1)
                           #( 0  0 -1))))

(nst:def-fixtures taxicab-distance ()
  (taxicab-distance-fn #'(lambda (a b)
                           (reduce #'+ (map 'list #'(lambda (x y)
                                                      (abs (- x y)))
                                            a b)))))

(nst:def-fixtures city-altitudes ()
  (+city-altitudes+'(("Alabama"  ("Birmingham" 664)
                                 ("Mobile" 218)
                                 ("Montegomery" 221))
                     ("Alaska"   ("Anchorage" 144)
                                 ("Fairbanks" 531))
                     ("Arizona"  ("Grand Canyon" 6606)
                                 ("Phoenix" 1132)
                                 ("Tuscon"  2641)))))

(nst:def-test-group examples ()
  (nst:def-test map-best (:equalp '(-25 16 -9))
    (with-track-best (:keep 3 :return-best nil)
      (dolist (v '(-5 -3 -1 0 2 4))
        (track v (abs v)))
      (map-best #'(lambda (item score)
                    (* item score)))))

  (nst:def-test longest-substring (:drop-values (:equal " Friday"))
    (let ((s1 "Keep playing cards on Friday.  Get smart.")
          (s2 "Thank G-d it's Friday!"))
      (with-track-best ()
        (dotimes (len (length s1))
          (dotimes (offset (- (length s1) len -1))
            (let ((needle (subseq s1 offset (+ offset len))))
              (when (search needle s2 :test #'string=)
                (track needle len))))))))

  (nst:def-test (closest-vertex :fixtures (test-vertexes
                                           taxicab-distance))
      (:values (:equalp (list #(1 0 0)
                              #(0 1 0)
                              #(0 0 1)))
               (:equal  (list (+ 1/2 1/3 1/4)
                              (+ 1/2 2/3 1/4)
                              (+ 1/2 1/3 3/4))))
    (with-track-best (:keep 3 :order-by-fn #'<)
      (dolist (v +octohedron-vertexes+)
        (track v (funcall taxicab-distance-fn v #(1/2 1/3 1/4))))))

  (nst:def-test (farthest-vertex :fixtures (test-vertexes
                                            taxicab-distance))
      (:values (:equalp #(-1 0 0))
               (:equal  (+ 3/2 1/3 1/4)))
    (with-track-best ()
      (dolist (v +octohedron-vertexes+)
        (track v (funcall taxicab-distance-fn v #(1/2 1/3 1/4))))))

  (nst:def-test (lowest-highest-point :fixtures (city-altitudes))
      (:values (:equalp '("Alaska" "Fairbanks"))
               (:equal  531))
    (with-track-best (:order-by-fn #'<)
      (dolist (state-info +city-altitudes+)
        (multiple-value-bind (city altitude)
            (with-track-best ()
              (dolist (city-info (rest state-info))
                (track (first city-info) (second city-info))))
          (track (list (first state-info) city) altitude)))))

  (nst:def-test lowest-and-highest (:seq (:equal 1)
                                         (:equal 10))
    (with-track-best (:name lowest :order-by-fn #'< :return-best nil)
      (with-track-best (:name highest :return-best nil)
        (dolist (v '(2 4 6 8 10
                     1 3 5 7 9))
          (track v v lowest)
          (track v v highest))
        (list (caar (map-best #'cons lowest))
              (caar (map-best #'cons highest)))))))
