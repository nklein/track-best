
# Track-Best Library

Track-Best is a Common Lisp library used to track the N best of some
series of items.  One creates a tracker and then adds scored items to
it.  The tracker keeps the N best items and returns them at the end.

The main entry point is the function `WITH-TRACK-BEST` macro.

    (with-track-best (keyword-args)
       ...body...)

The `body` can call `(track item score &optional tracker)` as often as
desired.  In the end, the `with-track-best` will return two values:
the list of best items and the list of their scores.

The `keyword-args` can be any of the following:

* `:NAME variable-name` --- A symbol used as the variable name for the
  tracker.  This is only needed if you will explicitly refer to the
  tracker in the `body` statements.
* `:KEEP number-to-keep` --- The number of items to track.  This
  defaults to `1` item.
* `:ORDER-BY-FN function-to-compare-scores` --- The function used to
  determine if one score is larger or smaller than another score.  The
  default for this argument is `#'>`
* `:SINGLE t-or-nil` --- If `single` is true and `keep` is `1`, then
  only return the best item rather than a list containing the best
  item.  The default for this argument is `T`.
* `:RETURN-BEST t-or-nil` --- if `return-best` is `NIL`, then the form
  returns the value(s) from the last expression in the `body` rather
  than the best items.  This argument defaults to `T`.  Note: This
  argument is evaluated at macroexpansion time, not at runtime.

## Example: Finding the longest matching substring in two strings

Given two strings `S1` and `S2`, find the longest substring they have
in common.

    (let ((s1 "Keep playing cards on Friday.  Get smart.")
          (s2 "Thank G-d it's Friday!"))
      (with-track-best ()
        (dotimes (len (length s1))
          (dotimes (offset (- (length s1) len -1))
            (let ((needle (subseq s1 offset (+ offset len))))
              (when (search needle s2 :test #'string=)
                (track needle len)))))))

The outer `DOTIMES` loop determines the length substring of `S1` to
look for in `S2`.  The inner `DOTIMES` loop picks the offset to start
the substring of `S1`.  Then, if the substring is found in `S2`, it is
tracked with its score being its length.

## Example: Finding the farthest vertex from a point

Given a list of vertexes `VERTEX-LIST`, find the vertex farthest from
a `TARGET` vertex based on a given `DISTANCE` function.

    (defun find-farthest (target vertex-list distance-fn)
      (with-track-best ()
        (dolist (v vertex-list)
          (track v (funcall distance-fn v target))))))

## Example: Finding the three closest vertexes to a point

Given a list of vertexes `VERTEX-LIST`, find the three that are closest
to a `TARGET` vertex based on a given `DISTANCE` function.

    (defun find-three-closest (target vertex-list distance-fn)
      (with-track-best (:keep 3 :order-by-fn #'<)
        (dolist (v vertex-list)
          (track v (funcall distance-fn v target))))))

The `:ORDER-BY-FN` function here specifies that the best vectors are
the ones with the smallest score.  The scores used here are the
distance from the `TARGET` as given by the `DISTANCE-FN`.

## Example: The lowest highest point

Suppose you had a list of various states.  For each state you had a
list of different cities and their altitude.  Find the highest
altitude in each state and find the state with the lowest high.

    (let ((data '(("Alabama"  ("Birmingham" 664)
                              ("Mobile" 218)
                              ("Montegomery" 221))
                  ("Alaska"   ("Anchorage" 144)
                              ("Fairbanks" 531))
                  ("Arizona"  ("Grand Canyon" 6606)
                              ("Phoenix" 1132)
                              ("Tuscon"  2641)))))
      (with-track-best (:order-by-fn #'<)
        (dolist (state-info data)
          (multiple-value-bind (city altitude)
              (with-track-best ()
                (dolist (city-info (rest state-info))
                  (track (first city-info) (second city-info))))
            (track (list (first state-info) city) altitude)))))

     => (values '("Alaska" "Fairbanks") 531)

The inner `DOLIST` here tracks the highest city in a given state.  The
outer `DOLIST` trackes the lowest of the highest cities.

## Example: Finding the lowest and highest numbers in a list

Given a list of numbers, return a list containing the lowest number
and highest number from the list.

    (with-track-best (:name lowest
                      :order-by-fn #'<
                      :return-best nil)
      (with-track-best (:name highest
                        :return-best nil)
        (dolist (v '(2 4 6 8 10 1 3 5 7 9))
          (track v v lowest)
          (track v v highest))

        (list (first (map-best #'cons lowest))
              (first (map-best #'cons highest)))))

In this example, we used the `:NAME` to specify two different
trackers.  In our loop, we tracked each value with both trackers.  In
the end, we also used the `:RETURN-RESULTS NIL` directive so that the
returned value would be our `LIST` expression rather than the best
result from the `LOWEST` tracker.

## Warning about how ties are handled

If the `:KEEP` parameter is `1`, the first item tracked with the best
score will be returned.  If the `:KEEP` parameter is greater than one,
there is no guarantee about which items are kept when multiple items
of the same score are found.  For example, the following form will
definitely return `(VALUES :EIGHT 8)`.

    (with-track-best ()
      (dolist (c '((:FIVE 5)  (:EIGHT 8)
                   (:CINQO 5) (:OCHO 8)
                   (:CINQ 5)  (:HUIT 8)))
        (track (first c) (second c))))

    => (values :EIGHT 8)

On the other hand, this snippet will not guarantee the order in which
`:EIGHT`, `:OCHO`, and `:HUIT` appear nor which of `:FIVE`, `:CINQO`,
or `:CINQ` will be included.

    (with-track-best (:keep 4)
      (dolist (c '((:FIVE 5)  (:EIGHT 8)
                   (:CINQO 5) (:OCHO 8)
                   (:CINQ 5)  (:HUIT 8)))
        (track (first c) (second c))))

    => (values ??? '(8 8 8 5))

To be sure, the answer is deterministic.  Given the same inputs in the
same order, the same output will be generated.  There are two caveats
to this:

* Where a tie gets inserted amongst its peers is highly sensitive to
  the number of items being kept and where its peers are in the list
  at the time of insertion.
* More importantly, I make no guarantee that this order will be
  preserved across releases.  You should not depend on particular
  behavior when `:KEEP` is greater than one.