#+nil
(progn
  (defvar *test-field* (make-field 3 4))
  (defun test-field-reset ()
    (inject *test-field*
            (parse-figure '("  * "
                            "****"
                            "   *"))
            0 0 nil))
  (test-field-reset)
  (show-field *test-field*))

#+nil
(progn
  (assert (= (evolve-cell *test-field* 0 0) +dead+))  ; dead
  (assert (= (evolve-cell *test-field* 0 2) +alive+)) ; survided
  (assert (= (evolve-cell *test-field* 0 3) +alive+)) ; born
  (assert (= (evolve-cell *test-field* 1 0) +dead+))  ; died (lone)
  (assert (= (evolve-cell *test-field* 1 2) +dead+))) ; died (overpopulation)

#+nil
(progn
  (show-field *test-field*)
  (show-field (compute-next-gen *test-field*)))

#+nil
(progn
  (show-field *test-field*)
  (tick *test-field*)
  (show-field *test-field*)
  (test-field-reset))

#+nil
(let ((field (init 10 20)))
  (multiple-value-bind (generation population period)
    (play field :generation-evolve-for 5 :show nil)
    (show-field field generation population)
    (format t "Stable life period, if any: ~a~%" period)))
