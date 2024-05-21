(defpackage :cgl-tests/life
  (:use :cl :rove)
  (:import-from :cgl/life
                #:+dead+
                #:+alive+
                #:make-field
                #:inject
                #:evolve-cell
                #:compute-next-gen
                #:tick
                #:show-field)
  (:import-from :cgl/figures #:parse-figure))
(in-package :cgl-tests/life)


(defun test-field ()
  (let ((field (make-field 3 4)))
    (inject field
            (parse-figure '("  * "
                            "****"
                            "   *"))
            0 0 nil)
    field))


(deftest gameplay-test
  (testing "evolve-cell"
    (let ((field (test-field)))
      (ok (= (evolve-cell field 0 0) +dead+) "should be dead")
      (ok (= (evolve-cell field 0 2) +alive+) "should have survived")
      (ok (= (evolve-cell field 0 3) +alive+) "should have been born")
      (ok (= (evolve-cell field 1 0) +dead+) "should have died (lone)")
      (ok (= (evolve-cell field 1 2) +dead+) "should have died (overpopulation)")))

  (testing "compute-next-gen"
    (let ((field (compute-next-gen (test-field))))
      (show-field field)
      (ok (= (aref field 0 3) +alive+) "control cell should be alive in gen 2")))

  (testing "tick"
    (let ((field (test-field)))
      (show-field field)
      (ok (= (aref field 0 3) +dead+) "control cell should be first dead")
      (tick field)
      (show-field field)
      (ok (= (aref field 0 3) +alive+) "control cell should be then alive"))))
