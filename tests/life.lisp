(defpackage :cgl-tests/life
  (:use :cl :rove)
  (:import-from :cgl/life
                #:+dead+
                #:+alive+
                #:make-field
                #:make-game
                #:game-field
                #:inject!
                #:evolve-cell
                #:tick!)
  (:import-from :cgl/figures #:parse-figure))
(in-package :cgl-tests/life)


(defun test-field ()
  (let ((field (make-field 3 4)))
    (inject! field
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

  (testing "tick!"
    (let* ((field (test-field))
           (game (make-game :field field :generation 1 :population 0)))
      (ok (= (aref (game-field game) 0 3) +dead+) "control cell should be first dead")
      (tick! game)
      (ok (= (aref (game-field game) 0 3) +alive+) "control cell should be then alive"))))
