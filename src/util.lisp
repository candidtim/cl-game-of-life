(defpackage :cgl/util
  (:use :cl)
  (:export #:copy-array))
(in-package :cgl/util)

(defun copy-array (src)
  (loop with dst = (make-array (array-dimensions src))
        for x below (array-total-size src)
        do (setf (row-major-aref dst x) (row-major-aref src x))
        finally (return dst)))

#+nil
(let* ((src (make-array '(2 2) :initial-element 42))
       (copy (copy-array src)))
  (format t "~a" copy)
  (and (equalp src copy) (not (eql src copy))))
