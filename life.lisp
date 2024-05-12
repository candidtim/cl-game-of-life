;;;; Game of Life

(load "figures.lisp")


;;; Initialization

(defun make-field (height width)
  (make-array (list height width) :element-type '(unsigned-byte 8) :initial-element 0))

(defun inject (field figure at-i at-j &optional center-p)
  (let ((loc-i (if center-p (- at-i (floor (array-dimension figure 0) 2)) at-i))
        (loc-j (if center-p (- at-j (floor (array-dimension figure 1) 2)) at-j)))
    (loop for i below (array-dimension figure 0) do
          (loop for j below (array-dimension figure 1) do
                (setf (aref field (+ i loc-i) (+ j loc-j)) (aref figure i j))))))

(defun alivep (cell-value) (>= cell-value 1))


;;; Rendering

(defun show-field (field &optional gen ppl)
  "Print the game field and return the number of lines printed out"
  (let ((height (array-dimension field 0)) (width (array-dimension field 1)))
    (format t "┏~v@{~A~:*~}┓~%" width "━")
    (loop for i below height do
          (format t "┃")
          (loop for j below width do
                (format t "~:[ ~;■~]" (alivep (aref field i j))))
          (format t "┃~%"))
    (format t "┗~v@{~A~:*~}┛~%" width "━")
    (format t "~@[Generation: ~5:d     ~]~@[Population: ~5:d~]~%" gen ppl)
    (+ height 3)))

(defun rewind (n)
  "Move the caret n lines up"
  (format t "~c[~aA" #\Esc n))


;;; Gameplay

(defun count-neighbors (field at-i at-j)
  "Returns a number of alive cells in a 3x3 square aroud the given cell, itself incuded"
  (let ((box-top (max (1- at-i) 0))
        (box-bottom (min (1+ at-i) (1- (array-dimension field 0))))
        (box-left (max (1- at-j) 0))
        (box-right (min (1+ at-j) (1- (array-dimension field 1)))))
    (loop for i from box-top to box-bottom
          sum (loop for j from box-left to box-right
                    count (alivep (aref field i j))))))

; TODO: represent newborn cells differently
(defun evolve-cell (field i j)
  "Returns next generation state for the given cell"
  (case (count-neighbors field i j)
    (3 1)
    (4 (aref field i j))
    (otherwise 0)))

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
  (assert (= (evolve-cell *test-field* 0 0) 0))  ; dead
  (assert (= (evolve-cell *test-field* 0 2) 1))  ; survided
  (assert (= (evolve-cell *test-field* 0 3) 1))  ; born
  (assert (= (evolve-cell *test-field* 1 0) 0))  ; died (lone)
  (assert (= (evolve-cell *test-field* 1 2) 0))) ; died (overpopulation)

(defun compute-next-gen (field)
  (let* ((height (array-dimension field 0))
         (width (array-dimension field 1))
         (new-field (make-field height width)))
    (loop for i below height
          do (loop for j below width
                   do (setf (aref new-field i j) (evolve-cell field i j))))
    new-field))

#+nil
(progn
  (show-field *test-field*)
  (show-field (compute-next-gen *test-field*)))

(defun tick (field)
  "Evolve the field to the next generation. Returns the new population count."
  (let ((next-gen (compute-next-gen field))
        (population 0)
        (change-count 0))
    (loop for i below (array-dimension field 0)
          do (loop for j below (array-dimension field 1)
                   do (if (alivep (aref next-gen i j)) (incf population))
                      (setf (aref field i j) (aref next-gen i j))))))
#+nil
(progn
  (show-field *test-field*)
  (tick *test-field*)
  (show-field *test-field*)
  (test-field-reset))

; TODO: accept figures as an argument
; TODO: print from a separate thread?
(defun play (height width tick-duration)
  (parse-library "figures")
  (let ((field (make-field height width))
        (mid-i (floor height 2))
        (mid-j (floor width 2)))
    (inject field (figure-by-name "gosper-glider-gun") mid-i mid-j t)
    (loop initially (rewind (show-field field 1))
          for population = (tick field)
          for gen from 2
          until (eq population 0) ; TODO: detect stable life
          finally (show-field field gen population)
          do
          (sleep tick-duration)
          (rewind (show-field field gen population)))))
