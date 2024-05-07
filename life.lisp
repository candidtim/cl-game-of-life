;;;; Game of Life

(load "figures.lisp")


;;; Initialization

(defun make-field (height width)
  (make-array (list height width) :element-type '(unsigned-byte 8) :initial-element 0))

(defun inject (field figure at-i at-j)
  (loop for i below (array-dimension figure 0) do
    (loop for j below (array-dimension figure 1) do
      (setf (aref field (+ i at-i) (+ j at-j)) (aref figure i j)))))

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
  (let ((box-top (max (1- at-i) 0))
        (box-bottom (min (1+ at-i) (1- (array-dimension field 0))))
        (box-left (max (1- at-j) 0))
        (box-right (min (1+ at-j) (1- (array-dimension field 1)))))
    (loop for i from box-top to box-bottom sum
      (loop for j from box-left to box-right count
        (alivep (aref field i j))))))

(defun compute-next-gen (field)
  (let ((new-field (make-field (array-dimension field 0) (array-dimension field 1))))
    (loop for i below (array-dimension field 0) do
      (loop for j below (array-dimension field 1) do
        (case (count-neighbors field i j)
          (3 (setf (aref new-field i j) 1))
          (4 (setf (aref new-field i j) (aref field i j)))
          (otherwise (setf (aref new-field i j) 0)))))
    new-field))

(defun tick (field)
  "Evolve the field to the next generation. Returns the new population count."
  (loop with next-gen = (compute-next-gen field)
        and population = 0
        and change-count = 0
        finally (return (values population change-count))
        for i below (array-dimension field 0) do
        (loop for j below (array-dimension field 1) do
              (if (alivep (aref next-gen i j))
                  (incf population))
              (if (not (eq (aref field i j) (aref next-gen i j)))
                  (progn (incf change-count)
                         (setf (aref field i j) (aref next-gen i j)))))))

; TODO: accept figures as an argument
; TODO: print from a separate thread?
(defun play (height width tick-duration)
  (parse-library "figures")
  (let ((field (make-field height width))
        (mid-i (floor height 2))
        (mid-j (floor width 2)))
    (inject field (figure-by-name "diehard") mid-i mid-j)
    (loop initially (rewind (show-field field 1))
          for population-and-change-count = (multiple-value-list (tick field))
          for population = (car population-and-change-count)
          for change-count = (cadr population-and-change-count)
          for gen from 2
          until (or (eq population 0) (eq change-count 0)) ; TODO: detect periodic stable life
          finally (show-field field gen population)
          do
      (sleep tick-duration)
      (rewind (show-field field gen population)))))
