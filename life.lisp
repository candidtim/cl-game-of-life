;;;; Game of Life

(load "figures.lisp")


;;; Initialization

(defconstant +alive+ 1)
(defconstant +dead+ 0)

(defun alivep (cell-value) (>= cell-value +alive+))

(defun make-field (height width)
  (make-array (list height width)
              :element-type '(unsigned-byte 8)
              :initial-element +dead+))

(defun inject (field figure at-i at-j &optional center-p)
  (let ((loc-i (if center-p (- at-i (floor (array-dimension figure 0) 2)) at-i))
        (loc-j (if center-p (- at-j (floor (array-dimension figure 1) 2)) at-j)))
    (loop for i below (array-dimension figure 0) do
          (loop for j below (array-dimension figure 1) do
                (setf (aref field (+ i loc-i) (+ j loc-j)) (aref figure i j))))))

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

(defun evolve-cell (field i j)
  "Returns next generation state for the given cell"
  (case (count-neighbors field i j)
    (3 +alive+)
    (4 (aref field i j))
    (otherwise +dead+)))

(defun compute-next-gen (field)
  (let* ((height (array-dimension field 0))
         (width (array-dimension field 1))
         (new-field (make-field height width)))
    (loop for i below height
          do (loop for j below width
                   do (setf (aref new-field i j) (evolve-cell field i j))))
    new-field))

(defconstant +no-change+ '(0)
  "An object representing that there were no changes in the playing field")

(defun tick (field)
  "Evolve the field to the next generation. Returns the new population count
  and an object representing the change signature (or +no-change+)."
  (let ((next-gen (compute-next-gen field))
        (population 0)
        (change-crc +no-change+))
    (loop for i below (array-dimension field 0)
          do (loop for j below (array-dimension field 1)
                   do (let ((old-cell (aref field i j))
                            (new-cell (aref next-gen i j)))
                        (if (alivep new-cell) (incf population))
                        (if (/= new-cell old-cell)
                            (progn
                              (setf change-crc (append change-crc (list i j new-cell)))
                              (setf (aref field i j) new-cell))))))
    (values population change-crc)))

(defun init (height width)
  (parse-library "figures")
  (let ((field (make-field height width))
        (mid-i (floor height 2))
        (mid-j (floor width 2)))
    (inject field (figure-by-name "penta-decathlon") mid-i mid-j t)
    field))

(defun fifo-add (elem lst max-length)
  "Like cons, but preserves the maximum length of a list"
  (if (>= (length lst) max-length)
      (cons elem (butlast lst))
      (cons elem lst)))

(defun play (field tick-duration)
  (do ((display-height (show-field field 1))
       (generation 2)
       (change-history nil)
       (done-p nil))
      (done-p generation)
    (multiple-value-bind (population change-crc) (tick field)
      (sleep tick-duration)
      (rewind display-height)
      (show-field field generation population)
      (if (equal change-crc +no-change+) (setf done-p t))
      (if (find change-crc change-history :test #'equal) (setf done-p t))
      (setf change-history (fifo-add change-crc change-history 15))
      (incf generation))))

