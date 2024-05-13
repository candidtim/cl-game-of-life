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


;;; Change history routines

(defconstant +no-change+ nil
  "An object representing that there were no changes in the playing field")

(defun fifo-add (elem lst max-length)
  "Like cons, but preserves the maximum length of a list"
  (if (>= (length lst) max-length)
      (cons elem (butlast lst))
      (cons elem lst)))


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

(defun tick (field)
  "Evolve the field to the next generation. Returns the new population count
  and an object representing the change signature (or +no-change+)."
  (let ((next-gen (compute-next-gen field))
        (population 0)
        (change-crc +no-change+))
    (loop for x below (array-total-size field)
          for old-cell = (row-major-aref field x)
          for new-cell = (row-major-aref next-gen x)
          do (if (alivep new-cell) (incf population))
             (if (/= new-cell old-cell)
                 (progn
                   (setf change-crc (append change-crc (list x new-cell)))
                   (setf (row-major-aref field x) new-cell))))
    (values population change-crc)))

(defun init (height width)
  (parse-library "figures")
  (let ((field (make-field height width))
        (mid-i (floor height 2))
        (mid-j (floor width 2)))
    (inject field (figure-by-name "diehard") mid-i mid-j t)
    field))

(defun play (field &key
                   (tick-duration-seconds 0)
                   (stable-life-max-period 15)
                   (generation-start-from 0)
                   (generation-evolve-for nil)
                   (show t))
  (do ((display-height (if show (show-field field 1) nil))
       (generation 2)
       (change-history nil)
       ;; game end:
       (done-p nil)
       (stable-life-period nil)
       (final-population nil))
      (done-p (values generation final-population stable-life-period))

    ;; evolve:
    (multiple-value-bind (population change-crc) (tick field)

      ;; display:
      (if (and show (> generation generation-start-from))
          (progn
            (sleep tick-duration-seconds)
            (rewind display-height)
            (show-field field generation population)))

      ;; detect stop conditions:
      (let ((period (position change-crc change-history :test #'equal)))
        (if (or period ; stable life
                (and generation-evolve-for (> generation generation-evolve-for)))
            (progn
              (setf done-p t)
              (setf final-population population)
              (setf stable-life-period period))
            (progn
              (setf change-history
                    (fifo-add change-crc change-history stable-life-max-period))
              (incf generation)))))))
