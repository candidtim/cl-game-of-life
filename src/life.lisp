;;;; Game of Life

(defpackage :cgl/life
  (:use :cl)
  (:export #:init #:play)
  (:import-from :bordeaux-threads #:make-thread)
  (:import-from :cgl/figures #:parse-library #:figure-by-name)
  (:import-from :cgl/util #:copy-array))
(in-package :cgl/life)


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

(defstruct gameplay-info generation population)


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

(defun start-render (field info &key (fps 60))
  "An endless thread showing the field with at most a given FPS"
  (make-thread
    (lambda ()
      (loop with display-height = 0
            for local-copy = (copy-array field)
            do (setf display-height
                     (show-field local-copy
                                 (gameplay-info-generation info)
                                 (gameplay-info-population info)))
               (sleep (/ 1 fps))
               (rewind display-height)))))


;;; Change history routines

(defconstant +no-change+ nil
  "An object representing that there were no changes in the playing field")

;; TODO: that's LIFO; better to init the lst to max-length at start?
(defun fifo-add (elem lst max-length)
  "Like cons, but preserves the maximum length of a list"
  (if (>= (length lst) max-length)
      (cons elem (butlast lst))
      (cons elem lst)))


;;; Gameplay

(defun count-alive (field &optional (top 0) (bottom nil) (left 0) (right nil))
  "Count alive cells in a field or a bounded box inside it."
  (loop with bottom-v = (or bottom (1- (array-dimension field 0)))
        with right-v = (or right (1- (array-dimension field 1)))
        for i from top to bottom-v
        sum (loop for j from left to right-v
                  count (alivep (aref field i j)))))

(defun count-neighbors (field at-i at-j)
  "Returns a number of alive cells in a 3x3 square aroud the given cell, itself incuded"
  (let ((top (max (1- at-i) 0))
        (bottom (min (1+ at-i) (1- (array-dimension field 0))))
        (left (max (1- at-j) 0))
        (right (min (1+ at-j) (1- (array-dimension field 1)))))
    (count-alive field top bottom left right)))

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
    (inject field (figure-by-name "gosper-glider-gun") mid-i mid-j t)
    (values field
            (make-gameplay-info :generation 1
                                :population (count-alive field)))))

(defun play (field info &key
                   (tick-duration-seconds 0)
                   (fps 60)
                   (stable-life-max-period 15)
                   (generation-evolve-for nil))

  (start-render field info
                ;; limit FPS if tick duration is set (no need in high FPS):
                :fps (if (= 0 tick-duration-seconds)
                         fps
                         (min fps (/ 1 tick-duration-seconds))))

  (do ((generation 2)
       (change-history nil)
       ;; game end:
       (done-p nil)
       (stable-life-period nil)
       (final-population nil))
      (done-p (values generation final-population stable-life-period))

    ;; evolve:
    (multiple-value-bind (population change-crc) (tick field)

      (setf (gameplay-info-generation info) generation)
      (setf (gameplay-info-population info) population)

      (sleep tick-duration-seconds)

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
