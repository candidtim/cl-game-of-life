;;;; Game of Life

(defpackage :cgl/life
  (:use :cl)
  (:export #:game #:game-field #:game-generation #:game-population #:new-game
           #:inject! #:play)
  (:import-from :bordeaux-threads #:make-thread)
  (:import-from :cgl/figures #:figure-by-name))
(in-package :cgl/life)


;;; Game field

(defconstant +alive+ 1)
(defconstant +dead+ 0)

(defun alivep (cell-value) (>= cell-value +alive+))

(defun make-field (height width)
  (make-array (list height width)
              :element-type '(unsigned-byte 8)
              :initial-element +dead+))

(defun inject! (field figure at-i at-j &optional center-p)
  "Inject a figure into the game field. By default, figure's top-left corner is
  at the given coordinates. Optionally, the figure is centered around the given
  coordinates."
  (let ((loc-i (if center-p (- at-i (floor (array-dimension figure 0) 2)) at-i))
        (loc-j (if center-p (- at-j (floor (array-dimension figure 1) 2)) at-j)))
    (loop for i below (array-dimension figure 0) do
          (loop for j below (array-dimension figure 1) do
                (setf (aref field (+ i loc-i) (+ j loc-j)) (aref figure i j))))))


;;; Game state

(defstruct game
  "Game field and related state"
  field generation population)

(defun new-game (height width)
  "Initialize a new game structure"
  (make-game :field (make-field height width) :generation 1 :population 0))


;;; Rendering

(defun show-field (game)
  "Print the game field and return the number of lines printed out"
  (let* ((field (game-field game))
         (height (array-dimension field 0))
         (width (array-dimension field 1)))
    (format t "┏~v@{~A~:*~}┓~%" width "━")
    (loop for i below height do
          (format t "┃")
          (loop for j below width do
                (format t "~:[ ~;■~]" (alivep (aref field i j))))
          (format t "┃~%"))
    (format t "┗~v@{~A~:*~}┛~%" width "━")
    (format t "~@[Generation: ~5:d     ~]~@[Population: ~5:d~]~%"
            (game-generation game) (game-population game))
    (+ height 3)))

(defun rewind (n)
  "Move the caret n lines up"
  (format t "~c[~aA" #\Esc n))

(defun start-render (game &optional (fps 60))
  "An endless thread showing the game field with at most a given FPS"
  (make-thread
    (lambda ()
      (loop do (rewind (show-field game))
               (sleep (/ 1 fps))))))


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

(defun tick! (game)
  "Evolve the game state to the next generation.
  Returns a new game state and an object representing the change signature."
  (let* ((cur-field (game-field game))
         (height (array-dimension cur-field 0))
         (width (array-dimension cur-field 1))
         (new-field (make-field height width))
         (population 0)
         (changes nil))
    (loop for i below height
          do (loop for j below width
                   for old-cell = (aref cur-field i j)
                   for new-cell = (evolve-cell cur-field i j)
                   if (alivep new-cell) do (incf population)
                   if (/= new-cell old-cell) do (push (list i j new-cell) changes)
                   do (setf (aref new-field i j) new-cell)))
    ;; update the game state (caution: non atomic)
    (incf (game-generation game))
    (setf (game-population game) population)
    (setf (game-field game) new-field)
    changes))

(defun play (game &key
                  (tick-duration-seconds 0)
                  (fps 60)
                  (history-length 15)
                  (max-generation nil))
  "Play the game continuously until any of the endgame conditions are met"
  (start-render game fps)
  (do ((history (make-list history-length))
       (change (tick! game) (tick! game))
       (game-over nil))
      ((or (find change history :test #'equal)
           (and max-generation (>= (game-generation game) max-generation))))
    (setf history (cons change (butlast history)))
    (sleep tick-duration-seconds))
  (show-field game))
