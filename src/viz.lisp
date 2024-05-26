;;;; Simple visualization of Game of Life

(defpackage :cgl/viz
  (:use :cl)
  (:export #:show-field #:start-render)
  (:import-from :cgl/life #:alivep #:game-field #:game-generation #:game-population)
  (:import-from :bordeaux-threads #:make-thread))
(in-package :cgl/viz)


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
