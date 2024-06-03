;;;; Entry point, wires and starts the gameplay and rendering

(defpackage :cgl/main
  (:use :cl)
  (:export #:main)
  (:import-from :cgl/tui #:configure-tty #:restore-tty
                         #:style +style-bold+ +style-reset+ +red+
                         #:erase-display #:write-at #:in-style #:draw-box)
  (:import-from :cgl/figures #:parse-library #:figure-by-name)
  (:import-from :cgl/life #:alivep #:game-field #:game-generation #:game-population
                          #:new-game #:inject! #:start-gameplay))
(in-package :cgl/main)


(defun princ-alive ()
  (format t (in-style "■" +red+ +style-bold+)))

(defun princ-dead ()
  (princ " "))

(defun render-field (game top left)
  (let* ((field (game-field game))
         (height (array-dimension field 0))
         (width (array-dimension field 1)))
    (loop for i below height do
          (loop for j below width do
                (if (alivep (aref field i j))
                    (write-at (+ top i) (+ left j) (in-style "■" +red+ +style-bold+))
                    (write-at (+ top i) (+ left j) " "))))
    (write-at (+ top height) 3
              (format nil " Generation: ~5:d " (game-generation game)))
    (write-at (+ top (array-dimension field 0)) (- width 18)
              (format nil " Population: ~5:d " (game-population game)))))

(defun run-tui (game &optional (fps 60))
  (configure-tty)
  (erase-display :all)
  (draw-box :height (array-dimension (game-field game) 0)
            :width (array-dimension (game-field game) 1)
            :title "Game of Life"
            :subtitle "Press 'q' to quit")
  (loop named main-loop
        do (progn
             (render-field game 2 2)
             (force-output)
             (when (eql (read-char-no-hang *standard-input*) #\q)
               (return-from main-loop))
             (sleep (/ 1 fps))))
  (restore-tty))

(defun main (height width start-figure &rest play-args)
  "usage: (main 40 80 \"gosper-glider-gun\" :tick-duration-seconds 0.05)"
  (parse-library "figures")
  (let ((game (new-game height width)))
    (inject! (game-field game)
             (figure-by-name start-figure)
             (floor height 2) (floor width 2) t)
    (apply #'start-gameplay (cons game play-args))
    (run-tui game)))
