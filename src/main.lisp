;;;; Entry point, wires and starts the gameplay and rendering

(defpackage :cgl/main
  (:use :cl)
  (:export #:main)
  (:import-from :cgl/tui #:configure-tty #:restore-tty #:on-resize
                         #:style +style-bold+ +style-reset+ +red+
                         #:erase-display #:write-at #:in-style #:draw-box
                         *terminal-rows* *terminal-cols*)
  (:import-from :cgl/figures #:parse-library #:figure-by-name)
  (:import-from :cgl/life #:alivep #:game-field #:game-generation #:game-population
                          #:new-game #:inject! #:start-gameplay))
(in-package :cgl/main)


(defun render-field (game top left)
  (let* ((field (game-field game))
         (height (array-dimension field 0))
         (width (array-dimension field 1)))
    (loop for i below height do
          (loop for j below width do
                (if (alivep (aref field i j))
                    (write-at (+ top i) (+ left j) (in-style "■" +red+ +style-bold+))
                    (write-at (+ top i) (+ left j) " "))))
    (write-at *terminal-rows* 3
              (format nil " Generation: ~5:d " (game-generation game)))
    (write-at *terminal-rows* 25
              (format nil " Population: ~5:d " (game-population game)))))

(defun run-tui (game &optional (fps 60))
  (configure-tty)
  (loop named tui-loop
        with should-redraw = t
        initially (on-resize (lambda () (setf should-redraw t)))
        do (progn
             (if should-redraw
                 (progn
                   (setf should-redraw nil)
                   (erase-display :all)
                   (draw-box :title "Game of Life"
                             :subtitle "Press 'q' to quit")))
             (render-field game 2 2)
             (force-output)
             (when (eql (read-char-no-hang *standard-input*) #\q)
               (return-from tui-loop))
             (sleep (/ 1 fps)))))

(defun main (height width start-figure &rest play-args)
  "usage: (main 40 80 \"gosper-glider-gun\" :tick-duration-seconds 0.05)"
  (parse-library "figures")
  (let ((game (new-game height width)))
    (inject! (game-field game)
             (figure-by-name start-figure)
             (floor height 2) (floor width 2) t)
    (apply #'start-gameplay (cons game play-args))
    (run-tui game)))
