;;;; Simple visualization of Game of Life

(defpackage :cgl/viz
  (:use :cl)
  (:export #:show-field #:start-render #:main)
  (:import-from :cgl/figures #:parse-library #:figure-by-name)
  (:import-from :cgl/life #:game-field #:game-generation #:game-population
                          #:alivep #:new-game #:inject! #:play)
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

(defun main (height width start-figure &rest play-args)
  "usage: (main 40 80 \"gosper-glider-gun\" :tick-duration-seconds 0.05)"
  (parse-library "figures")
  (let ((game (new-game height width)))
    (inject! (game-field game)
             (figure-by-name start-figure)
             (floor height 2) (floor width 2) t)
    (start-render game 60)
    (format t "Hit Ctrl-C to stop~%")
    (handler-case
      (progn
         (apply #'play (cons game play-args))
         (show-field game))
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl ccl:interrupt-signal-condition
       #+clisp system::simple-interrupt-condition
       #+ecl ext:interactive-interrupt
       #+allegro excl:interrupt-signal
       ()
       (progn
         (show-field game)
         (format t "~%Aborted~%"))))))
