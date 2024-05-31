;;;; Interactive user interface

(defpackage :cgl/tui
  (:use :cl)
  (:export #:run-tui #:main)
  (:import-from :cgl/figures #:parse-library #:figure-by-name)
  (:import-from :cgl/life #:alivep #:game-field #:game-generation #:game-population
                          #:new-game #:inject! #:start-gameplay)
  (:import-from :cl-charms))
(in-package :cgl/tui)


(defun render-field (field window)
  (charms:clear-window window)
  (loop for i below (array-dimension field 0) do
        (loop for j below (array-dimension field 1) do
              (if (alivep (aref field i j))
                  (charms:write-string-at-point window "x" j i)
                  (charms:write-string-at-point window " " j i))))
  (charms:refresh-window window))

(defun run-tui (game &optional (fps 60))
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (let* ((height (array-dimension (game-field game) 0))
           (width (array-dimension (game-field game) 1))
           (window (charms:make-window (1+ width) (1+ height) 0 0)))
      (charms:enable-non-blocking-mode window)
      (loop named main-loop
            for field = (game-field game)
            do (progn
                 (render-field field window)
                 (when (eql (charms:get-char window :ignore-error t) #\q)
                   (return-from main-loop))
                 (sleep (/ 1 fps)))))))

(defun main (height width start-figure &rest play-args)
  "usage: (main 40 80 \"gosper-glider-gun\" :tick-duration-seconds 0.05)"
  (parse-library "figures")
  (let ((game (new-game height width)))
    (inject! (game-field game)
             (figure-by-name start-figure)
             (floor height 2) (floor width 2) t)
    (apply #'start-gameplay (cons game play-args))
    (run-tui game)))
