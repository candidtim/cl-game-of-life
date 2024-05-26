(defpackage :cgl/main
  (:use :cl)
  (:export #:main)
  (:import-from :cgl/figures #:parse-library #:figure-by-name)
  (:import-from :cgl/life #:game-field #:new-game #:inject! #:play)
  (:import-from :cgl/viz #:show-field #:start-render)
  (:import-from :cgl/tui #:start-tui))
(in-package :cgl/main)


(defun main (height width start-figure &rest play-args)
  "Usage: (main 40 80 \"gosper-glider-gun\" :tick-duration-seconds 0.05)"
  (parse-library "figures")
  (let ((game (new-game height width)))
    (inject! (game-field game)
             (figure-by-name start-figure)
             (floor height 2) (floor width 2) t)
    (start-render game 60)
    (format t "Hit Ctrl-C to stop~%")
    (handler-case
      (apply #'play (cons game play-args))
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl ccl:interrupt-signal-condition
       #+clisp system::simple-interrupt-condition
       #+ecl ext:interactive-interrupt
       #+allegro excl:interrupt-signal
       ()
       (progn
         (show-field game)
         (format t "~%Aborted~%"))))))
