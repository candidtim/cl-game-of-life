(defpackage :cgl/main
  (:use :cl)
  (:export #:main)
  (:import-from :cgl/life #:init #:play))
(in-package :cgl/main)


(defun main (init-args play-args)
  "Usage: (main '(40 80) '(:tick-duration-seconds 0.05))"
  (format t "Hit Ctrl-C to stop~%")
  (handler-case

    (multiple-value-bind (field info) (apply #'init init-args)
      (apply #'play (append (list field info) play-args)))

    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      ()
      (format t "~%Aborted~%"))))
