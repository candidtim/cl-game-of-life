;;;; Interactive user interface

(defpackage :cgl/tui
  (:use :cl)
  (:export #:start-tui)
  (:import-from :cl-charms))
(in-package :cgl/tui)


(defun start-tui ()
  (format t "~a~%" "hi"))
