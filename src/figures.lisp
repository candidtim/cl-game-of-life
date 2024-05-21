;;;; Game of Life - Figure Library Parser

(defpackage :cgl/figures
  (:use :cl)
  (:export #:parse-library #:figure-by-name))
(in-package :cgl/figures)


(defparameter *figures* (make-hash-table :test 'equal))


(defun strip (text)
  (string-trim '(#\Space #\Tab #\Newline) text))

(defun commentp (text)
  (eq (char (strip text) 0) #\#))

(defun emptyp (text)
  (string= (strip text) ""))


(defun parse-figure (lines)
  (let* ((height (length lines))
         (width (loop for line in lines maximizing (length line)))
         (figure (make-array (list height width) :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop
      for line in lines
      for i from 0
      do (loop
           for ch across line
           for j from 0
           do (if (eq ch #\*) (setf (aref figure i j) 1))))
    figure))

(defun read-figure (s)
  "Consume the stream until enough lines representing a single figure are read.
  Return the significant lines, including the header."
  (loop for line = (read-line s nil)
        while (and line (not (emptyp line)))
        when (not (commentp line))
        collect line))

(defun parse-library (library-file-path)
  "Parse the given library file and add all figures to the global figures storage"
  (with-open-file (s library-file-path :direction :input)
    (loop for lines = (read-figure s)
          while (peek-char nil s nil) do
          (let ((name (car lines))
                (content (cdr lines)))
            (setf (gethash name *figures*) (parse-figure content))))))

(defun figure-by-name (name)
  (gethash name *figures*))
