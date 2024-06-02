;;;; Experimental TUI. No curses, mostly ANSI control sequences and stty control.

;;; Further information:
;;; ANSI Escape Codes: https://en.wikipedia.org/wiki/ANSI_escape_code

;; To-do list:
;; FIXME: Predefined control sequences should be constants/compiled/memorized?
;; TODO:  Handle signals, especially WINCH and INT
;; FIXME: Printing some wide characters doesn't work (e.g., emojis)
;; TODO:  Restore TTY configuration on any unhandled erorr
;;        (especially, disable the alternate screen buffer to see the errors)
;; FIXME: Non-blocking input is still processed on every "frame" only
;; FIXME: And yet, should this use terminfo?

(defpackage :cgl/tui
  (:use :cl)
  (:export #:main)
  (:import-from :alexandria #:define-constant)
  (:import-from :uiop #:run-program)
  (:import-from :cgl/figures #:parse-library #:figure-by-name)
  (:import-from :cgl/life #:alivep #:game-field #:game-generation #:game-population
                          #:new-game #:inject! #:start-gameplay))
(in-package :cgl/tui)


;;; Factories for control sequences

(defun esc-code (code)
  (format nil "~c~a" #\Esc code))

(defun csi (&key spec p i f)
  "Control Sequence Introducer, accepts either a complete specification in the `:spec'
  argument as a string, or individual 'bytes': parameter bytes in `:p',
  intermediate bytes in `:i', and a final byte in the `:f' argument, where parameter
  and intermediate bytes can be either a single value or a list of values, and
  intermediate bytes are optional."
  (if (not (null spec))
      (format nil "~c[~a" #\Esc spec)
      (format nil
              "~c[~:[~a~;~{~a~^;~}~]~:[~a~;~{~a~^;~}~]~a"
              #\Esc (listp p) p (listp i) i f)))


;;; Cursor movements

(defun cursor-up (&optional (n 1))
  (princ (csi :p n :f #\A)))

(defun cursor-down (&optional (n 1))
  (princ (csi :p n :f #\B)))

(defun cursor-forward (&optional (n 1))
  (princ (csi :p n :f #\C)))

(defun cursor-back (&optional (n 1))
  (princ (csi :p n :f #\D)))

(defun cursor-next-line (&optional (n 1))
  (princ (csi :p n :f #\E)))

(defun cursor-previous-line (&optional (n 1))
  (princ (csi :p n :f #\F)))

(defun cursor-horizontal-absolute (&optional (n 1))
  (princ (csi :p n :f #\G)))

(defun cursor-position (&optional (n 1) (m 1))
  (princ (csi :p (list n m) :f #\H)))

(defun erase-display (&optional (how 'down))
  "Erase in a display. The `how' can be one of: 'up, 'down, 'all, and 'purge.
  Purge is same as all, and also attempts to erase the scrollback history.
  Default is down."
  (let ((n (case how
             (down 0)
             (up 1)
             (all 2)
             (purge 3))))
    (princ (csi :p n :f #\J))))

(defun erase-line (how)
  "Erase in a line. The `how' can be one of: 'forward, 'backward, and 'all.
  Cursor position doesn't change. Default is forward."
  (let ((n (case how
             (forward 0)
             (backward 1)
             (all 2))))
    (princ (csi :p n :f #\K))))


;;; Text styles

(define-constant +style-reset+ 0)

(define-constant +style-bold+ 1)
(define-constant +style-dim+ 2)
(define-constant +style-no-bold+ 22)
(define-constant +style-no-dim+ 22)

(define-constant +style-italic+ 3)
(define-constant +style-no-italic+ 23)

(define-constant +style-underline+ 4)
(define-constant +style-double-underline+ 21)
(define-constant +style-no-underline+ 24)

(define-constant +style-blink+ 5)
(define-constant +style-blink-slow+ 5)
(define-constant +style-blink-rapid+ 6)
(define-constant +style-no-blink+ 25)

(define-constant +style-invert+ 7)
(define-constant +style-no-invert+ 27)

(define-constant +style-conceal+ 8)
(define-constant +style-no-conceal+ 28)

(define-constant +style-strike+ 9)
(define-constant +style-no-strike+ 29)

(defun style-font (&optional (n 0))
  "Choose an alternative font, from 1 to 10, or a default font 0. Default is 0."
  (+ 10 n))

(defun style (&rest styles)
  "Select Graphic Rendition (SGR) control sequence to set display attributes.
  `styles' argument can be one or many +style-...+ constants or values generated
  with other style- functions."
  (csi :p styles :f #\m))


;;;; Colors

(define-constant +black+ 30)
(define-constant +red+ 31)
(define-constant +green+ 32)
(define-constant +yellow+ 33)
(define-constant +blue+ 34)
(define-constant +magenta+ 35)
(define-constant +cyan+ 36)
(define-constant +white+ 37)
(define-constant +gray+ 90)
(define-constant +bright-red+ 91)
(define-constant +bright-green+ 92)
(define-constant +bright-yellow+ 93)
(define-constant +bright-blue+ 94)
(define-constant +bright-magenta+ 95)
(define-constant +bright-cyan+ 96)
(define-constant +bright-white+ 97)

(define-constant +bg-black+ 40)
(define-constant +bg-red+ 41)
(define-constant +bg-green+ 42)
(define-constant +bg-yellow+ 43)
(define-constant +bg-blue+ 44)
(define-constant +bg-magenta+ 45)
(define-constant +bg-cyan+ 46)
(define-constant +bg-white+ 47)
(define-constant +bg-gray+ 100)
(define-constant +bg-bright-red+ 101)
(define-constant +bg-bright-green+ 102)
(define-constant +bg-bright-yellow+ 103)
(define-constant +bg-bright-blue+ 104)
(define-constant +bg-bright-magenta+ 105)
(define-constant +bg-bright-cyan+ 106)
(define-constant +bg-bright-white+ 107)


;;; Display control

(defun show-cursor ()
  (princ (csi :spec "?25h")))

(defun hide-cursor ()
  (princ (csi :spec "?25l")))

(defun enable-alternate-screen-buffer ()
  (princ (csi :spec "?1049h")))

(defun disable-alternative-screen-buffer ()
  (princ (csi :spec "?1049l")))


;;; TTY control

(defun tty-raw ()
  "Put TTY into raw mode"
  (with-output-to-string (out)
    (values
      (uiop:run-program '("stty" "raw") :ignore-error-status t :output out :error-output t :input :interactive)
      out)))

(defun tty-sane ()
  "Put TTY into sane mode"
  (with-output-to-string (out)
    (values
      (uiop:run-program '("stty" "sane") :ignore-error-status t :output out :error-output t :input :interactive)
      out)))


;;; Signal processing
;; TODO


;;; High-level TTY control

(defun configure-tty ()
  (enable-alternate-screen-buffer)
  (hide-cursor)
  (tty-raw))

(defun restore-tty ()
  (disable-alternative-screen-buffer)
  (show-cursor)
  (tty-sane))


;;; High-level rendering

;; TODO: get terminal size, handle SIGWINCH
(defun draw-window ()
  ())


;;; Game rendering

(defun princ-alive ()
  (princ (style +red+ +bg-blue+ +style-bold+))
  (princ "■")
  (princ (style +style-reset+)))

(defun princ-dead ()
  (princ (style +bg-blue+))
  (princ " ")
  (princ (style +style-reset+)))

;; TODO: show generation and population
(defun render-field (field)
  (loop for i below (array-dimension field 0) do
        (loop for j below (array-dimension field 1) do
              (cursor-position i j)
              (if (alivep (aref field i j))
                  (princ-alive)
                  (princ-dead)))))

(defun run-tui (game &optional (fps 60))
  (configure-tty)
  (loop named main-loop
        for field = (game-field game)
        do (progn
             (render-field field)
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
