(ql:quickload :usocket)
(ql:quickload :croatoan)
(defpackage #:common-game
  (:use :cl)
  (:use :sb-ext)
  (:use :usocket)
  (:use :croatoan)
  (:export :evolve))

(in-package :common-game)

(defvar *port* 8080)
(defvar *ip-address* "127.0.0.1")
(dotimes (index (length *posix-argv*)) (when (equal (nth index *posix-argv*) "--port") (setf *port* (parse-integer (nth (+ 1 index) *posix-argv*)))))
(dotimes (index (length *posix-argv*)) (when (equal (nth index *posix-argv*) "--ip") (setf *ip-address* (nth (+ 1 index) *posix-argv*))))

(defun stream-read (stream)
  "Reads from a usocket connected stream"
  (read (usocket:socket-stream stream)))

(defun stream-print (string stream)
  "Prints to a usocket connected stream"
  (print string (usocket:socket-stream stream))
  (force-output (usocket:socket-stream stream)))

(defparameter my-stream (usocket:socket-connect *ip-address* *port*))
(defvar *in* ())


(defparameter *width*  100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)
(defparameter *plants* (make-hash-table :test #'equal))
(defparameter *reproduction-energy* 200)
(defparameter *players* ())
(defvar *animals* ())

(defstruct animal x y energy dir genes)

;; instead of using princ to draw to stdout, use add-string to draw to the curses screen.
;; the screen has to be initialized first in the main function evolve.
(defvar *recent-name* "")
(defun draw-world-croatoan (scr)
  (loop 
    for y 
    from 0
    below *height*
    do (loop 
         for x 
         from 0
         below *width*
         do (add-string scr
                        (format nil "~A"
                                (cond 
                                  ;; if there is one or more animals, print a M.
                                  ((some (lambda (animal) (and (= (animal-x animal) x)
                                                               (= (animal-y animal) y)))
                                         *animals*)
                                   #\M)
                                  ;; if there is a plant, print *
                                  ((gethash (cons x y) *plants*) #\*)

                                  ((some (lambda (player) (and (= (second player) x)
                                                               (= (third player) y)
                                                               (setf *recent-name* (first player))))
                                         *players*)
                                   *recent-name*)
                                  ;; if there is neithe a plant nor an animal, print a space.
                                  (t #\space)))
                        :y y
                        :x x)))
  ;; refresh the physical screen to dsplay the drawn changes.
  (refresh scr))

;; enter a recursive infinite loop as the programs main loop.
(defvar *last-signal* -1)
(defvar *timebomb* (get-internal-real-time))
(defun evolve ()
  (with-screen (scr :input-blocking nil :input-echoing nil :cursor-visibility nil)
    (clear scr)
    (setf (.background scr) (make-instance 'complex-char :color-pair '(:green :white)))

    (setq *width* (.width scr))
    (setq *height* (.height scr))

    (loop
      initially
      (draw-world-croatoan scr)

      for ch = (get-char scr)

      while (or (= ch -1) (not (equal (code-char ch) #\q)))
      do
      (update-world)
      (when (not (= ch -1)) (setf *timebomb* (get-internal-real-time)))
      (when (and (> (- (get-internal-real-time) *timebomb*) 175) (= ch -1)(not (= ch *last-signal*))) (stream-print ch my-stream)(setf *last-signal* ch))
      (when (and (not (= ch *last-signal*)) (not (= ch -1)))(setf *last-signal* ch)(stream-print ch my-stream))
      (draw-world-croatoan scr))))


(defun update-world ()
  (setf *in* (stream-read my-stream))
  (when (search "*plants*" *in*) (setf *plants* (make-hash-table :test #'equal)))
  (eval (read-from-string *in*)))

(evolve)
