(ql:quickload :usocket)
(ql:quickload :croatoan)
(defpackage #:common-game
  (:use :cl)
  (:use :sb-ext)
  (:use :usocket)
  (:use :croatoan)
  (:export :evolve))
(in-package :common-game)

;; network code ;;
(defvar *in* ())
(defvar *port* 8080)
(defvar *ip-address* "127.0.0.1")
(dotimes (index (length *posix-argv*)) (when (equal (nth index *posix-argv*) "--port") (setf *port* (parse-integer (nth (+ 1 index) *posix-argv*)))))
(dotimes (index (length *posix-argv*)) (when (equal (nth index *posix-argv*) "--ip") (setf *ip-address* (nth (+ 1 index) *posix-argv*))))
;Reads command line args
(defun stream-read (stream)
  "Reads from a usocket connected stream"
  (read (usocket:socket-stream stream)))

(defun stream-print (string stream)
  "Prints to a usocket connected stream"
  (print string (usocket:socket-stream stream))
  (force-output (usocket:socket-stream stream)))

(defparameter my-stream (usocket:socket-connect *ip-address* *port*))
;; network code ;;

;; world state ;;
(defvar *width*  100)
(defvar *height* 30)
(defvar *x*  30)
(defvar *y* 30)
(defparameter *plants* (make-hash-table :test #'equal))
(defparameter *reproduction-energy* 200)
(defparameter *players* ())
(defstruct animal x y energy dir genes)
(defvar *animals* ())
(defvar *my-name* ())
;; world state ;;

;; Main logic and graphics;;
; enter a recursive infinite loop as the programs main loop.
(defvar *last-signal* -1)
(defvar *timebomb* (get-internal-real-time))
(defun evolve ()
  (with-screen (scr :input-blocking nil :input-echoing nil :cursor-visibility nil)
    (clear scr)
    (setf (.background scr) (make-instance 'complex-char :color-pair '(:green :white)))

    (setq *width* (.width scr))
    (setq *height* (.height scr))

    (loop
      for ch = (get-char scr)

      while (or (= ch -1) (not (equal (code-char ch) #\q)))
      do
      (update-world)
      (when (not (= ch -1)) (setf *timebomb* (get-internal-real-time)))
      (when (and (> (- (get-internal-real-time) *timebomb*) 175) (= ch -1)(not (= ch *last-signal*))) (stream-print ch my-stream)(setf *last-signal* ch))
      (when (and (not (= ch *last-signal*)) (not (= ch -1)))(setf *last-signal* ch)(stream-print ch my-stream))
      (unless *my-name* (setf *my-name* (first (car (last *players*)))))
      (let ((x (- (first (cdr (assoc *my-name* *players*))) (floor *width* 2))) (y (- (second (cdr (assoc *my-name* *players*))) (floor *height* 2))))
        (when (< (+ 10 *x*) x) (setf *x* (+ *x* 1)))
        (when (< (+ 10 *y*) y) (setf *y* (+ *y* 1)))
        (when (> (- *x* 10) x) (setf *x* (- *x* 1)))
        (when (> (- *y* 10) y) (setf *y* (- *y* 1))))
      (draw-world-croatoan scr))))

(defun update-world ()
  (setf *in* (stream-read my-stream))
  (when (search "*plants*" *in*) (setf *plants* (make-hash-table :test #'equal)))
  (eval (read-from-string *in*)))

; use add-string to draw to the curses screen.
; the screen has to be initialized first in the main function evolve.
(defvar *recent-name* "")
(defvar *recent-color* '(:green :white))
(defun draw-world-croatoan (scr)
(dotimes (height *height*)(add-string scr (format nil "~a" (subseq "                                                  " 0 (floor *width* 4))) :x 0 :y height :color-pair '(3 4)))
(dotimes (height *height*)(add-string scr (format nil "~a" (subseq "                                                  " 0 (floor *width* 4))) :x (* 3 (floor *width* 4)) :y height :color-pair '(3 4)))

(add-string scr (format nil "Player Name: ~a" *my-name*) :x 0 :y 1 :color-pair '(3 4))
(add-string scr (format nil "Energy: ~a" (fourth (assoc *my-name* *players*))) :x 0 :y 2 :color-pair '(3 4))
(add-string scr (format nil "Global Coords: ~a ~a" (second (assoc *my-name* *players*))(third (assoc *my-name* *players*))) :x 0 :y 3 :color-pair '(3 4));this will be based off of genetics
(add-string scr (format nil "Ability 1: Movement. WASD") :x 0 :y 4 :color-pair '(3 4));this will be based off of genetics
  (loop 
    for y 
    from 0
    below *height*
;    from (floor *height* 4)
;    below (* 3 (floor *height* 4))
    do (loop 
         for x 
         from (floor *width* 4)
         below (* 3 (floor *width* 4))
         do (progn (add-string scr
                        (format nil "~A"
                                (cond 
                                  ;; if there is one or more animals, print a M.
                                  ((some (lambda (animal) (and (= (animal-x animal) (+ x *x*))
                                                               (= (animal-y animal) (+ y *y*))))
                                         *animals*)
                                   #\M)
                                  ;; if there is a plant, print *
                                  ((gethash (cons (+ x *x*) (+ y *y*)) *plants*) #\*)

                                  ;; if there is a player, pring their name
                                  ((some (lambda (player) (and (= (second player) (+ x *x*))
                                                               (= (third player) (+ y *y*))
                                                               (setf *recent-name* (first player))
                                                               (when (equalp *my-name* *recent-name*)(setf *recent-color* '(3 4)))))
                                         *players*)
                                   *recent-name*)
                                  ;; if there is nothing, print a space.
                                  (t #\.)))
                        :y y
                        :x x
                        :color-pair *recent-color*) (setf *recent-color* '(:green :white)))))
  ;; refresh the physical screen to dsplay the drawn changes.
  (refresh scr))
;; main logic and graphics ;;

;; start up! ;;
(evolve)
;; start up! ;;
