(ql:quickload :usocket)
(ql:quickload :bordeaux-threads)
(defpackage #:common-game
  (:use :cl)
  (:use :sb-ext)
  (:use :usocket)
  (:export :serve)
  (:export :evolve))
(in-package :common-game)

;; biome bounds ;;
(defparameter *width*  150);this is in chars, not pixels. Much larger
(defparameter *height* 150)
(defparameter *jungle* '(45 10 10 10))
;; biome bounds ;;

;; wildlife state ;;
(defparameter *plant-energy* 40)
(defparameter *plants* (make-hash-table :test #'equal))
(defvar *animal-gene-count* 20)
;the first 8 are for pathfinding
;number 9 is for reproduction calorie count
;nunbers 10 through x are for abilities
(defstruct animal x y energy dir genes)
(defparameter *animals*
  (list (make-animal :x (ash *width* -1)
                     :y (ash *height* -1)
                     :energy 1000
                     :dir 0
                     :genes (loop repeat *animal-gene-count* collect (1+ (random 10))))
        (make-animal :x 45
                     :y 15
                     :energy 1000
                     :dir 0
                     :genes (loop repeat *animal-gene-count* collect (1+ (random 10))))
        (make-animal :x 46
                     :y 15
                     :energy 1000
                     :dir 0
                     :genes (loop repeat *animal-gene-count* collect (1+ (random 10))))
        (make-animal :x 47
                     :y 15
                     :energy 1000
                     :dir 0
                     :genes (loop repeat *animal-gene-count* collect (1+ (random 10))))
        (make-animal :x 48
                     :y 15
                     :energy 1000
                     :dir 0
                     :genes (loop repeat *animal-gene-count* collect (1+ (random 10))))
        (make-animal :x 49
                     :y 15
                     :energy 1000
                     :dir 0
                     :genes (loop repeat *animal-gene-count* collect (1+ (random 10))))))
;; wildlife state ;;

;; client state ;;
(defparameter *players* ())
(defparameter *input* ())
(defvar *names* "QWERTYUIOPASDFGHJKLZXCVBNM")
;; client state ;;

;; main loop state ;;
(defparameter *continue* nil)
(defparameter *sleep-time* 0.15)
(defparameter *commands* (make-hash-table :test #'equal))
;; main loop state ;;

;; function identifiers ;;
(defvar *W* 119)
(defvar *S* 115)
(defvar *A* 97)
(defvar *D* 100)
(defvar *status* 555)
(defvar *location* 554)
(defvar *eat* 553)
;; function identifiers ;;

;; infrastructure ;;
(defun a-list-exec (ls ex &optional key)
  "takes an alist and a function identifier and executes the function at that location. Automatically handles the case where an alist is stores as the cdr of a cons,
   and interprets the optional key to be an index of an alist in an alist. This should smooth over the use of alists as complex objects."
  (let ((real-ls ls))
    (when (last ls) (setf real-ls (cdr ls)));handles case where alist is stored as a named cons
    (when key (setf real-ls (cdr (assoc key ls))));handle case where alist is stored in an alist
  (funcall (or (cdr (assoc ex real-ls)) (lambda () (warning-log "failed a-list-exec "))))))

(defun sleepf (tim) (when (> tim 0) (sleep tim)))

(defvar *log* "")
(defun warning-log (new) (setf *log* (concatenate 'string *log* new)))
;; infrastructure ;;

;; wildlife functions ;;
(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width))
                   (+ top  (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  ;; First one in the jungle.
  (apply #'random-plant *jungle*)
  ;; Then one in the rest of the world.
  (random-plant 0 0 *width* *height*))

(defun move- (animal)
(when (or (= 2 (nth 12 (animal-genes animal))) (= 2 (nth 11 (animal-genes animal))))
  (let ((dir (animal-dir animal))
        (x   (animal-x animal))
        (y   (animal-y animal)))

    ;; If x>width, wrap around the world border with the mod function.
    (setf (animal-x animal)
          (mod (+ x (cond ((and (>= dir 2) (< dir 5)) 1)
                          ((or  (=  dir 1) (= dir 5)) 0)
                          (t -1))
                  *width*)
               *width*))

    ;; Down is +1, up is -1.
    (setf (animal-y animal)
          (mod (+ y (cond ((and (>= dir 0) (< dir 3)) -1)
                          ((and (>= dir 4) (< dir 7))  1)
                          (t 0))
                  *height*)
               *height*))

    (decf (animal-energy animal)))))

(defun turn (animal)
  (let ((x (random (apply #'+ (subseq (animal-genes animal) 0 7)))))
    (labels ((angle (genes x)
               (let ((xnu (- x (car genes))))
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))

      (setf (animal-dir animal)
            (mod (+ (animal-dir animal)
                    (angle (animal-genes animal) x))
                 8)))))

(defun eat (animal)

(when (or (= 3 (nth 12 (animal-genes animal))) (= 3 (nth 11 (animal-genes animal))))

  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*)))))

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e (* 100 (nth 9 (animal-genes animal))))
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
            (genes     (copy-list (animal-genes animal)))
            (mutation  (random *animal-gene-count*)))
        (setf (animal-x animal-nu) (+ 1 (animal-x animal-nu)))
        (setf (animal-y animal-nu) (+ 1 (animal-y animal-nu)))
        ;; mutate a random gene by +1 or -1.
        (setf (nth mutation genes)
              (max 1 (+ (nth mutation genes) (random 3) -1)))
        ;; deep copy of the gene list.
        (setf (animal-genes animal-nu) genes)
        ;; push the new animal to the list.
        (push animal-nu *animals*)))))
;1 = photo
;2 = walk
;3 = eat
;4 = kill
(defun photosynthisize (animal)
(let ((no-one-here t))

(dolist (other *animals*) (when (and (= (animal-x animal) (animal-x other)) (= (animal-y animal) (animal-y other)) (not (equalp (animal-genes animal) (animal-genes other))))
      (setf no-one-here nil)))
  (when (and no-one-here (or (= 1 (nth 12 (animal-genes animal))) (= 1 (nth 11 (animal-genes animal)))))
(incf (animal-energy animal) 2)))

(defun kill (animal)
  (when (or (= 4 (nth 12 (animal-genes animal))) (= 4 (nth 11 (animal-genes animal))))
    (dolist (other *animals*) (when (and (= (animal-x animal) (animal-x other)) (= (animal-y animal) (animal-y other)) (not (equalp (animal-genes animal) (animal-genes other))))
      (setf (animal-energy animal) (+ (animal-energy animal) (animal-energy other)))(setf (animal-energy other) -50)))
)))
;; wildlife functions ;;

;; main logic ;;
(defun update-world ()
  ;; Remove dead animals.
  (setf *animals*
        (remove-if (lambda (animal) (<= (animal-energy animal) 0))
                   *animals*))
(setf *animals*
        (remove-if (lambda (animal) (>= (animal-x animal) *width*))
                   *animals*))
(setf *animals*
        (remove-if (lambda (animal) (>= (animal-y animal) *height*))
                   *animals*))
  ;; Do what animals do.
  (mapc (lambda (animal)
          (turn animal)
          (move- animal)
          (eat animal)
          (photosynthisize animal)
          (setf (animal-energy animal) (- (animal-energy animal) 1))
          (reproduce animal))
        *animals*)
   ;; Do what players do.
   (dolist (player *players*) (when (remhash (a-list-exec player *location*) *plants*) (a-list-exec player *eat*)))
  ;; Grow plants.
  (add-plants))
(defun init ()
  (defparameter my-socket (usocket:socket-listen *WILDCARD-HOST* *port*))
  (bordeaux-threads:make-thread
   (lambda () (loop (let ((sock (usocket:socket-accept my-socket)) (name (make-name)) (x 30) (y 30) (energy 2000)(in ()))
                      (bordeaux-threads:make-thread (lambda () (loop (sleep 0.15) (stream-print (gen-players) sock)(stream-print (gen-hash *plants* (cons x y)) sock)(stream-print (gen-animals (cons x y)) sock))))
                      (bordeaux-threads:make-thread (lambda () (loop (sleep 0.15) (push (cons name (stream-read sock)) *input*))))
                      (push (cons name (list
                                        (cons -1 (lambda () (setf energy (- energy 1))))
                                        (cons *W* (lambda ()(setf y (- y 1)) (setf energy (- energy 2))))
                                        (cons *S* (lambda ()(setf y (+ 1 y)) (setf energy (- energy 2))))
                                        (cons *A* (lambda ()(setf x (- x 1)) (setf energy (- energy 2))))
                                        (cons *D* (lambda ()(setf x (+ 1 x)) (setf energy (- energy 2))))
                                        (cons *status* (lambda () (list name x y energy)))
                                        (cons *location* (lambda () (cons x y)))
                                        (cons *eat* (lambda () (setf energy (+ energy *plant-energy*))))
                                        )) *players*))))))
(defun serve ()
  "Main control loop"
(setf *continue* t)
(setf *sleep-time* 0.15)
  (loop
    while *continue*
    do
                                        ;(format t "~a~%" *input*)
    (let ((start-time (get-internal-real-time)))
      (loop while (> (length *input*) 0)
            do (let ((command (pop *input*)))
                                        ;(format t "~a~%" command)
                 (setf (gethash (car command) *commands*) (cdr command))))
      (maphash #'(lambda (key value) (a-list-exec *players* value key)) *commands*)

      (update-world)
      (sleepf (- *sleep-time* (/ (- (get-internal-real-time) start-time) internal-time-units-per-second))) 
)))
;; main logic ;;

;; network code ;;
(defvar *port* 8080)
(dotimes (index (length *posix-argv*)) (when (equal (nth index *posix-argv*) "--port") (setf *port* (parse-integer (nth (+ 1 index) *posix-argv*)))))
(defun stream-read (stream)
  "Reads from a usocket connected stream"
  (read (usocket:socket-stream stream)))
(defun stream-print (string stream)
  "Prints to a usocket connected stream"
  (print string (usocket:socket-stream stream))
  (force-output (usocket:socket-stream stream)))

(defun gen-hash (hash target)
  (let ((str "(progn "))
    (maphash #'(lambda (key value) (when (and value (> (car key) (- (car target) 30))(< (car key) (+ (car target) 30))(> (cdr key) (- (cdr target) 30))(< (cdr key) (+ (cdr target) 30))) (setf str (concatenate 'string str (format nil "(setf (gethash '~a *plants*) t)" key))))) *plants*) (concatenate 'string str ")")))

(defun filter  (f args x)
    (cond ((null args) nil)
        ((if (funcall f (car args) x)
            (cons (car args) (filter  f (cdr args) x))
            (filter  f (cdr args) x)))))

(defun in-vision (animal coords)
(let ((target (cons (animal-x animal) (animal-y animal))))
  (when
    (and (> (car coords) (- (car target) 30))(< (car coords) (+ (car target) 30))(> (cdr coords) (- (cdr target) 30))(< (cdr coords) (+ (cdr target) 30)))
    (return-from in-vision t)) nil))

(defun gen-animals (coords)
  (let ((filtered (filter #'in-vision *animals* coords)))
  (format nil "(setf *animals* '~a)" filtered)))
(defun gen-players ()
  (let ((str "(progn (setf *players* ()) "))
    (dolist (play *players*)
      (setf str (concatenate 'string str (format nil "(push '~a *players*)" (a-list-exec play *status*))))) (concatenate 'string str ")")))

(defun make-name () (format t "new thread ~%") (let* ((rand (random (length *names*)))(name (subseq *names* (- rand 1) rand)))(setf *names* (concatenate 'string (subseq *names* 0 (- rand 1)) (subseq *names* rand))) name))
;; network code ;;

;; admin functions ;;
(defun adimal ()
  (push
    (make-animal :x (+ 30 (random 40))
                     :y (+ 30 (random 40))
                     :energy 1000
                     :dir 0
                     :genes (loop repeat *animal-gene-count* collect (1+ (random 10)))) *animals*))

(defun boot () (unless *continue* (bordeaux-threads:make-thread (lambda () (serve)))))
(defun fast () (setf *sleep-time* 0))
(defun pause () (setf *continue* (not *continue*)))
(defun clear-plants ()(setf *plants* (make-hash-table :test #'equal)))
(defun status () (format t "~a plants~%~a animals~%" (hash-table-count *plants*) (length *animals*)))

(defun genes ()
  (let ((one 0) (two 0) (three 0 ) (four 0) (five 0))
    (dolist (animal *animals*)
      (format t "~a . ~a~%" (nth 11 (animal-genes animal)) (nth 12 (animal-genes animal)))
      (when (or (eq (nth 11 (animal-genes animal)) 1) (eq (nth 12 (animal-genes animal)) 1)) (setf one (+ one 1)))
      (when (or (eq (nth 11 (animal-genes animal)) 2) (eq (nth 12 (animal-genes animal)) 2)) (setf two (+ two 1)))
      (when (or (eq (nth 11 (animal-genes animal)) 3) (eq (nth 12 (animal-genes animal)) 3)) (setf three (+ three 1)))
      (when (or (eq (nth 11 (animal-genes animal)) 4) (eq (nth 12 (animal-genes animal)) 4)) (setf four (+ four 1)))
      (when (or (eq (nth 11 (animal-genes animal)) 4) (eq (nth 12 (animal-genes animal)) 4)) (setf four (+ four 1)))
      (when (or (> (nth 11 (animal-genes animal)) 4) (< (nth 11 (animal-genes animal)) 1)
        (> (nth 12 (animal-genes animal)) 4) (< (nth 12 (animal-genes animal)) 1) ) (setf five (+ four 1))))
    (format t "Out of ~a animals:~%Photo:~a Walk:~a Eat:~a Kill:~a Dud:~a~%" (length *animals*) one two three four five)))

(defun count-gene (gene)
  (let ((matches ()) (one 0) (two 0) (three 0) (four 0) (five 0))
    (dolist (animal *animals*)
      (when (or (eq (nth 11 (animal-genes animal)) gene) (eq (nth 12 (animal-genes animal)) gene)) (push animal matches)))
    (dolist (match matches)
      (when (or (eq (nth 11 (animal-genes match)) 1) (eq (nth 12 (animal-genes match)) 1)) (setf one (+ one 1)))
      (when (or (eq (nth 11 (animal-genes match)) 2) (eq (nth 12 (animal-genes match)) 2)) (setf two (+ two 1)))
      (when (or (eq (nth 11 (animal-genes match)) 3) (eq (nth 12 (animal-genes match)) 3)) (setf three (+ three 1)))
      (when (or (eq (nth 11 (animal-genes match)) 4) (eq (nth 12 (animal-genes match)) 4)) (setf four (+ four 1)))
      (when (or (> (nth 11 (animal-genes match)) 4) (< (nth 11 (animal-genes match)) 1)
        (> (nth 12 (animal-genes match)) 4) (< (nth 12 (animal-genes match)) 1) ) (setf five (+ four 1))))
    (format t "Out of ~a animals:~%Photo:~a Walk:~a Eat:~a Kill:~a Dud:~a~%" (length matches) one two three four five)))

(defun draw-world ();does not show players yet
  (loop for y
        below *height*
        do (progn (fresh-line)
                  ;; beginning of the line.
                  (princ "|")
                  (loop for x
                        below *width*
                        do (princ (cond 
                                    ;; if there is one or more animals, print a M.
                                    ((some (lambda (animal) (and (= (animal-x animal) x)
                                                                 (= (animal-y animal) y)))
                                           *animals*)
                                     #\M)
                                    ;; if there is a plant, print *
                                    ((gethash (cons x y) *plants*) #\*)
                                    ;; if there is neithe a plant nor an animal, print a space.
                                    (t #\space))))
                  ;; end of the line.
                  (princ "|"))))
;; admin functions ;;

;; run ;;
(init)
(dotimes (x 500) (adimal))
(boot)
;; run ;;

