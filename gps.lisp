(defpackage #:gps
  (:use :cl)
  (:export :serve)
  (:export :evolve))
(in-package :gps)

(defvar *state* nil "The current state: a list of conditions.")

(defvar *ops* nil "A list of available operators.")

(defstruct op "An operation."
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun gps (*state* goals *ops*)
  "General Problem Solver: achieve all golas using *ops*."
  (if (achieve-all goals) 'solved))

(defun achieve (goal)
  "A goal is achieved if it already holds, or if there is an appropriate op for it that is applicable."
(format t "achieve ~a ~%" goal)
  (or (member goal *state*)
      (some #'apply-op
            (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is its add list."
  (member goal (op-add-list op)))

(defun apply-op (op)
  "Print a message and update *state* if op is applicable."
  (when (achieve-all (op-preconds op))
    (print (list 'executing (op-action op)))
(format t "hey")
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))
(defun achieve-all (goals)
  "Try to achieve each goal, then make sure they still hold."
  (and (every #'achieve goals) (subsetp goals *state*)))

(defun find-all (item sequence &rest keyword-args
                      &key (test #'eql) test-not &allow-other-keys)
  "find all those elements of sequence that match items,
  according to keywords. Doesn't alter sequence"
  (if test-not
    (apply #'remove item sequence
           :test-not (complement test-not) keyword-args)
    (apply #'remove item sequence
           :test (complement test) keyword-args)))

(defparameter *school-ops*
  (list
(make-op :action 'ask-phone-number
      :preconds '(in-comunication-with-shop)
      :add-list '(have-phone-number))
    (make-op :action 'drive-son-to-school
      :preconds '(son-at-home car-works)
      :add-list '(son-at-school)
      :del-list '(son-at-home))
    (make-op :action 'shop-installs-battery
      :preconds '(car-needs-battery shop-knows-problem shop-has-money)
      :add-list '(car-works))
    (make-op :action 'tell-shop-problem
      :preconds '(in-comunication-with-shop)
      :add-list '(shop-knows-problem))
    (make-op :action 'telephone-shop
      :preconds '(have-phone-number)
      :add-list '(in-comunication-with-shop))
    (make-op :action 'look-up-number
      :preconds '(have-phone-book)
      :add-list '(have-phone-number))
    (make-op :action 'give-shop-money
      :preconds '(have-money)
      :add-list '(shop-has-money)
      :del-list '(have-money))
))
;(debug :gps)
;;dbg section

(defvar *dbg-ids* nil "Identifiers used by dbg") 

(defun dbg (id format-string &rest args)
  "Print debugging info if (debug id) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun debug (&rest ids)
  "start dbg output on the given ids"
  (setf *dbg-ids* (union ids *dbg-io*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids. With no ids, stop debug altogether."
  (setf *dbg-ids* (if (null ids) nil
                    (set-difference *dbg-ids* ids))))

;todo(defun dbg-indent )


;(gps '(son-at-home car-needs-battery have-money have-phone-book) '(son-at-school) *school-ops*)



;(gps '(son-at-home have-money car-needs-battery have-phone-book) '(son-at-shool have-money) *school-ops*)
;(gps '(son-at-home have-money car-needs-battery have-phone-book) '(son-at-shool) *school-ops*)
