;;;;initiate
(ql:quickload :hunchentoot)
(ql:quickload :defrest)
(defpackage #:common-vote
  (:use :cl)
  (:use :defrest)
  (:use :sb-ext)
  (:export :serve)
  (:export :test))
(in-package :common-vote)

(defvar *dispatch-table* (list (create-rest-table-dispatcher)));all routes

(defun create-client ()
  (let ((input ()) (state ()))
    (return-from create-vote
      (list :pop-input (lambda () (pop input)) :push-input (lambda (in) (push in input))))))





(defun process-request (in)
  (format t "qaz"))

(defun name-gen ()
  (format nil "butt"))

(defrest:defrest "/name-gen" :GET ()
  (format nil "~a" (name-gen)))

(defrest:defrest "/input" :GET ()
  (process-request (hunchentoot:get-parameter "vote"))
  (format nil "<p>hey</p>"))

(defrest:defrest "/" :GET ()
  (process-request (hunchentoot:get-parameter "vote"))
  (format nil "<script type='text/javascript'>
          function httpGet(theUrl)
          {
            var xmlHttp = new XMLHttpRequest();
            xmlHttp.open( "GET", theUrl, false  ); // false for synchronous request
            xmlHttp.send( null  );
            return xmlHttp.responseText;
          }
          var qaz = httpget(http://localhost:8080/name-gen);
          var newURL = '/input?name=' + qaz;
          setTimeout(function(){window.location.href = newURL;}, 5000); </script>"))


;;;;promising utilities
(defun list-exec (ls ex &optional (n -1))
  "takes a plist and a :property-name and executes the funcion at that location. optionally operates on the :property of a list at nth of the given list"
  (unless (= -1 n) (setf ls (nth n ls)))
  (funcall (getf ls ex)))

(defun split-by (char string);make general
    (loop for i = 0 then (1+ j)
          as j = (position #\, string :start i)
          collect (subseq string i j)
          while j))



;;;;book keeping
(defun serve ()
  (defvar *server* (defrest:start (make-instance 'defrest:easy-acceptor :port 8080)))
  (push (create-rest-table-dispatcher) hunchentoot:*dispatch-table*)
  (read))


(serve)
