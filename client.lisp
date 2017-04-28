(ql:quickload :usocket)
(defun stream-read (stream)
"Reads from a usocket connected stream"
  (read (usocket:socket-stream stream)))

;; (print) puts the string in the stream buffer
;; (force-output) pushes the buffer to the stream

(defun stream-print (string stream)
"Prints to a usocket connected stream"
  (print string (usocket:socket-stream stream))
  (force-output (usocket:socket-stream stream)))

(defparameter my-stream (usocket:socket-connect "127.0.0.1" 8082))

(defun read-loop ()
  (format t "~a" (stream-read my-stream))
  (read-loop))
