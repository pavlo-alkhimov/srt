(in-package #:kd)

(defparameter *minimal-dbg-level* 2)
(defparameter *current-dbg-level* 2)
(defparameter *dbg-prefix* "DEBUG: ")

(defmacro DBGMSG (&rest body)
  `(when (>= *current-dbg-level* *minimal-dbg-level*)
     (format t "~s ~a~%"
             *dbg-prefix*
             ,body)))

(defmacro DBGFORMAT (string &rest body)
  `(when (>= *current-dbg-level* *minimal-dbg-level*)
     (format t "~a" *dbg-prefix*)
     (format t ,string
             ,@body)))

(defmacro DBGEXE (&rest body)
  `(when (>= *current-dbg-level* *minimal-dbg-level*)
     ,@body))
