(in-package #:kd)

(defparameter *debug-level* 1)

(defmacro DBGMSG (level &rest body)
  (when (>= level *minimal-dbg-level*)
    `(progn
       (format t "DEBUG[~a]: " level)
       (format t ,body))))

(defmacro DBGEXE (level &rest body)
  (when (>= level *minimal-dbg-level*)
    `(,@body)))
