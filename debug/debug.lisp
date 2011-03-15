(in-package #:kd)

(defparameter *debug-level* 3)

(defmacro DBGMSG (level &rest body)
  (when (<= level *debug-level*)
    `(progn
       (format t "DEBUG[~a]: " ,level)
       (format t ,@body)
       (format t "~%"))))

(defmacro DBGEXE (level &rest body)
  `(when (<= ,level *debug-level*)
     ,@body))
