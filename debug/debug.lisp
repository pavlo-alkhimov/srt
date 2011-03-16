(in-package #:kd)

(defparameter *debug-level* 3)

(defmacro DBGMSG (level &rest body)
  `(when (<= ,level ,*debug-level*)
     (progn
       (format t "DEBUG[~a]: " ,level)
       (format t ,@body)
       (format t "~%"))))

(defmacro DBGEXE (level &rest body)
  `(when (<= ,level *debug-level*)
     ,@body))

(defun point->string (point)
  (format nil "[~,3f ~,3f ~,3f]"
          (elt point 0)
          (elt point 1)
          (elt point 2)))
