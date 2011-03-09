(in-package #:kd)

(defparameter *kd-dbg-level* 0)

(defmacro ifdebug (level &rest body)
  `(when (and (numberp *kd-dbg-level*)
              (<= ,level *kd-dbg-level*))
     ,@body))
