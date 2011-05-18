(in-package #:kd)

(defparameter *test-patch* nil)

(with-dbg-block "[TEST] Traverse"
  
  (set-dbg-level 4)
  
  (defparameter *file-prefix*
    #+windows "lisp/development/" ;; path provided by LispCabinet + Quicklisp
    #+darwin "programming/github/")
  (defparameter *file-infix* "srt/data/")
  (defparameter *file* "gourd.obj")
  (defparameter *file* "cube.obj")
  (defparameter *file* "v.obj")
  (defparameter *file* "go.obj")
  ;; (defparameter *file* "t2.obj")
  
  (defparameter *file-name* (concatenate 'string *file-prefix* *file-infix* *file*))
  
  (setf *test-patch* (load-patch *file-name*))
  (dump-many 1 ((dump *test-patch*))))