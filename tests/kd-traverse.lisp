(in-package #:kd)

(defparameter *test-patch* nil)

(with-dbg-block "[TEST] Traverse"
  (with-dbg-level 6
    
    (defparameter *file-prefix*
      #+windows "lisp/development/" ;; path provided by LispCabinet + Quicklisp
      #+darwin "programming/github/")
    (defparameter *file-infix* "srt/data/")
    (defparameter *file* (list "go3.obj" 1.0))
    
    (defparameter *file-name* (concatenate 'string *file-prefix* *file-infix* (car *file*)))
    
    (setf *test-patch* (load-patch *file-name* :scale (cadr *file*)))
    (dump-many 1 ((dump *test-patch*)))))