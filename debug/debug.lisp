(in-package #:kd)

(defparameter *Debug-Level* 1)

(defmacro DBGMSG (level &rest body)
  `(when (<= ,level *Debug-Level*)
     (progn
       (format t "DEBUG[~a]: " ,level)
       (format t ,@body)
       (format t "~%"))))

(defmacro DBGEXE (level &rest body)
  `(when (<= ,level *Debug-Level*)
     ,@body))

(defun point->string (point)
  (format nil "[~,3f ~,3f ~,3f]"
          (elt point 0)
          (elt point 1)
          (elt point 2)))

(defun SET-DEBUG-LEVEL (new-level)
  (declare (type fixnum new-level))
  (if (not (= new-level *Debug-Level*))
      (progn
        (DBGMSG *Debug-Level* "~[Increased~;Decreased~] *Debug-Level*: ~a->~a"
                (if (> new-level *Debug-Level*)0 1)
                *Debug-Level*
                new-level)
        (setf *Debug-Level* new-level))))

(defmacro with-dbgmsg (level format-part &rest body)
  `(progn
     (DBGMSG ,level ,@format-part)
     ,@body))

;; example:
;; (with-message 3 ("tralala" x y) ;; < as for FORMAT
;;               (call uno)
;;               (call dos))
