(in-package #:kd)

(defparameter *Debug-Level* 1)
(defparameter *current-block* 'top-level)

(defmacro with-dbg-block (name &rest body)
  `(let ((*current-block* ',name))
     ,@body))

(defmacro defun-with-dbg (name args &body body)
  (let* ((doc-string (if (stringp (car body))
                         (prog1
                             (car body)
                           (setf body (cdr body))) ;; we trim the DOC-STRING from BODY
                         "Documentation is not provided"))
         (decl (and (eq 'declare (caar body))
                    (prog1
                        (car body)
                      (setf body (cdr body)))))) ;; we trim one(only one!!) DECLARATION from BODY
    (if decl
        `(defun ,name ,args
           ,doc-string
           ,decl
           (let ((*current-block* ',name))
             ,@body))
        `(defun ,name ,args
           ,doc-string
           (let ((*current-block* ',name))
             ,@body)))))

(defun trimmer (string &key (max-len 50))
  (if (> (length string) max-len)
      (let ((res (concatenate 'string
                              (subseq string 0 max-len)
                              "...")))
        (if (char= (aref string 0) #\()
            (concatenate 'string res ")")
            res))
      string))

(defmacro dbg-msg (level &rest body)
  `(when (<= ,level *Debug-Level*)
     (progn
       (format t "~a|~a: " ,level *current-block*)
       (format t ,@body)
       (format t "~%"))))

(defmacro dbg-exe (level &rest body)
  `(when (<= ,level *Debug-Level*)
     ,@body))

(defun point->string (point)
  (format nil "[~,3f ~,3f ~,3f]"
          (elt point 0)
          (elt point 1)
          (elt point 2)))

(defun set-dbg-level (new-level)
  (declare (type fixnum new-level))
  (if (not (= new-level *Debug-Level*))
      (progn
        (DBG-MSG *Debug-Level* "~[Increased~;Decreased~] *Debug-Level*: ~a->~a"
                 (if (> new-level *Debug-Level*)0 1)
                 *Debug-Level*
                 new-level)
        (setf *Debug-Level* new-level))))

(defmacro with-dbg-level (new-level &rest body)
  (with-gensyms (stored-level saved-result)
    `(let ((,stored-level *Debug-Level*)
           ,saved-result)
       (SET-DBG-LEVEL ,new-level)
       (setf ,saved-result (progn ,@body))
       (SET-DBG-LEVEL ,stored-level)
       ,saved-result)))

(defmacro dump (&rest variables)
  "Ugly thing..."
  (if variables
      `(,(reduce #'(lambda (x y) (concatenate 'string x y))
                 (mapcar #'(lambda(i) (string-downcase (format nil "~a:~~a  " i)))
                         variables))
         ,@(mapcar #'(lambda (x)
                       `(if (and (listp ,x)
                                 (numberp (car ,x))
                                 (< 2 (length ,x)))
                            (format nil "(~a ~a ~a ...[~a total])" (first ,x) (second ,x) (third ,x) (length ,x))
                            ,x))
                   variables))))

(defmacro with-dbg (level format-group &rest body)
  "Produces a debug output, executes the form and outputs the result.
LEVEL is a verbocity limit;
FORMAT-GROUP is either:
1) parameters of the FORMAT
2) (make-format ....);
BODY is... body."
  (with-gensyms (result)
    `(let ((,result))
       ,(if (and format-group
                 (car format-group))
            (append '(progn)
                    (iter (for i in format-group)
                          (if (eq 'dump
                                  (car i))
                              (let ((res (macroexpand-1 i)))
                                (collect `(DBG-MSG ,level ,@res)))
                              (collect `(DBG-MSG ,level ,@i))))
                    (list `(setf ,result (progn ,@body))))
            `(setf ,result (progn ,@body)))
       (DBG-MSG ,level
                "~a => ~a"
                (trimmer (string-downcase (format nil "~a" ',body)))
                ,result)
       ,result)))
