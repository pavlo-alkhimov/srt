(in-package #:kd)

(defparameter *Debug-Level* 4)
(defparameter *current-block* "TOP")

(defmacro with-dbg-block (name &rest body)
  `(let ((*current-block* (concatenate 'string *current-block* ">" ,name)))
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
           (with-dbg-block ,(symbol-name name) ,@body))
        `(defun ,name ,args
           ,doc-string
           (with-dbg-block ,(symbol-name name) ,@body)))))

(defun trimmer (string &key (max-len 50))
  (if (> (length string) max-len)
      (let ((res (concatenate 'string
                              (subseq string 0 max-len)
                              "...")))
        (if (char= (aref string 0) #\()
            (concatenate 'string
                         res
                         ")")
            res))
      string))

(defmacro dbg-msg (level &rest body)
  `(when (<= ,level *Debug-Level*)
     (progn
       (format t "~a|~a: " ,level *current-block*)
       ,(if (consp body)
            `(format t ,@body)
            `(format t ,body))
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
  (when variables
    `(,(reduce #'(lambda (x y) (concatenate 'string x y))
               (mapcar #'(lambda(i) (string-downcase (format nil "~a~~a  " i)))
                       variables))
       ,@(mapcar #'(lambda (x)
                     `(if (listp ,x)
                          (let ((res (make-array '(0) :element-type 'base-char
                                                 :fill-pointer 0 :adjustable t)))
                            (with-output-to-string (s res)
                              (pprint ,x s))
                            (concatenate 'string " {pprinted}:" res))
                          (format nil "=~a" ,x)))
                 variables))))

(defmacro with-dbg (level format-group &rest body)
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

(defmacro dump-many (level format-group)
  (if (and format-group
           (car format-group))
      (append '(progn)
              (iter (for i in format-group)
                    (if (eq 'dump
                            (car i))
                        (let ((res (macroexpand-1 i)))
                          (collect `(DBG-MSG ,level ,@res)))
                        (collect `(DBG-MSG ,level ,@i)))))))

(defmacro with-dbg-header (level format-group &rest body)
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
       ,result)))