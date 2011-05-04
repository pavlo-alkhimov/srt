(in-package #:kd)

(defun get-coord-by-indexes (patch triangle vertex axis)
  (aref (patch-vs patch)
        (aref (patch-is patch)
              triangle
              vertex)
        axis))

(defmethod print-object ((obj patch) stream)
  (with-accessors ((name patch-name)
                   (vs patch-vs)
                   (vi patch-is)
                   (aabb patch-aabb)
                   (kd-tree patch-kd-tree)) obj
    (let* ((vs-len (array-dimension vs 0))
           (is-len (array-dimension vi 0))
           (short-name (subseq name
                               (1+ (or (position #\/ name :from-end t) -1))
                               (or (position #\. name :from-end t)
                                   (length name))))
           (*print-lines* 4))
      (format stream "#<\"~a\"~%~@(~r~) ~:*(~a) vert~:*~[ices~;ex~:;ices~]:" short-name vs-len)
      (pprint vs)
      (format stream "~%~@(~r~) ~:*(~a) index~:*~[es~;~:;es~]:" is-len)
      (pprint vi)
      (format stream "~%~a~%~a>"
              aabb (or kd-tree "No kd yet")))))

(defmethod initialize-instance :after ((p patch) &key name given-vs given-is)
  (setf (patch-name p) name)
  (setf (patch-vs p) (make-array (list (length given-vs) 3)
                                     :element-type 'coordinate
                                     :initial-contents given-vs))
  (setf (patch-is p) (make-array (list (length given-is) 3)
                                     :element-type 'index-type
                                     :initial-contents given-is))
  (setf (patch-aabb p) (calc-aabb (patch-vs p)))
  (setf (patch-kd-tree p) (build-tree p)))