(in-package #:kd)

(defun get-coord-by-indexes (patch triangle vertex axis)
  (aref (patch-vs patch)
        (aref (patch-is patch)
              triangle
              vertex)
        axis))

(defmethod print-object ((obj tri-patch) stream)
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
                                   (length name)))))
      (format stream "#<\"~a\" V(~a) I(~a) AABB:~a ~a>"
              short-name vs-len is-len aabb (or kd-tree "No kd")))))

(defmethod initialize-instance :after ((patch tri-patch) &key name given-vs given-is)
  (let* ((vx-len (length given-vs))
         (ix-len (length given-is))) 
    (setf (patch-vs patch)
          (make-array (list vx-len 3)
                      :element-type 'coordinate
                      :adjustable nil))
    (setf (patch-is patch)
          (make-array (list ix-len 3)
                      :element-type 'index-type
                      :adjustable nil))
    (setf (patch-name patch) name)
    ;; copy vertexes: coerce
    (do ((v given-vs (cdr v))
         (i 0 (1+ i)))
        ((not v))
      (do ((coord (car v) (cdr coord))
           (j 0 (1+ j)))
          ((or (> j 2) (not coord))) 
        (setf (aref (patch-vs patch) i j)
              (coerce (car coord) 'coordinate))))
    ;; copy indexes: 1- and coerce
    (do ((ix given-is (cdr ix))
         (i 0 (1+ i)))
        ((not ix))
      (do ((index (car ix) (cdr index))
           (j 0 (1+ j)))
          ((or (> j 2) (not index))) 
        (setf (aref (patch-is patch) i j)
              (coerce (1- (car index)) 'index-type))))
    ;; aabb
    (setf (patch-aabb patch)
          (calc-aabb (patch-vs patch)))
    
    (setf (slot-value patch 'kd-tree-root)
          (build-tree patch))))