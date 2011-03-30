(in-package #:kd)

(defclass tri-patch ()
  ((vertexes :accessor v
             :initform (make-array '(0 3)
                                   :element-type 'coordinate
                                   :adjustable t))
   (indexes :accessor i
            :initform (make-array '(0 3)
                                  :element-type 'index-type
                                  :adjustable t))
   (aabb :accessor a
         :type aabb
         :initform (make-instance 'aabb))
   (kd-tree-root :accessor k
                 :type (or kd-node null)
                 :initform nil)))
