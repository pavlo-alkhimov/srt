(in-package #:kd)

(defclass tri-patch ()
  ((vertexes :accessor vertexes
             :initform (make-array '(0 3)
                                   :element-type 'coordinate
                                   :adjustable t))
   (indexes :accessor indexes
            :initform (make-array '(0 3)
                                  :element-type 'index-type
                                  :adjustable t))
   (aabb :accessor aabb
         :type aabb
         :initform (make-instance 'aabb))
   (tree :accessor tree
         :type kd-node
         :initform nil)))
