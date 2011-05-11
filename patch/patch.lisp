(in-package #:kd)

(defstruct triangle
  (v0 0 :type index-type)
  (v1 0 :type index-type)
  (v2 0 :type index-type)
  (square 0.0 :type coordinate))

(defclass patch ()
  ((name :accessor patch-name
         :type string
         :initform "Fresh empty patch")
   (vertexes :accessor patch-vs
             :initform (make-array '(0 3)
                                   :element-type 'coordinate
                                   :adjustable t))
   (normals :accessor patch-ns
            :initform (make-array '(0 3)
                                  :element-type 'coordinate
                                  :adjustable t))   
   (indexes :accessor patch-is
            :initform (make-array '(0 3)
                                  :element-type 'index-type
                                  :adjustable t))
   (squares :accessor patch-squares
            :type (array 'coordinate)
            :initform (make-array 0 :element-type '(array 'coordinate)))
   (aabb :accessor patch-aabb
         :type aabb
         :initform (make-instance 'aabb))
   (kd-tree-root :accessor patch-kd-tree
                 :type (or node null)
                 :initform nil)))
