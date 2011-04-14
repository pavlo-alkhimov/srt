(in-package #:kd)

(defstruct triangle
  (v0 0 :type index-type)
  (v1 0 :type index-type)
  (v2 0 :type index-type)
  (square 0.0 :type coordinate))

(defclass tri-patch ()
  ((name :accessor patch-name
         :type string
         :initform "fresh patch")
   (vertexes :accessor patch-vs
             :initform (make-array '(0 3)
                                   :element-type 'coordinate
                                   :adjustable t))
   (vertexes-indexes :accessor patch-is
                     :initform (make-array '(0 3)
                                           :element-type 'index-type
                                           :adjustable t))
   (triangles-data :accessor patch-tris
                   :type (array triangle-cached-ref)
                   :initform (make-array 0 :element-type 'triangle))
   (aabb :accessor patch-aabb
         :type aabb
         :initform (make-instance 'aabb))
   (kd-tree-root :accessor patch-kd-tree
                 :type (or kd-node null)
                 :initform nil)))
