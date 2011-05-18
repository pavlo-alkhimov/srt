(in-package #:kd)

(defclass patch ()
  ((name :accessor name
         :type string
         :initform "Fresh empty patch")
   
   (vertexes :accessor vertexes
             :initform (make-array '(0 3) :element-type 'coordinate))
   (triangles-indexes-array :accessor triangles-indexes-array
                            :initform (make-array '(0 3) :element-type 'index-type))
   
   (normals :accessor normals
            :initform (make-array '(0 3) :element-type 'coordinate))
   (normals-indexes-array :accessor normals-indexes-array
                          :initform (make-array '(0 3) :element-type 'index-type))
   
   (texture-coords :accessor texture-coords
                   :initform (make-array '(0 2) :element-type 'coordinate))
   (texture-coords-indexes-array :accessor texture-coords-indexes-array
                                 :initform (make-array '(0 3) :element-type 'index-type))
   
   (squares :accessor squares
            :initform (make-array 0 :element-type 'coordinate))
   (aabb :accessor aabb
         :type aabb
         :initform (make-instance 'aabb))
   (kd-tree :accessor kd-tree
            :type (or node null)
            :initform nil)
   
   (faces-list :accessor faces-list
               :documentation "List of indexes of vertexes of each original face: ((0 1 2 3 4)(0 1 6 5 8)(1 2 3)...)")))

