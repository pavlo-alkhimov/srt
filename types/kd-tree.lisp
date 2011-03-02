(in-package #:kd)

(defclass kd-node ()
  ((split-position :initarg :split-position
                   :initform 0.0
                   :accessor split-position
                   :type coordinate)
   (split-axis :initarg :split-axis
                :initform 0
                :accessor split-axis
                :type index-type)
   (left :initarg :l
         :initform nil
         :accessor l) 
   (right :initarg :r
          :initform nil
          :accessor r)) 
  (:documentation "If both branches are available, it is a node.
If a the LEFT has something and the RIGHT is nil,
it is a leaf and the LEFT has the contents of the leaf.
Otherwise, it is a node."))

(defparameter *default-nil-node* (make-instance 'kd-node))

(defun is-leaf (node)
  (and node
       (slot-value node 'left)
       (not (slot-value node 'right))))

(defun next-axis (axis)
  (mod (1+ axis) 3))

(defun prev-axis (axis)
  (mod (1- axis) 3))