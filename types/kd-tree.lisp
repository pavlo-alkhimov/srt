(in-package #:kd)

(defclass node ()
  ((split-position :initarg :split-position :initform 0.0 :accessor node-split
                   :type coordinate)
   (split-axis :initarg :split-axis :initform 0 :accessor node-axis
               :type index-type)
   (left :initarg :l :initform nil :accessor node-left) 
   (right :initarg :r :initform nil :accessor node-right))
  (:documentation "(and left right) => it is a node.
(and left (null right)) => it is a leaf
and the LEFT has the contents of the leaf."))

(defun get-print-list-from-kd-tree (obj)
  (if (node-left obj)
      (if (node-right obj)
          (list (case (node-axis obj)
                  (0 :x) (1 :y) (2 :z))
                (node-split obj)
                (get-print-list-from-kd-tree (node-left obj))
                (get-print-list-from-kd-tree (node-right obj)))
          (length (node-left obj)))
      :empty))

(defmethod print-object ((obj node) stream)
  (let ((res (get-print-list-from-kd-tree obj)))
    (if (consp res)
        (progn
          (format t "KD-TREE:")
          (pprint res stream))
        (format t "KD tree leaf with ~a triangle~:p.~%" res))))

(defun is-leaf (node)
  (if node
      (with-dbg 6 (("(is-leaf ~a)?" node))
                (and (node-left node)
                     (null (node-right node))))
      (error "Tried to execute (is-leaf nil). But nil is not a tree and cannot be checked, sorry.")))

(defun next-axis (axis)
  (mod (1+ axis) 3))

(defun prev-axis (axis)
  (mod (1- axis) 3))