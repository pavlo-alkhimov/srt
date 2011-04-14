(in-package #:kd)

(defclass node ()
  ((split-position :initarg :split-position :initform 0.0 :accessor node-split
                   :type coordinate)
   (split-axis :initarg :split-axis :initform 0 :accessor node-axis
               :type index-type)
   (left :initarg :l :initform nil :accessor node-left) 
   (right :initarg :r :initform nil :accessor node-right))
  (:documentation "If both branches are available, it is a node.
If a the LEFT has something and the RIGHT is nil,
it is a leaf and the LEFT has the contents of the leaf.
Otherwise, it is a node."))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (if (node-left obj)
        (if (node-right obj)
            (format stream "Node: ~[X~;Y~;Z~]=~,3f. left={~a} right={~a}"
                    (node-axis obj)
                    (node-split obj)
                    (node-left obj)
                    (node-right obj))
            (format stream "Leaf: ~[X~;Y~;Z~] [~S]"
                    (node-axis obj) (length (node-left obj))))
        (format stream "Empty."))))

(defun is-leaf (node)
  (DBG-MSG 10 "(is-leaf ~a) => ~a"
           node
           (and node
                (node-left node)
                (not (node-right node))))
  (and node
       (node-left node)
       (not (node-right node))))

(defun next-axis (axis)
  (mod (1+ axis) 3))

(defun prev-axis (axis)
  (mod (1- axis) 3))