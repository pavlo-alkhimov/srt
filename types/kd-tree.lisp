(in-package #:kd)

(defclass kd-node ()
  ((split-position :initarg :split-position :initform 0.0 :accessor s
                   :type coordinate)
   (split-axis :initarg :split-axis :initform 0 :accessor a
               :type index-type)
   (left :initarg :l :initform nil :accessor l) 
   (right :initarg :r :initform nil :accessor r))
  (:documentation "If both branches are available, it is a node.
If a the LEFT has something and the RIGHT is nil,
it is a leaf and the LEFT has the contents of the leaf.
Otherwise, it is a node."))

(defmethod print-object ((obj kd-node) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-accessors ((s s) (a a) (l l) (r r)) obj
      (if l
          (if r
              (format stream "Node: ~[X~;Y~;Z~]=~,3f"
                      a s)
              (format stream "Leaf: ~[X~;Y~;Z~] [~S]"
                      a (length l)))
          (format stream "Empty.")))))

(defun is-leaf (node)
  (DBG-MSG 10 "(is-leaf ~a) => ~a"
          node
          (and node
               (slot-value node 'left)
               (not (slot-value node 'right))))
  (and node
       (slot-value node 'left)
       (not (slot-value node 'right))))

(defun next-axis (axis)
  (mod (1+ axis) 3))

(defun prev-axis (axis)
  (mod (1- axis) 3))