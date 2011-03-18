(in-package #:kd)

(defclass aabb ()
  ((corners :accessor corners
            :type list)) 
  (:documentation "Axis-Aligned Bounding Box.
CORNERS are represented as list (x0 y0 z0 x1 y1 z1)"))

(defmethod initialize-instance :after ((box aabb) &key from-list)
  (setf (slot-value box 'corners)
        (copy-tree from-list)))

(defun calc-aabb (vertexes &key (start -1) (end -1))
  (let* ((start (if (= -1 start) 0 start))
         (end (if (= -1 end)
                  (array-dimension vertexes 0)
                  end)))
    (make-instance 'aabb
                   :from-list (loop for i from start below end 
                                 minimizing (aref vertexes i 0) into x0
                                 minimizing (aref vertexes i 1) into y0
                                 minimizing (aref vertexes i 2) into z0
                                 maximizing (aref vertexes i 0) into x1
                                 maximizing (aref vertexes i 1) into y1
                                 maximizing (aref vertexes i 2) into z1
                                 finally (return (list x0 y0 z0 x1 y1 z1))))))

(defun split-aabb (&key aabb axis position)
  (let* ((l (copy-tree aabb))
         (r (copy-tree l)))
    (setf (nth (+ axis 3) l)
          position)
    (setf (nth axis r)
          position)
    (values l r)))
