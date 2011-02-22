(in-package #:kd)

(defclass aabb ()
  ((corners :accessor corners
            :type list)) 
  (:documentation "Axis-Aligned Bounding Box.
CORNERS are represented as list (x0 y0 z0 x1 y1 z1)"))
