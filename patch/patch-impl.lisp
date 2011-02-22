(in-package #:kd)

(defmethod initialize-instance :after ((patch tri-patch) &key vertexes indexes)
  (let* ((vx-len (length vertexes))
         (ix-len (length indexes))) 
    (setf (slot-value patch 'vertexes)
          (make-array (list vx-len 3)
                      :element-type 'coordinate
                      :adjustable nil))
    (setf (slot-value  patch 'indexes) 
          (make-array (list ix-len 3)
                      :element-type 'index-type
                      :adjustable nil))
    ;; copy vertexes: coerce
    (do ((v vertexes (cdr v))
         (i 0 (1+ i)))
        ((not v))
      (do ((coord (car v) (cdr coord))
           (j 0 (1+ j)))
          ((or (> j 2) (not coord))) 
        (setf (aref (slot-value patch 'vertexes) i j)
              (coerce (car coord) 'coordinate))))
    ;; copy indexes: 1- and coerce
    (do ((ix indexes (cdr ix))
         (i 0 (1+ i)))
        ((not ix))
      (do ((index (car ix) (cdr index))
           (j 0 (1+ j)))
          ((or (> j 2) (not index))) 
        (setf (aref (slot-value patch 'indexes) i j)
              (coerce (1- (car index)) 'index-type))))
    ;; aabb
    (setf (slot-value patch 'aabb)
          (calc-aabb (slot-value patch 'vertexes)))))