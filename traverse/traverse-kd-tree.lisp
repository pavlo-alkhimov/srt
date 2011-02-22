(in-package #:kd)

(defun intersection-of-aabb-with-ray (aabb ray)
  "RAY is an array [2 3] coordinates: the vector \"from\" and the vector \"direction\". "
  (declare (optimize (debug 3)))
  (let* ((x (aref ray 0 0))
         (y (aref ray 0 1))
         (z (aref ray 0 2))
         (dx (aref ray 1 0))
         (dy (aref ray 1 1))
         (dz (aref ray 1 2))
         (aabb-data (corners aabb))
         (x0 (car aabb-data)) (aabb-data (cdr aabb-data))
         (y0 (car aabb-data)) (aabb-data (cdr aabb-data))
         (z0 (car aabb-data)) (aabb-data (cdr aabb-data))
         (x1 (car aabb-data)) (aabb-data (cdr aabb-data))
         (y1 (car aabb-data)) (aabb-data (cdr aabb-data))
         (z1 (car aabb-data))
         (method
          (cond ((not (= 0.0 (* dx dy dz))) :arbitrary) ;; the most frequent case is tested first
                ((= 0.0 dx dy dz) :zero-direction)
                ((= 0.0 dy dz) :x-line)
                ((= 0.0 dx dz) :y-line)
                ((= 0.0 dx dy) :z-line)
                ((= 0.0 dz) :xy-plane)
                ((= 0.0 dx) :yz-plane)
                ((= 0.0 dy) :xz-plane)
                (t (error "Failed to test the (~a,~a,~a) vector." dx dy dz))))
         ;; To make dx, dy and dz >=0, the space can be flipped:
         (flipped-x (when (< dx 0.0)
                      (setf dx (- dx))
                      (setf x (- x))
                      (setf x0 (- x1))   ;; !!! 0 -> 1
                      (setf x1 (- x0)))) ;; !!! 1 -> 0
         (flipped-y (when (< dy 0.0)
                      (setf dy (- dy))
                      (setf y (- y))
                      (setf y0 (- y1))   ;; !!! 0 -> 1
                      (setf y1 (- y0)))) ;; !!! 1 -> 0
         (flipped-z (when (< dz 0.0)
                      (setf dz (- dz))
                      (setf z (- z))
                      (setf z0 (- z1))   ;; !!! 0 -> 1
                      (setf z1 (- z0)))) ;; !!! 1 -> 0
         (result (case method
                   (:x-line (when (and (<= y0 y y1)
                                       (<= z0 z z1)
                                       (<= x x0))
                              (list x0 y z)))
                   (:y-line (when (and (<= x0 x x1)
                                       (<= z0 z z1)
                                       (<= y y0))
                              (list x y0 z)))
                   (:z-line (when (and (<= x0 x x1)
                                       (<= y0 y y1)
                                       (<= z z0))
                              (list x y z0)))
                   (:xy-plane (let* ((t-from-x (/ (- x0 x) dx))
                                     (t-from-y (/ (- y0 y) dy)))
                                (when (and (<= 0.0 t-from-x)
                                           (<= 0.0 t-from-y)
                                           (<= z0 z z1))
                                  (let* ((x-hit (+ x (* dx t-from-y)))
                                         (y-hit (+ y (* dy t-from-x))))
                                    (if (<= x0 x-hit x1)
                                        (list x-hit y0 z)
                                        (when (<= y0 y-hit y1)
                                          (list x0 y-hit z)))))))
                   (:xz-plane (let* ((t-from-x (/ (- x0 x) dx))
                                     (t-from-z (/ (- z0 z) dz))
                                     (x-hit (+ x (* dx t-from-z)))
                                     (z-hit (+ z (* dz t-from-x))))
                                (when (and (<= 0.0 t-from-x)
                                           (<= 0.0 t-from-z) 
                                           (<= y0 y y1))
                                  (if (<= x0 x-hit x1)
                                      (list x-hit y z0)
                                      (when (<= z0 z-hit z1)
                                        (list x0 y z-hit))))))
                   (:yz-plane (let* ((t-from-y (/ (- y0 y) dy))
                                     (t-from-z (/ (- z0 z) dz))
                                     (y-hit (+ y (* dy t-from-z)))
                                     (z-hit (+ z (* dz t-from-y))))
                                (when (and (<= 0.0 t-from-y)
                                           (<= 0.0 t-from-z)
                                           (<= x0 x x1))
                                  (if (<= y0 y-hit y1)
                                      (list x y-hit z0)
                                      (when (<= z0 z-hit z1)
                                        (list x y0 z-hit))))))
                   (:arbitrary (let* ((te (/ (- x0 x) dx)) ;; hit YZ plane at x = x0
                                      (y-res (+ y (* dy te)))
                                      (z-res (+ z (* dz te))))
                                 (if (and (<= 0.0 te)
                                          (<= y0 y-res y1)
                                          (<= z0 z-res z1))
                                     (list x0 y-res z-res)
                                     (let* ((te (/ (- y0 y) dy)) ;; hit XZ plane at y = y0
                                            (x-res (+ x (* dx te)))
                                            (z-res (+ z (* dz te))))
                                       (if (and (<= 0.0 te)
                                                (<= x0 x-res x1)
                                                (<= z0 z-res z1))
                                           (list x-res y0 z-res)
                                           (let* ((te (/ (- z0 z) dz)) ;; hit XY plane at z = z0
                                                  (x-res (+ x (* dx te)))
                                                  (y-res (+ y (* dy te))))
                                             (when (and (<= 0.0 te)
                                                        (<= x0 x-res x1)
                                                        (<= y0 y-res y1))
                                               (list x-res y-res z0)))))))))))
    (when result
      (when flipped-x
        (setf (nth 0 result) (- (nth 0 result))))
      (when flipped-y
        (setf (nth 1 result) (- (nth 1 result))))
      (when flipped-z
        (setf (nth 2 result) (- (nth 2 result)))))
    (values result
            method
            (when flipped-x :flipped-x)
            (when flipped-y :flipped-y)
            (when flipped-z :flipped-z))))

(defun intersection-of-split-with-ray (&key aabb ray axis-index split point)
  (declare (optimize (debug 3))
           (type fixnum axis-index)
           (type coordinate split)
           (type list point)
           (type (simple-array coordinate (2 3)) ray))
  (let* ((d (aref ray 1 axis-index));; i.e. DX
         (p (nth axis-index point)));; i.e. X
    ;; RAY touches the POINT?
    (if (= p split)
        ;; RAY lies in plane?
        (if (= 0.0 d)
            (values point
                    :in-plane)
            (values point
                    (if (> 0.0 d)
                        :R
                        :L)))
        ;; is RAY outgoing?
        (if (< 0.0 (* (- p split) d))
            (values point
                    (if (> 0.0 d)
                        :R
                        :L))
            (let* ((hit (let* ((te (/ (- split
                                         (aref ray 0 axis-index))
                                      d)))
                          (loop for i from 0 below 3
                             collect (+ (aref ray 0 i)
                                        (* te (aref ray 1 i))))))
                   (aabb-data (corners aabb))
                   (x0 (car aabb-data)) (aabb-data (cdr aabb-data))
                   (y0 (car aabb-data)) (aabb-data (cdr aabb-data))
                   (z0 (car aabb-data)) (aabb-data (cdr aabb-data))
                   (x1 (car aabb-data)) (aabb-data (cdr aabb-data))
                   (y1 (car aabb-data)) (aabb-data (cdr aabb-data))
                   (z1 (car aabb-data))
                   (x (nth 0 hit))
                   (y (nth 1 hit))
                   (z (nth 2 hit))) 
              (when (and (<= x0 x x1)
                         (<= y0 y y1)
                         (<= z0 z z1))
                (values hit
                        (if (> 0.0 d)
                            :RL
                            :LR))))))))

(defun test ()
  (let* ((aabb1 (make-instance 'aabb
                               :from-list '( 0.0  0.0  0.0
                                            10.0 10.0 10.0)))
         (ray1 (make-array '(2 3)
                           :element-type 'coordinate
                           :initial-contents '(( -1.0  -1.0  11.0)
                                               (  1.0   1.0  -3.0)))))
    (multiple-value-bind
          (point method fx fy fz)
        (intersection-of-aabb-with-ray aabb1 ray1)
      (multiple-value-bind
            (a b)
          (intersection-of-split-with-ray :aabb aabb1 :ray ray1
                                          :axis-index 2 :split 5.0
                                          :point point)
        (values a b point method fx fy fz)))))

(defun test2 ()
  (let* ((p (load-patch "d:/Paul.revised//git.repos//github//slrt//dodecahedron.obj"))
         (k (build-tree p :aabb (corners (aabb p))))
         (r (make-array '(2 3)
                        :element-type 'coordinate
                        :initial-contents '((-10.0 -10.0 -10.0)
                                            (  1.0   1.0   1.0)))))
    (setf (tree p) k)
    (intersection-of-patch-with-ray p r)))

(defun intersection-of-patch-with-ray (patch ray)
  (declare (type tri-patch patch))
  (multiple-value-bind
        (point method fx fy fz)
      (intersection-of-aabb-with-ray (aabb patch) ray)
    (when point
      (multiple-value-bind
            (a b)
          (intersection-of-split-with-ray :aabb (aabb patch)
                                          :ray ray
                                          :axis-index 0
                                          :split (split-position (tree patch)) 
                                          :point point)
        (pprint (tree-statistics (tree patch)))
        (format t "~%")
        (intersection-of-kd-with-ray (tree patch)
                                     (corners (aabb patch))
                                     ray
                                     :axis-index 0
                                     :point point)))))

(defun intersection-of-kd-with-ray (node aabb ray &key (axis-index 0) (point nil))
  (when node
    (if (and (l node)
             (not (r node)))
        (progn
          (format t "Leaf split by ~a, with ~a triangles is visited.~%" (case (mod (1- axis-index) 3)
                                                                          (0 'x) (1 'y) (2 'z)) 
                  (length (l node)))
          nil)
        (let ((next-axis (mod (1+ axis-index) 3)))
          (multiple-value-bind (l-aabb r-aabb)
              (split-aabb aabb :axis axis-index :position (split-position node))
            (format t "Entering the leaf split by ~a~%" (case (mod axis-index 3) (0 'x) (1 'y) (2 'z)))
            (or (intersection-of-kd-with-ray (l node) l-aabb ray :axis-index next-axis :point point)
                (intersection-of-kd-with-ray (r node) r-aabb ray :axis-index next-axis :point point))
            (format t "Leaving the leaf split by ~a~%" (case (mod axis-index 3) (0 'x) (1 'y) (2 'z))))))))
