(in-package #:kd)

(defun sah (aabb axis-index)
  "DRAFT(to test BUILD-KD): Splits the space taken by TRIANGLES by axis \"BY\" and returns the position."
  (let ((div-position  (+ 0.25 (random 0.5)))) 
    (+ (nth axis-index aabb)
       (* div-position (- (nth (+ 3 axis-index) aabb)
                          (nth axis-index aabb))))))

(defun touches-triangle (patch triangle axis-index split-position predicate)
  (iter (for i from 0 to 2)
        (always (funcall predicate
                         (get-coord-by-indexes patch triangle i axis-index)
                         split-position))))

(defun build-tree (patch &key
                   (aabb nil aabb-setp)
                   (axis-index 0)
                   (depth 0)
                   (max-depth 4 m-d-setp)
                   (triangles-list nil triangles-list-setp)
                   (triangles-count -1 triangles-count-setp)
                   (min-triangles-number 3))
  (declare (optimize (debug 3)))
  (if (and (< depth max-depth)
           (or (not triangles-count-setp)
               (< min-triangles-number triangles-count)))
      (let* ((triangles-list (if (not triangles-list-setp)
                                 (loop for i from 0 below (array-dimension (indexes patch) 0)
                                    collecting i)
                                 triangles-list))
             (aabb (if aabb-setp
                       aabb
                       (corners (slot-value patch 'aabb))))
             (split-position (sah aabb axis-index))
             l-triangles (l-count 0)
             r-triangles (r-count 0))
        (multiple-value-bind (l-aabb r-aabb) (split-aabb aabb
                                                         :axis axis-index
                                                         :position split-position)
          (loop for triangle in triangles-list
             when (touches-triangle patch triangle axis-index split-position #'>)
             do (progn (push triangle r-triangles)
                       (incf r-count))
             when (touches-triangle patch triangle axis-index split-position #'<)
             do (progn (push triangle l-triangles)
                       (incf l-count)))
          
          ;; (DBGMSG 4 "Call recursively or put everything here")
          (if (and (< 0 l-count)
                   (< 0 r-count)
                   (or (not triangles-count-setp)
                       (and (< l-count triangles-count)
                            (< r-count triangles-count))))
              (let ((next-axis (mod (1+ axis-index) 3))
                    (next-depth (1+ depth)))
                (DBGEXE 4
                        (DBGMSG 4 "DEPTH:~A AXIS-INDEX:~A TRIANGLES-COUNT:~A L-COUNT:~A R-COUNT:~A"
                                depth axis-index triangles-count l-count r-count)
                        (let ((difference (- (+ l-count r-count) (if (< triangles-count 0)
                                                                     (+ l-count r-count)
                                                                     triangles-count))))
                          (if (< difference 0)
                              (DBGMSG 4 "ERROR: ~a TRIANGLES ARE LOST!!!" (- difference))
                              (DBGMSG 4 "Intersect of ~a triangles" difference))))
                (DBGMSG 4 "Recursion...")
                (make-instance 'kd-node
                               :split-position split-position
                               :split-axis axis-index
                               :l (build-tree patch :aabb l-aabb :axis-index next-axis
                                              :depth next-depth :max-depth max-depth
                                              :triangles-list l-triangles :triangles-count l-count)
                               :r (build-tree patch :aabb r-aabb :axis-index next-axis
                                              :depth next-depth :max-depth max-depth
                                              :triangles-list r-triangles :triangles-count r-count)))
              (progn
                (DBGMSG 4 "No recursion. The leaf is created. DEPTH:~A AXIS-INDEX:~A TRIANGLES-COUNT:~A L-COUNT:~A R-COUNT:~A"
                        depth axis-index triangles-count l-count r-count)
                (make-instance 'kd-node :l triangles-list)))))
      (progn
        (when (not (< depth max-depth))
          )
        (if triangles-list
            (progn
              (DBGMSG 4 "Hit DEPTH:~A limit. The leaf is created. MAX-DEPTH:~A MIN-TRI-NUM:~A TRI-COUNT:~a"
                      depth max-depth
                      min-triangles-number triangles-count)
              (make-instance 'kd-node :l triangles-list))
            (progn
              (DBGMSG 4 "Hit DEPTH:~A limit]. TRIANGLES-LIST: nil." depth)
              nil)))))

(defun tree-statistics (tree &key (axis 0))
  (declare (type kd-node))
  (let ((next-axis (mod (1+ axis) 3)))
    (if (null tree)
        nil
        (if (and (null (r tree))
                 (l tree))
            (length (l tree))
            (list ;; (/ (round (* 100 (split-position tree))) 100.0)
             (split-axis tree)
             (tree-statistics (l tree) :axis next-axis)
             (tree-statistics (r tree) :axis next-axis))))))
