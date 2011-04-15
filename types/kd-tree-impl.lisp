(in-package #:kd)




(defun-with-dbg build-tree (patch &key (triangles nil triangles-setp)
                                  (current-aabb nil current-aabb-setp)
                                  (axis-index 0)
                                  (recursion-steps-left 1)
                                  (min-triangles-count 3))
  (declare (optimize (debug 3))
           (type (or null aabb) current-aabb))
  (flet ((try-tri (triangle split-position predicate)
           (iter (for i from 0 to 2)
                 (thereis (funcall predicate
                                   (get-coord-by-indexes patch triangle i axis-index)
                                   split-position)))))
   (if (< 0 recursion-steps-left)
       (let* ((triangles (if (not triangles-setp)
                             (iter (for x from 0 below (array-dimension (patch-is patch) 0))
                                   (collect x))
                             triangles))
              (current-aabb (if current-aabb-setp
                                current-aabb
                                (patch-aabb patch)))
              (split-position (old-sah patch current-aabb axis-index triangles))
              l-triangles
              r-triangles)
         (iter (for i in triangles)
               ;; triangle touches left
               (when (try-tri i split-position #'<)
                 (push i l-triangles))
               ;; triangle touches right
               (when (try-tri i split-position #'>)
                 (push i r-triangles))
               ;; triangle is in-plane
               (when (not (try-tri i split-position #'/=))
                 (push i l-triangles)))
         (multiple-value-bind (l-aabb r-aabb) (split-aabb current-aabb axis-index split-position)
           (if (and (< min-triangles-count (length l-triangles) (length triangles))
                    (< min-triangles-count (length r-triangles) (length triangles)))
               (let ((next-axis (mod (1+ axis-index) 3))
                     (recursion-steps-left (1- recursion-steps-left)))
                 (make-instance 'node
                                :split-position split-position
                                :split-axis axis-index
                                :l (build-tree patch :current-aabb l-aabb :axis-index next-axis
                                               :recursion-steps-left recursion-steps-left
                                               :triangles l-triangles)
                                :r (build-tree patch :current-aabb r-aabb :axis-index next-axis
                                               :recursion-steps-left recursion-steps-left
                                               :triangles r-triangles)))
               (make-instance 'node :l triangles))))
       (if triangles
           (make-instance 'node :l triangles)
           (dbg-msg 4 "ERROR: TRIANGLES-LIST is empty. Should never reach this logical branch.")))))
