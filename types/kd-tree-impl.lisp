(in-package #:kd)

(defun-with-dbg build-tree (patch &key (triangles nil triangles-setp)
                                  (current-aabb nil current-aabb-setp)
                                  (axis-index 0)
                                  (recursion-steps-left 3)
                                  (min-triangles-count 3))
  (declare (optimize (debug 3))
           (type (or null aabb) current-aabb))
  
  (if (< 0 recursion-steps-left)
      (with-dbg-header
          6 (("Recurring. (~a levels left)" recursion-steps-left))
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
            (dbg-msg 6 "Separate triangles to left and right branches.")
            (flet ((triangle-is (predicate triangle)
                     (iter (for i from 0 to 2)
                           (thereis (funcall predicate
                                             (get-coord-by-indexes patch triangle i axis-index)
                                             split-position)))))
              (iter (for i in triangles)
                    (when (triangle-is #'< i) (push i l-triangles))
                    (when (triangle-is #'> i) (push i r-triangles))
                    (when (not (triangle-is #'/= i)) (push i l-triangles))))
            (dbg-msg 6 "Create the rest of the tree under this node.")
            (multiple-value-bind (l-aabb r-aabb) (split-aabb current-aabb axis-index split-position)
              (if (and (< min-triangles-count (length l-triangles) (length triangles))
                       (< min-triangles-count (length r-triangles) (length triangles)))
                  (with-dbg-header
                      6 (("Create the node recurring during the creation."))
                      (let ((next-axis (mod (1+ axis-index) 3))
                            (recursion-steps-left (1- recursion-steps-left)))
                        (make-instance 'node
                                       :split-position split-position
                                       :split-axis axis-index
                                       :l (with-dbg-header
                                              5 (("Creating left branch with ~r level~:p left." recursion-steps-left))
                                              (build-tree patch :current-aabb l-aabb :axis-index next-axis
                                                          :recursion-steps-left recursion-steps-left
                                                          :triangles l-triangles))
                                       :r (with-dbg-header
                                              5 (("Creating right branch with ~r level~:p left." recursion-steps-left))
                                              (build-tree patch :current-aabb r-aabb :axis-index next-axis
                                                          :recursion-steps-left recursion-steps-left
                                                          :triangles r-triangles)))))
                  (with-dbg-header 5 (("Create the leaf without any recursion."))
                                   (make-instance 'node :l triangles))))))
      (with-dbg-header 5 (("Recursion ended, creating the leaf with ~a triangles." (length triangles)))
                       (if triangles
                           (make-instance 'node :l triangles)
                           (dbg-msg 0 "ERROR: TRIANGLES-LIST is empty. Should never reach this logical branch.")))))
