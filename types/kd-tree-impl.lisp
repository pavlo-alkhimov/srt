(in-package #:kd)


(defun try-triangle (patch triangle axis-index split-position predicate)
  (iter (for i from 0 to 2)
        (thereis (funcall predicate
                          (get-coord-by-indexes patch triangle i axis-index)
                          split-position))))

(defun-with-dbg build-tree (patch &key (triangles nil triangles-setp)
                                  (current-aabb nil current-aabb-setp)
                                  (axis-index 0)
                                  (recursion-steps-left 1)
                                  (min-triangles-count 3))
  (declare (optimize (debug 3))
           (type (or null aabb) current-aabb))

  (dump-many 4 ((dump patch)
                (dump recursion-steps-left)
                (dump current-aabb)
                (dump (patch-aabb patch))))
  
  (if (< 0 recursion-steps-left)
      (with-dbg 4 (("RECURSION. recursion-steps-left: ~a" recursion-steps-left))
                (let* ((triangles (if (not triangles-setp)
                                      (iter (for x from 0 below (array-dimension (patch-is patch) 0))
                                            (collect x))
                                      triangles))
                       (triangles-count (length triangles))
                       
                       (current-aabb (if current-aabb-setp
                                         current-aabb
                                         (patch-aabb patch)))
                       (split-position (old-sah patch current-aabb axis-index triangles))
                       l-triangles
                       r-triangles)
                  
                  (with-dbg 4 ((dump triangles-count triangles))
                            (iter (for i in triangles)
                                  (when (try-triangle patch i axis-index split-position #'<)
                                    (push i l-triangles)) ;; triangle touches left
                                  (when (try-triangle patch i axis-index split-position #'>)
                                    (push i r-triangles)) ;; triangle touches right
                                  (when (not (try-triangle patch i axis-index split-position #'/=))
                                    (push i l-triangles)))) ;; triangle is in-plane
                  
                  (multiple-value-bind (l-aabb r-aabb) (split-aabb current-aabb axis-index split-position)
                    
                    (dump-many 4 (("Split ~a triangles in two groups" triangles-count)
                                  (dump (length l-triangles) (length r-triangles))
                                  (dump split-position)))
                    
                    (if (and (< min-triangles-count (length l-triangles) (length triangles))
                             (< min-triangles-count (length r-triangles) (length triangles)))
                        (let ((next-axis (mod (1+ axis-index) 3))
                              (recursion-steps-left (1- recursion-steps-left)))
                          (DBG-MSG 4 "Recursion.")
                          (make-instance 'node
                                         :split-position split-position
                                         :split-axis axis-index
                                         :l (build-tree patch :current-aabb l-aabb :axis-index next-axis
                                                        :recursion-steps-left recursion-steps-left
                                                        :triangles l-triangles)
                                         :r (build-tree patch :current-aabb r-aabb :axis-index next-axis
                                                        :recursion-steps-left recursion-steps-left
                                                        :triangles r-triangles)))
                        (with-dbg 4 (("The leaf is created."))
                                  (make-instance 'node :l triangles))))))
      (with-dbg 4 (("NO RECURSION. recursion-steps-left: ~a" recursion-steps-left))
        (if triangles
            (with-dbg 4 (("The leaf is created."))
                      (make-instance 'node :l triangles))
            (dbg-msg 4 "ERROR: TRIANGLES-LIST is empty. Should never reach this logical branch.")))))

(defun-with-dbg tree-statistics (tree &key (axis 0))
  (declare (type node tree))
  (let ((next-axis (mod (1+ axis) 3)))
    (if (null tree)
        nil
        (if (is-leaf tree)
            (length (node-left tree))
            (list
             (node-axis tree)
             (tree-statistics (node-left  tree) :axis next-axis)
             (tree-statistics (node-right tree) :axis next-axis))))))