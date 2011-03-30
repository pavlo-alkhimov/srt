(in-package #:kd)


(defun try-triangle (patch triangle axis-index split-position predicate)
  (iter (for i from 0 to 2)
        (thereis (funcall predicate
                          (get-coord-by-indexes patch triangle i axis-index)
                          split-position))))

(defstruct triangle-cached-ref
  (index 0 :type index-type)
  (square 0.0 :type coordinate))

(defun-with-dbg build-tree (patch &key (triangles nil triangles-setp)
                                  (current-aabb nil current-aabb-setp)
                                  (axis-index 0)
                                  (recursion-steps-left 1)
                                  (min-triangles-count 3))
  (declare (optimize (debug 3))
           (type aabb current-aabb))
  
  (with-dbg 4 ((dump patch)
               (dump recursion-steps-left)
               (dump current-aabb)
               (dump (slot-value patch 'aabb)))
            (if (< 0 recursion-steps-left)
                (with-dbg 4 (("recursion-steps-left: ~a. Trying to recur." recursion-steps-left))
                          (let* ((current-aabb (if current-aabb-setp
                                                   current-aabb
                                                   (slot-value patch 'aabb)))
                                 
                                 (split-position (sah patch
                                                      current-aabb
                                                      axis-index
                                                      nil))
                                 (l-triangles nil)
                                 (r-triangles nil)
                                 (triangles (if (not triangles-setp)
                                                (iter (for x from 0 below (array-dimension (i patch) 0))
                                                      (collect x))
                                                triangles))
                                 (triangles-count (length triangles)))
                            (iter (for i in triangles)
                                  (when (try-triangle patch i axis-index split-position #'<)
                                    (push i l-triangles)) ;; triangle touches left
                                  (when (try-triangle patch i axis-index split-position #'>)
                                    (push i r-triangles)) ;; triangle touches right
                                  (when (not (try-triangle patch i axis-index split-position #'/=))
                                    (push i l-triangles))) ;; triangle is in-plane
                            (multiple-value-bind (l-aabb r-aabb)
                                (split-aabb current-aabb axis-index split-position)
                              (with-dbg 4 (("Split ~a triangles in two groups" triangles-count)
                                           (dump (length l-triangles)
                                                 (length r-triangles))
                                           (dump split-position))
                                        (if (and (< 0 (length l-triangles) (length triangles))
                                                 (< 0 (length r-triangles) (length triangles)))
                                            (let ((next-axis (mod (1+ axis-index) 3))
                                                  (recursion-steps-left (1- recursion-steps-left)))
                                              (with-dbg 4 (("Recursion."))
                                                        (make-instance 'kd-node
                                                                       :split-position split-position
                                                                       :split-axis axis-index
                                                                       :l (build-tree patch :current-aabb l-aabb :axis-index next-axis
                                                                                      :recursion-steps-left recursion-steps-left
                                                                                      :triangles l-triangles)
                                                                       :r (build-tree patch :current-aabb r-aabb :axis-index next-axis
                                                                                      :recursion-steps-left recursion-steps-left
                                                                                      :triangles r-triangles))))
                                            (with-dbg 4 (("The leaf is created."))
                                                      (make-instance 'kd-node :l triangles)))))))
                (with-dbg 4
                  (("recursion-steps-left: ~a. Not entering build-tree splitter." recursion-steps-left))
                  (if triangles
                      (with-dbg 4 (("The leaf is created."))
                                (make-instance 'kd-node :l triangles))
                      (with-dbg 4 (("triangles-list: nil."))
                                nil))))))

(defun-with-dbg tree-statistics (tree &key (axis 0))
  (declare (type kd-node))
  (let ((next-axis (mod (1+ axis) 3)))
    (if (null tree)
        nil
        (if (and (null (r tree))
                 (l tree))
            (length (l tree))
            (list
             (a tree)
             (tree-statistics (l tree) :axis next-axis)
             (tree-statistics (r tree) :axis next-axis))))))