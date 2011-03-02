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
                   (max-depth 4)
                   (triangles-list nil triangles-list-setp)
                   (triangles-count -1 triangles-count-setp)
                   (min-triangles-number 3))
  (declare (optimize (debug 3)))
  (if (and (< depth max-depth)
           (or (not triangles-count-setp)
               (< min-triangles-number triangles-count)))
      ;; try to recur
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

          (ifdebug 3 (format t "l: ~a, r: ~a~%" l-count r-count))
          
          ;; call recursively or put everything here
          (if (and (< 0 l-count)
                   (< 0 r-count)
                   (or (not triangles-count-setp)
                       (and (< l-count triangles-count)
                            (< r-count triangles-count))))
              ;; recursion
              (let ((next-axis (mod (1+ axis-index) 3))
                    (next-depth (1+ depth)))

                (ifdebug 3
                         (format t "DBG REC at #~a:~a [~a]=[~a]+[~a]~%"
                                 depth axis-index triangles-count l-count r-count)
                         (when (> (+ l-count r-count) triangles-count)
                           (format t "DBG At depth ~a: ~a + ~a -> intersect of ~a triangles~%"
                                   depth l-count r-count  (- (+ l-count r-count) triangles-count))))
                
                (make-instance 'kd-node :split-position split-position
                               :l (build-tree patch :aabb l-aabb :axis-index next-axis
                                              :depth next-depth :max-depth max-depth
                                              :triangles-list l-triangles :triangles-count l-count)
                               :r (build-tree patch :aabb r-aabb :axis-index next-axis
                                              :depth next-depth :max-depth max-depth
                                              :triangles-list r-triangles :triangles-count r-count)))
              ;; no recursion: put everything here
              (progn
                (ifdebug 3 (format t "DBG INT at #~a:~a [~a]=[~a]+[~a]~%"
                                   depth axis-index triangles-count l-count r-count))
                (make-instance 'kd-node :l triangles-list)))))
      ;; we do not recur. Put everything into this node or return nil.
      (if triangles-list
          (progn
            (ifdebug 3 (format t "DBG FIN at #~a:~a [~a]~%"
                               depth axis-index triangles-count))
            (make-instance 'kd-node :l triangles-list))
          nil)))

;; (defparameter *a* (obj:load-patch "d:/Paul.revised/git.repos/github/gourd.obj"))
;; (defparameter *b* (srt-kd:build-tree *a*))
;; (srt-kd:tree-statistics *b*)

(defun tree-statistics (tree &key (axis 0))
  (declare (type kd-node))
  (let ((next-axis (mod (1+ axis) 3)))
    (if (null tree)
        nil
        (if (and (null (r tree))
                 (l tree))
            (length (l tree))
            (list ;; (/ (round (* 100 (split-position tree))) 100.0)
             (case axis (0 :x) (1 :y) (2 :z))
             (tree-statistics (l tree) :axis next-axis)
             (tree-statistics (r tree) :axis next-axis))))))
