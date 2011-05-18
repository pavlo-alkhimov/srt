(in-package #:kd)

(defun get-coord-by-indexes (patch triangle vertex axis)
  (aref (vertexes patch)
        (aref (triangles-indexes-array patch)
              triangle
              vertex)
        axis))
(defun get-normal-by-indexes (patch triangle vertex axis)
  (aref (normals patch)
        (aref (normals-indexes-array patch)
              triangle
              vertex)
        axis))

(defmethod print-object ((obj patch) stream)
  (format stream "#<patch \"~a\" V[~a] N[~a] Tri[~a]/Norm[~a]/Tex[~a]>"
          (subseq (name obj)
                  (1+ (or (position #\/ (name obj) :from-end t) -1))
                  (or (position #\. (name obj) :from-end t)
                      (length (name obj))))
          (array-dimension (vertexes obj) 0)
          (array-dimension (normals obj) 0)
          (array-dimension (triangles-indexes-array obj) 0)
          (array-dimension (normals-indexes-array obj) 0)
          (array-dimension (texture-coords-indexes-array obj) 0)))

(defmethod initialize-instance :after ((p patch)
                                       &key name
                                       given-vs given-ns given-fs)

  (setf (vertexes p)
        (make-array (list (length given-vs) 3)
                    :element-type 'coordinate
                    :initial-contents given-vs))
  (setf (normals p)
        (make-array (list (length given-ns) 3)
                    :element-type 'coordinate
                    :initial-contents given-ns))
  (setf (name p)
        name)

  (setf (faces-list p)
        given-fs)

  (let ((counts (list 0 0 0)))
    (dolist (face given-fs)
      (dotimes (index (- (length face) 2))
        (dotimes (i 3)
          (let ((a (nth i (nth 0           face)))
                (b (nth i (nth (+ index 1) face)))
                (c (nth i (nth (+ index 2) face))))
            (and a b c
                 (incf (nth i counts))))))))
  
  (let* ((results (list nil nil nil)))
    (dolist (face given-fs)
      (dotimes (index (- (length face) 2))
        (dotimes (i 3)
          (let ((a (nth i (nth 0           face)))
                (b (nth i (nth (+ index 1) face)))
                (c (nth i (nth (+ index 2) face))))
            (and a b c
                 (push (list a b c)
                       (nth i results)))))))

    (and (nth 0 results)
         (setf (triangles-indexes-array p)
               (make-array (list (length (nth 0 results)) 3)
                           :element-type 'index-type
                           :initial-contents (nth 0 results))))

    (and (nth 2 results)
         (setf (normals-indexes-array p)
               (make-array (list (length (nth 2 results)) 3)
                           :element-type 'index-type
                           :initial-contents (nth 2 results))))

    (and (nth 1 results)
         (setf (texture-coords-indexes-array p)
               (make-array (list (length (nth 1 results)) 3)
                           :element-type 'index-type
                           :initial-contents (nth 1 results)))))

  (setf (aabb p) (calc-aabb (vertexes p)))
  (setf (kd-tree p) (build-tree p)))



