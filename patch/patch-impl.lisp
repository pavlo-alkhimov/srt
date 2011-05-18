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
  (format stream "#<patch \"~a\" V=~a N=~a [~a: T"
          ;; (subseq (name obj)
          ;;         (1+ (or (position #\/ (name obj) :from-end t) -1))
          ;;         (or (position #\. (name obj) :from-end t)
          ;;             (length (name obj))))
          (name obj)
          (array-dimension (vertexes obj) 0)
          (array-dimension (normals obj) 0)
          (array-dimension (triangles-indexes-array obj) 0))
  (when (< 0 (array-dimension (normals-indexes-array obj) 0))
    (format stream " N"))
  (when (< 0 (array-dimension (texture-coords-indexes-array obj) 0))
    (format stream " TX"))
  (when (< 0 (array-dimension (squares obj) 0))
    (format stream " S"))
  (format stream "]>"))

(defmethod initialize-instance :after ((p patch)
                                       &key name
                                       given-vs given-ns given-fs)  
  (setf (name p)
        (subseq name
                (1+ (or (position #\/ name :from-end t) -1))
                (or (position #\. name :from-end t)
                    (length name))))
  
  (setf (vertexes p) (make-array (list (length given-vs) 3)
                                 :element-type 'coordinate
                                 :initial-contents given-vs))
  (setf (normals p) (make-array (list (length given-ns) 3)
                                :element-type 'coordinate
                                :initial-contents given-ns))
  (setf (faces-list p) given-fs)
  
  (let* ((results (list nil nil nil nil)))
    (dolist (face given-fs)
      (dotimes (index (- (length face) 2))
        (dotimes (i 3)
          (let ((a (nth i (nth 0           face)))
                (b (nth i (nth (+ index 1) face)))
                (c (nth i (nth (+ index 2) face))))
            (and a b c
                 (push (list a b c)
                       (nth i results)))
            (when (= i 0)
              (let* ((dxab (- (aref (vertexes p) b 0) (aref (vertexes p) a 0)))
                     (dxbc (- (aref (vertexes p) c 0) (aref (vertexes p) b 0)))
                     (dxac (- (aref (vertexes p) c 0) (aref (vertexes p) a 0)))
                     
                     (dyab (- (aref (vertexes p) b 1) (aref (vertexes p) a 1)))
                     (dybc (- (aref (vertexes p) c 1) (aref (vertexes p) b 1)))
                     (dyac (- (aref (vertexes p) c 1) (aref (vertexes p) a 1)))
                     
                     (dzab (- (aref (vertexes p) b 2) (aref (vertexes p) a 2)))
                     (dzbc (- (aref (vertexes p) c 2) (aref (vertexes p) b 2)))
                     (dzac (- (aref (vertexes p) c 2) (aref (vertexes p) a 2)))
                     
                     (ab (sqrt (+ (* dxab dxab) (* dyab dyab) (* dzab dzab))))
                     (bc (sqrt (+ (* dxbc dxbc) (* dybc dybc) (* dzbc dzbc))))
                     (ac (sqrt (+ (* dxac dxac) (* dyac dyac) (* dzac dzac))))
                     
                     (p/2 (* 0.5 (+ ab bc ac)))
                     (s (sqrt (* p/2 (- p/2 ab) (- p/2 bc) (- p/2 ac)))))
                
                (push s (nth 3 results))))))))
    
    (and (nth 0 results)
         (setf (triangles-indexes-array p)
               (make-array (list (length (nth 0 results)) 3)
                           :element-type 'index-type
                           :initial-contents (nth 0 results))))
    (and (nth 1 results)
         (setf (texture-coords-indexes-array p)
               (make-array (list (length (nth 1 results)) 3)
                           :element-type 'index-type
                           :initial-contents (nth 1 results))))
    (and (nth 2 results)
         (setf (normals-indexes-array p)
               (make-array (list (length (nth 2 results)) 3)
                           :element-type 'index-type
                           :initial-contents (nth 2 results))))
    (and (nth 3 results)
         (setf (squares p)
               (make-array (length (nth 3 results))
                           :element-type 'coordinate
                           :initial-contents (nth 3 results)))))
  (setf (aabb p) (calc-aabb (vertexes p)))
  (setf (kd-tree p) (build-tree p))
  
  (dump-many 6 (("Patch \"~a\" initialized with T=~a N=~a TX=~a"
                 (name p)
                 (array-dimension (triangles-indexes-array p) 0)
                 (array-dimension (normals-indexes-array p) 0)
                 (array-dimension (texture-coords-indexes-array p) 0)))))