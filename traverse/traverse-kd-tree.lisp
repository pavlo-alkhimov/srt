(in-package #:kd)

(defparameter *kd-stack-length* 50)
(defparameter *nowhere* :no-object)
(defparameter *default-nil-node* (make-instance 'node))

(defclass stack-element ()
  ((node :type (or node :no-object)
         :initform *default-nil-node*)
   (te :initform 0.0
       :type coordinate)
   (pb :type (simple-array coordinate (3))
       :initform (make-array 3
                             :element-type 'coordinate
                             :initial-element 0.0))
   (prev :initform 0
         :type index-type)))

(defun dump-stack (level stack)
  (flet ((dump (l element index)
           (if (not (eq *default-nil-node* (slot-value element 'node)))
               (DBG-MSG l "Stack-element[~a]: node=~a. t=~,3f Pb=~a Prev=~a"
                        index
                        (slot-value element 'node)
                        (slot-value element 'te)
                        (point->string (slot-value element 'pb))
                        (slot-value element 'prev))
               (DBG-MSG l "Stack-element[~a]: pointing to nowhere." index))))
    (DBGEXE level
            (let ((prn nil))
              (iter (for i from (1- *kd-stack-length*) downto 0) 
                    (if prn
                        (dump level (aref stack i) i)
                        (if (not (eq (slot-value (aref stack i) 'node)
                                     *default-nil-node*))
                            (setf prn t))))))))

(defun ray-box-intersect (ray aabb)
  (declare (ignore aabb))
  (let* ((aaa (random 0.5))
         (bbb (+ aaa (random 0.5)))) 
    (return-from ray-box-intersect
      (values (aref ray 0 1)
              aaa bbb))))

(defmacro set-pb (place &optional amount)
  (if amount
      `(dotimes (i 3)
         (setf (aref (slot-value (aref stack ,place) 'pb) i)
               (+ (aref ray 0 i)
                  (* (aref ray 1 i)
                     ,amount))))
      `(dotimes (i 3)
         (setf (aref (slot-value (aref stack ,place) 'pb) i)
               (aref ray 0 i)))))

(defun-with-dbg ray-trav-alg-rec-b (root aabb ray)
  "Recursive ray traversal algorithm."
  (declare (optimize (debug 3)
                     (safety 3)
                     (speed 0))) 
  (multiple-value-bind (intersection a b)
      (ray-box-intersect ray aabb)
    ;; (break "~a" (list intersection a b))
    (if (not intersection)
        *nowhere*
        (let* ((stack (make-array *kd-stack-length*))
               (currNode root)
               (enPt 0)
               (exPt 1)
               (splitVal 0.0)
               (axis 0)
               (nextAxis 0)
               (prevAxis 0)
               farChild
               (te 0.0))
          
          (DBG-MSG 3 "Start from the KD-tree root node")
          (DBG-MSG 3 "Setting entry point enPt: ~a" enPt)
          (DBG-MSG 3 "Setting exit point exPt: ~a" exPt)
          
          (dotimes (i *kd-stack-length*)
            (setf (aref stack i)
                  (make-instance 'stack-element)))
          
          (setf (slot-value (aref stack enPt) 'te) a)
          (if (>= a 0.0)
              (set-pb enPt a)
              (set-pb enPt))
          
          (setf (slot-value (aref stack exPt) 'te) b)
          (set-pb exPt b)
          (setf (slot-value (aref stack exPt) 'node) *nowhere*)
          
          (let ((endless-loop-protector 1))
            (DBG-MSG 3 "Entering outer loop")
            (iter outer-traverser
                  (until (eq currNode *nowhere*))
                  (DBG-MSG 3 "Entering inner loop")
                  (iter
                    (until (is-leaf currNode))
                    
                    (setf splitVal (slot-value currNode 'split-position))
                    (setf axis (slot-value currNode 'split-axis))
                    (setf nextAxis (next-axis axis))
                    (setf prevAxis (prev-axis axis))
                    
                    (DBG-MSG 3 "Starting the ~:r iteration" endless-loop-protector)
                    (DBG-MSG 3 "currNode: ~a" currNode)
                    (DBG-MSG 3 "splitVal: ~,3f" splitVal)
                    (DBG-MSG 3 "axis: ~[X~;Y~;Z~]" axis)
                    (dump-stack 3 stack)
                    (incf endless-loop-protector)
                    (when (> endless-loop-protector 10)
                      (return-from ray-trav-alg-rec-b :endless-loop-protector-fired))
                    
                    (if (>= splitVal
                            (aref (slot-value (aref stack enPt) 'pb)
                                  axis))
                        (progn
                          (when (>= splitVal
                                    (aref (slot-value (aref stack exPt) 'pb)
                                          axis))
                            (progn
                              (DBG-MSG 3 "Case N1 N2 N3 P5 Z2 and Z3")
                              (setf currNode (slot-value currNode 'left))
                              (next-iteration)))
                          (when (= splitVal
                                   (aref (slot-value (aref stack exPt) 'pb)
                                         axis))
                            (progn
                              (DBG-MSG 3 "Case Z1")
                              (setf currNode (slot-value currNode 'right))
                              (next-iteration)))
                          (DBG-MSG 3 "Case N4")
                          (setf farChild (slot-value currNode 'right))
                          (setf currNode (slot-value currNode 'left)))
                        (progn
                          (when (< splitVal
                                   (aref (slot-value (aref stack exPt) 'pb)
                                         axis))
                            (progn
                              (DBG-MSG 3 "Case P1, P2, P3 and N5")
                              (setf currNode (slot-value currNode 'right))
                              (next-iteration)))                            
                          (DBG-MSG 3 "Case P4")
                          (setf farChild (slot-value currNode 'left))
                          (setf currNode (slot-value currNode 'right))))
                    (DBG-MSG 3 "Case P4 or N4: traverse both children")
                    (setf te (/ (- splitVal
                                   (aref ray 0 axis))
                                (aref ray 1 axis)))
                    (DBG-MSG 3 "Signed distance to the splitting plane: ~,3f." te)
                    (let ((tmp exPt))
                      (setf exPt (1+ exPt))
                      ;; possibly skip current entry point so not to overwrite the data
                      (if (= exPt enPt)
                          (setf exPt (1+ exPt)))
                      (DBG-MSG 3 "Setup the new exit point exPt: ~a->~a" tmp exPt)
                      (with-slots ((prev1 prev) (te1 te) (node1 node) (pb1 pb))
                          (aref stack exPt)
                        (DBG-MSG 3 "Pushing values onto the stack")
                        (setf prev1 tmp)
                        (setf te1 te)
                        (setf node1 farChild)
                        (setf (aref pb1 axis) splitVal)
                        (setf (aref pb1 nextAxis)
                              (+ (aref ray 0 nextAxis) (* te (aref ray 1 nextAxis))))
                        (setf (aref pb1 prevAxis)
                              (+ (aref ray 0 prevAxis) (* te (aref ray 1 prevAxis)))))))
                  
                  (DBG-MSG 3 "Intersect ray with each object in the object list, discarding")
                  (DBG-MSG 3 "those lying before stack[enPt].t or farther than stack[exPt].t")
                  
                  (if nil
                      (return-from outer-traverser :object-is-found))
                  
                  (setf enPt exPt)
                  (DBG-MSG 3 "Pop the element from the stack. enPt: ~a" enPt)
                  
                  (setf currNode (slot-value (aref stack exPt) 'node))
                  (setf exPt (slot-value (aref stack enPt) 'prev))
                  (DBG-MSG 3 "Retrieve pointer to the next node: ~a. exPt: ~a."
                           (if (eq currNode *default-nil-node*)
                               "nowhere"
                               currNode)
                           exPt)
                  
                  (DBG-MSG 3 "Ray leaves the scene."))
            (return-from ray-trav-alg-rec-b :no-object))))))
