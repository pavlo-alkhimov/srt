(in-package #:kd)

(defparameter *kd-stack-length* 50) ;; sdfg sdfg sdfg
(defparameter *nowhere* :no-object)

(defclass stack-element ()
  ((node :type (or kd-node :no-object)
         :initform *default-nil-node*)
   (te :initform 0.0
       :type coordinate)
   (pb :type (simple-array coordinate (3))
       :initform (make-array 3
                             :element-type 'coordinate
                             :initial-element 0.0))
   (prev :initform 0
         :type index-type)))


(defun dump-stack (stack)
  (labels ((dump (element index)
             (if (not (eq *default-nil-node* (slot-value element 'node)))
                 (DBGMSG 3 "|  Stack-element[~a]: node=~a. T=~a Pb=~a Prev=~a"
                         index
                         (slot-value element 'node)
                         (slot-value element 'te)
                         (slot-value element 'pb)
                         (slot-value element 'prev))
                 (DBGMSG 3 "|  Stack-element[~a]: pointing to nowhere." index))))
    (DBGEXE 3
            (let ((prn nil))
              (DBGMSG 3
                      "+- Dumping the stack.")
              (iter (for i from (1- *kd-stack-length*) downto 0) 
                    (if (not (eq (slot-value (aref stack i) 'node)
                                 *default-nil-node*))
                        (setf prn t))
                    (when prn
                      (dump (aref stack i) i)))
              (DBGMSG 3 "+- The end of the stack.")))))

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

(defun ray-trav-alg-rec-b (root aabb ray)
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
               (enPt 0) (exPt 1)
               (splitVal 0.0)
               (axis 0)
               (nextAxis 0) (prevAxis 0)
               farChild
               (te 0.0))
          ;; Init stack:
          (dotimes (i *kd-stack-length*)
            (setf (aref stack i)
                  (make-instance 'stack-element)))
          
          (setf (slot-value (aref stack enPt) 'te)
                a)
          (if (>= a 0.0)
              (set-pb enPt a)
              (set-pb enPt))
          
          (setf (slot-value (aref stack exPt) 'te)
                b)
          (set-pb exPt b)
          (setf (slot-value (aref stack exPt) 'node)
                *nowhere*)
          
          (dump-stack stack)
          ;; (return-from ray-trav-alg-rec-b :no-object)
          
          (let ((endless-loop-protector 0))
            (iter outer-traverser
                  (until (eq currNode *nowhere*))
                  
                  (iter (until (is-leaf currNode))
                        
                        (setf splitVal (slot-value currNode 'split-position))
                        (setf axis (slot-value currNode 'split-axis))
                        (setf nextAxis (next-axis axis))
                        (setf prevAxis (prev-axis axis))
                        
                        (dump-stack stack)
                        (incf endless-loop-protector)
                        (when (> endless-loop-protector 10)
                          (return-from ray-trav-alg-rec-b :endless-loop-protector-fired))
                        
                        #|(break "currNode=~a (~:[node~;leaf~]) axis=~a prev=~a next=~a"
                               currNode
                               (is-leaf currNode)
                               axis
                               prevAxis
                               nextAxis)|#
                        
                        (if (<= (aref (slot-value (aref stack enPt) 'pb)
                                      axis)
                                splitVal)
                            (progn
                              (if (<= (aref (slot-value (aref stack exPt) 'pb)
                                            axis)
                                      splitVal)
                                  (progn
                                    (if (equalp currNode (slot-value currNode 'left))
                                        (break "WILL FAIL!!!"))
                                    (setf currNode (slot-value currNode 'left))
                                    (next-iteration)))
                              ;; (break "passed 1")
                              (if (= (aref (slot-value (aref stack exPt) 'pb)
                                           axis)
                                     splitVal)
                                  (progn
                                    (if (equalp currNode (slot-value currNode 'right))
                                        (break "WILL FAIL!!!"))
                                    (setf currNode (slot-value currNode 'right))
                                    (next-iteration)))
                              ;; (break "passed 2")
                              (setf farChild (slot-value currNode 'right))
                              (setf currNode (slot-value currNode 'left)))
                            
                            (progn 
                              ;; (break "1if 2 a")
                              (if (< splitVal
                                     (aref (slot-value (aref stack exPt) 'pb)
                                           axis))
                                  (progn
                                    (if (equalp currNode (slot-value currNode 'right))
                                        (break "WILL FAIL!!!"))
                                    (setf currNode (slot-value currNode 'right))
                                    (next-iteration)))                            
                              ;; (break "1if 2 b")
                              (setf farChild (slot-value currNode 'left))
                              (setf currNode (slot-value currNode 'right))))
                        ;; (break "after if")
                        (setf te (/ (- splitVal (aref ray 0 axis))
                                    (aref ray 1 axis)))
                        
                        (let ((tmp exPt))
                          (setf exPt (1+ exPt))
                          (if (= exPt enPt)
                              (setf exPt (1+ exPt)))
                          
                          (with-slots ((prev1 prev) (te1 te) (node1 node) (pb1 pb))
                              (aref stack exPt)
                            (setf prev1 tmp)
                            (setf te1 te)
                            (setf node1 farChild)
                            (setf (aref pb1 axis) splitVal)
                            (setf (aref pb1 nextAxis)
                                  (+ (aref ray 0 nextAxis) (* te (aref ray 1 nextAxis))))
                            (setf (aref pb1 prevAxis)
                                  (+ (aref ray 0 prevAxis) (* te (aref ray 1 prevAxis)))))
                          
                          ;; intersect ray with each object in the object list, discarding
                          ;; those lying before stack[enPt].t or farther than stack[exPt].t
                          
                          (if nil
                              (return-from outer-traverser :object-is-found))
                          
                          (princ "after return.")
                          
                          (setf enPt exPt)
                          
                          (setf currNode (slot-value (aref stack exPt) 'node))
                          (setf exPt (slot-value (aref stack enPt) 'prev)))
                        
                        (princ "after iter.")
                        
                        (return-from ray-trav-alg-rec-b :no-object))))))))


;; (lisp-cabinet:load-quicklisp)

;; (ql:quickload "iterate")
;; (ql:quickload "cl-ppcre")
;; (ql:quickload "parse-number")

;; (in-package :iterate)
