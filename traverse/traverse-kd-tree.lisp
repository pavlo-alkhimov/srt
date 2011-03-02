(in-package #:kd)

(defparameter *kd-stack-length* 50)
(defparameter *nowhere* :no-object)

(defclass stack-element ()
  ((node :type kd-node
         :initform *default-nil-node*)
   (te :initform 0.0
       :type coordinate)
   (pb :type (simple-array coordinate (3))
       :initform (make-array 3
                             :element-type 'coordinate
                             :initial-element 0.0))
   (prev :initform 0
         :type index-type)))

(defun ray-box-intersect (ray aabb)
  (declare (ignore aabb)) 
  (values (aref ray 0 1)
          (- (random 1.0) 0.5)
          (- (random 1.0) 0.5)))

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
                     (safety 3)))
  (multiple-value-bind (intersection a b)
      (ray-box-intersect ray aabb)
    (break)
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
          ;; Init stack:
          (break)
          (dotimes (i *kd-stack-length*)
            (setf (aref stack i)
                  (make-instance 'stack-element)))
          (break)
          (setf (slot-value (aref stack enPt) 'te)
                a)
          (if (>= a 0.0)
              (set-pb enPt a)
              (set-pb enPt nil))
          (break)
          (setf (slot-value (aref stack exPt) 'te)
                b)
          (set-pb exPt b)
          (setf (slot-value (aref stack exPt) 'node)
                *nowhere*)
          (break)
          (iter outer-traverser
                (until (eq currNode *nowhere*))
                (break)
                (iter (until (is-leaf currNode))
                      (break)
                      (setf splitVal (slot-value currNode 'split-position))
                      (setf axis (slot-value currNode 'split-axis))
                      (setf nextAxis (next-axis axis))
                      (setf prevAxis (prev-axis axis))
                      
                      (if (<= (aref (slot-value (aref stack enPt) 'pb)
                                    axis)
                              splitVal)
                          (progn
                            (break)
                            (if (<= (aref (slot-value (aref stack exPt) 'pb)
                                          axis)
                                    splitVal)
                                (setf currNode (slot-value currNode 'left))
                                (next-iteration))
                            (if (= (aref (slot-value (aref stack exPt) 'pb)
                                         axis)
                                   splitVal)
                                (setf currNode (slot-value currNode 'right))
                                (next-iteration))
                            (setf farChild (slot-value currNode 'right))
                            (setf currNode (slot-value currNode 'left))
                            (break))
                          (progn
                            (break)
                            (if (< splitVal
                                   (aref (slot-value (aref stack exPt) 'pb)
                                         axis))
                                (setf currNode (slot-value currNode 'right))
                                (next-iteration))
                            (setf farChild (slot-value currNode 'left))
                            (setf currNode (slot-value currNode 'right))
                            (break)))
                      
                      (setf te (/ (- splitVal (aref ray 0 axis))
                                  (aref ray 1 axis)))
                      
                      (let ((tmp exPt))
                        (setf exPt (1+ exPt))
                        (if (= exPt enPt)
                            (setf exPt (1+ exPt)))
                        
                        (break)
                        
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
                        (break)
                        (if nil
                            (return-from outer-traverser :object-is-found))
                        
                        (setf enPt exPt)
                        
                        (setf currNode (slot-value (aref stack exPt) 'node))
                        (setf exPt (slot-value (aref stack enPt) 'prev)))
                      (break)
                      (return :no-object)))))))


;; (lisp-cabinet:load-quicklisp)

;; (ql:quickload "iterate")
;; (ql:quickload "cl-ppcre")
;; (ql:quickload "parse-number")

;; (in-package :iterate)
