(in-package #:kd)

(with-dbg-block "[TEST] Traverse"
  
  (set-dbg-level 4)
  
  (defparameter *test-patch* (load-patch "lisp/development/srt/data/gourd.obj"))
  (defparameter *test-tree* (build-tree *test-patch* :recursion-steps-left 2))
  (setf (slot-value *test-patch* 'kd-tree-root) *test-tree*)
  
  (dbg-msg 1 "build-tree produced the structure: ~a"
           (tree-statistics *test-tree*))
  
  #|(with-dbg-block "[TEST] Traversing..."
  (let ((result (ray-trav-alg-rec-b (slot-value *test-patch* 'kd-tree-root)
  (slot-value *test-patch* 'aabb)
  (make-array '(2 3) :initial-contents '((0.0 0.0 0.0)(1.0 2.0 3.0))))))
  (dbg-msg 1 "Traversing result: ~a" result)))|#)