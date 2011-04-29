(in-package #:kd)

(defparameter *test-patch* nil)

(with-dbg-block "[TEST] Traverse"
  
  (set-dbg-level 4)
  (defparameter *file-name* "lisp/development/srt/data/tetrahedron.obj")
  (defparameter *file-name* "lisp/development/srt/data/dodecahedron.obj")
  
  (setf *test-patch* (load-patch *file-name*))
  (dbg-msg 1 "Result: ~a" *test-patch*)
  
  ;; (defparameter *test-tree* (build-tree *test-patch*))
  ;; (setf (slot-value *test-patch* 'kd-tree-root) *test-tree*)
  
  
  
  ;; (with-dbg-block "[TEST] Traversing..."
  ;;   (let ((result (ray-trav-alg-rec-b (slot-value *test-patch* 'kd-tree-root)
  ;;                                     (slot-value *test-patch* 'aabb)
  ;;                                     (make-array '(2 3) :initial-contents '((0.0 0.0 0.0)(1.0 2.0 3.0))))))
  ;;     (dbg-msg 1 "Traversing result: ~a" result)))
  )