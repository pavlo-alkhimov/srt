(in-package #:kd)

(defparameter *test-patch* (load-patch "lisp/development/srt/data/gourd.obj"))
(defparameter *test-tree* (build-tree *test-patch*))
(setf (slot-value *test-patch* 'kd-tree-root) *test-tree*)



(ray-trav-alg-rec-b (slot-value *test-patch* 'kd-tree-root)
                    (slot-value *test-patch* 'aabb)
                    (make-array '(2 3) :initial-contents '((0.0 0.0 0.0)(1.0 2.0 3.0))))