(in-package #:kd)

(with-dbg-block "[TEST] Build KD-tree"
  (with-dbg-level 4
    (let ((result (tree-statistics (build-tree (load-patch "lisp/development/srt/data/tetrahedron.obj")))))
      (if result
          (dbg-msg 3 "\"BUILD KD TREE\" test passed resulting with structure ~a." result)
          (dbg-msg 3 "Test failed: BUILD KD TREE")))))

