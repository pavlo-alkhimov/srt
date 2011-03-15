(in-package #:kd)

(let ((result
       (tree-statistics
        (build-tree
         (load-patch
          "lisp/development/srt/data/gourd.obj")))))
  (if result
      (DBGMSG 3 "\"BUILD KD TREE\" test passed resulting with structure ~a." result)
      (error "Test failed: BUILD KD TREE")))

