(in-package #:kd)

(let ((result (tree-statistics
               (build-tree
                (load-patch
                 "d:/Paul.revised/git.repos/github/srt/data/gourd.obj")))))
  (if result
      (format t "\"BUILD KD TREE\" test passed resulting with structure ~a.~%" result)
   (error "Test failed: BUILD KD TREE")))
