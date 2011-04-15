(in-package #:kd)

(with-dbg-block "TEST1"
  (with-dbg-level 4
    (let ((result (build-tree (load-patch
                               "lisp/development/srt/data/dodecahedron.obj")
                              :recursion-steps-left 3)))
      (if result
          (dbg-msg 3 "Passed with result ~a." result)
          (dbg-msg 3 "Failed.")))))

