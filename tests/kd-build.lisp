(in-package #:kd)

(with-dbg-block "Build test"
  (with-dbg-level 3
    (let* ((name "lisp/development/srt/data/dodecahedron.obj")
           (p (load-patch name))
           (result (build-tree p)))
      (if (and p
               result)
          (dbg-msg 3 "Passed with ~a." result)
          (if p
              (dbg-msg 3 "Failed on the patch loaded from the ~a" name)
              (dbg-msg 3 "Patch ~a could not be loaded, test not applicable." name))))))

