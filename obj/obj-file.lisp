(in-package :kd)

(defun remove-comment (line)
  (subseq line 0 (position #\# line)))

(defun parse-vertex (line)
  (destructuring-bind (label x y z &optional w)
      (cl-ppcre:split "\\s+" (remove-comment line))
    (declare (ignorable label w))
    (list (coerce (parse-number x) 'coordinate)
          (coerce (parse-number y) 'coordinate)
          (coerce (parse-number z) 'coordinate))))

(defun parse-face-point (string)
  (destructuring-bind (vertex-index &optional texture-coordinate normal)
      (cl-ppcre:split "/" string)
    (declare (ignorable texture-coordinate normal))
    (list (1- (parse-number vertex-index))
          (when (and texture-coordinate
                     (< 0 (length texture-coordinate)))
            (1- (parse-number texture-coordinate)))
          (when (and normal
                     (< 0 (length normal)))
            (1- (parse-number normal))))))

(defun parse-face (line)
  (destructuring-bind (label &rest face-points)
      (cl-ppcre:split "\\s+" (remove-comment line))
    (declare (ignorable label))
    (map 'list #'(lambda (x) (parse-face-point x)) face-points)))

(defun parse-obj-file (filespec)
  "I utilize only V, VN and F lines. Others are to be added eventually."
  (macrolet ((eat (string function container)
               `(when (cl-ppcre:scan ,string line)
                  (collect (,function line) into ,container))))
    (iter (for line in-file filespec using #'read-line)
          (eat "^v " parse-vertex vertices)
          (eat "^vn " parse-vertex normals)
          (eat "^f " parse-face faces)
          (finally (return (list vertices normals faces))))))

(defun-with-dbg load-patch (filename)
  (let* ((data (parse-obj-file filename))
         (*print-lines* 2))
    (with-dbg 6 ((dump (nth 0 data)) 
                 (dump (nth 1 data))
                 (dump (nth 2 data)))
              (make-instance 'patch
                             :name filename
                             :given-vs (nth 0 data)
                             :given-ns (nth 1 data)
                             :given-fs (nth 2 data)))))
