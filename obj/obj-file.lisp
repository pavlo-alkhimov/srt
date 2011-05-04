(in-package :kd)

(defun remove-comment (line)
  (subseq line 0 (position #\# line)))

(defun parse-vertex (line given-type)
  (destructuring-bind (label x y z &optional w)
      (cl-ppcre:split "\\s+" (remove-comment line))
    (declare (ignorable label w))
    (list (coerce (parse-number x) given-type)
          (coerce (parse-number y) given-type)
          (coerce (parse-number z) given-type))))

(defun parse-face-point (string)
  (destructuring-bind (vertex-index &optional texture-coordinate normal)
      (cl-ppcre:split "/" string)
    (declare (ignorable texture-coordinate normal))
    (parse-number vertex-index)))

(defun parse-face (line)
  (destructuring-bind (label &rest face-points)
      (cl-ppcre:split "\\s+" (remove-comment line))
    (declare (ignorable label))
    (map 'list #'(lambda (x) (1- (parse-face-point x))) face-points)))

(defun parse-obj-file (filespec)
  (with-open-file (in-stream filespec :direction :input)
    (loop for line = (read-line in-stream nil)
       while line
       when (cl-ppcre:scan "^v " line) collect (parse-vertex line 'coordinate) into vertices
       when (cl-ppcre:scan "^f " line) collect (parse-face line) into faces
       finally (return (list vertices faces)))))

(defun load-patch (filename)
  (let* ((data (parse-obj-file filename))
         (p (make-instance 'patch
                            :name filename
                            :given-vs (first data)
                            :given-is (second data))))
    p))
