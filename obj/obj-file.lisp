(in-package :kd)

(defun remove-comment (line)
  (subseq line 0 (position #\# line)))

(defun parse-vertex (line)
  (destructuring-bind (label x y z &optional w)
      (cl-ppcre:split "\\s+" (remove-comment line))
    (declare (ignorable label w))
    (list (parse-number x)
          (parse-number y)
          (parse-number z))))

(defun parse-face-point (string)
  (destructuring-bind (vertex-index &optional texture-coordinate normal)
      (cl-ppcre:split "/" string)
    (declare (ignorable texture-coordinate normal))
    (parse-number vertex-index)))

(defun parse-face (line)
  (destructuring-bind (label &rest face-points)
      (cl-ppcre:split "\\s+" (remove-comment line))
    (declare (ignorable label))
    (map 'list #'parse-face-point face-points)))

(defun parse-obj-file (filespec)
  (with-open-file (in-stream filespec :direction :input)
    (loop for line = (read-line in-stream nil)
       while line
       when (cl-ppcre:scan "^v " line) collect (parse-vertex line) into vertices
       when (cl-ppcre:scan "^f " line) collect (parse-face line) into faces
       finally (return (list vertices faces)))))

(defun load-patch (filename)
  (let* ((data (parse-obj-file filename))
         (patch (make-instance 'tri-patch
                               :name filename
                               :given-vs (first data)
                               :given-is (second data))))
    patch))
