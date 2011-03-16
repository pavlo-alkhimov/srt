(defpackage #:srt-kd
  (:nicknames :kd)
  (:use #:cl
        #:iterate
        #:parse-number
        #:cl-ppcre
        #:alexandria)
  (:export
   :coordinate :index-type
   :tri-patch :vertexes :indexes :aabb :tree
   :split-aabb
   :touches-triangle
   :aabb :corners
   :kd-node :l :r :split-position))
