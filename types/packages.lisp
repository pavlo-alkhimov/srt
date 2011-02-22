(defpackage #:srt-kd
  (:nicknames :kd)
  (:use #:cl)
  (:export
           :coordinate :index-type
           :tri-patch :vertexes :indexes :aabb :tree
           :split-aabb
           :touches-triangle
           :aabb :corners
           :kd-node :l :r :split-position))
