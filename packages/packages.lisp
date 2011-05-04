(defpackage #:srt-kd
  (:nicknames :kd)
  (:use #:cl
        #:iterate
        #:parse-number
        #:cl-ppcre
        #:alexandria
        #:metabang-bind
        #:lispbuilder-sdl)
  (:export
   :coordinate :index-type
   :patch :vertexes :indexes :aabb :tree
   :split-aabb
   :touches-triangle
   :aabb :corners
   :node :l :r :split-position
   :draw-xy-kd-tree))
