(in-package #:kd)



(defun world->screen (value &key (delta 300) (scale 150))
  (coerce (round (+ delta (* scale value))) 'fixnum))

(defun draw-xy-kd-tree (patch)
  (labels ((affine (trio) 
             (iter (for i from 0 to 2)
                   (collect (sdl:point :x (world->screen (car (elt trio i)))
                                       :y (world->screen (cadr (elt trio i)))))))
           (get-xy-by-triangle-number (tri)
             (iter (for i from 0 to 2)
                   (collect (list (get-coord-by-indexes patch tri i 0)
                                  (get-coord-by-indexes patch tri i 1))))))
    (sdl:with-init ()
      (sdl:window 600 600 :title-caption  "Hit Q or ESC to quit.")
      (setf (sdl:frame-rate) 60)
      (sdl-gfx:initialise-default-font sdl-gfx:*Font-6x12*)
      
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key key)
                         (when (or (sdl:key= key :sdl-key-escape) 
                                   (sdl:key= key :sdl-key-q))  
                           (sdl:push-quit-event)))
        (:idle ()
               (sdl:clear-display sdl:*black*)
               
               (let* ((l (mapcar #'(lambda (x)(world->screen x)) (corners (aabb patch))))
                      
                      (left   (elt l 0))
                      (bottom (elt l 1))
                      (right  (elt l 3))
                      (top    (elt l 4))
                      (r (rectangle-from-edges-* left bottom right top))

                      (split-x       (world->screen (split-position (kd-tree-root patch))))
                      (split-y-left  (world->screen (split-position (l (kd-tree-root patch)))))
                      (split-y-right (world->screen (split-position (r (kd-tree-root patch))))))

                 (sdl:with-color (color (sdl:color :r 150 :g 75 :b 0))
                   (iter (for i from 0 below (array-dimension (indexes patch) 0))
                         (sdl:draw-polygon (affine (get-xy-by-triangle-number i)) :aa t))
                   
                   (sdl:with-color (color (sdl:color :r 150 :g 0 :b 0))
                     (sdl:draw-line (sdl:point :x split-x :y bottom)
                                    (sdl:point :x split-x :y top)))
                   
                   (sdl:with-color (color (sdl:color :r 0 :g 150 :b 0))
                     (sdl:draw-line (sdl:point :x left    :y split-y-left)
                                    (sdl:point :x split-x :y split-y-left))
                     (sdl:draw-line (sdl:point :x split-x :y split-y-right)
                                    (sdl:point :x right   :y split-y-right)))
                   
                   (sdl:draw-rectangle r)))
                 
               (sdl:update-display))))))

(draw-xy-kd-tree *test-patch*)