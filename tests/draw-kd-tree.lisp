;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Here one can find a mix from OpenGl/GLUT examples.
;;; It is a temporary stup.

(in-package #:kd)

;; (defun draw-xy-kd-tree (patch)
;;   (labels ((affine (trio) 
;;              (iter (for i from 0 to 2)
;;                    (collect (sdl:point :x (world->screen (car (elt trio i)))
;;                                        :y (world->screen (cadr (elt trio i)))))))
;;            (get-xy-by-triangle-number (tri)
;;              (iter (for i from 0 to 2)
;;                    (collect (list (get-coord-by-indexes patch tri i 0)
;;                                   (get-coord-by-indexes patch tri i 1))))))
;;     (sdl:with-init ()
;;       (sdl:window 600 600 :title-caption  "Hit Q or ESC to quit.")
;;       (setf (sdl:frame-rate) 60)
;;       ;; (with-font (font sdl-gfx:*Font-6x12*))

;;       (sdl:with-events ()
;;         (:quit-event () t)
;;         (:key-down-event (:key key)
;;                          (when (or (sdl:key= key :sdl-key-escape)
;;                                    (sdl:key= key :sdl-key-q))  
;;                            (sdl:push-quit-event)))
;;         (:idle ()
;;                (sdl:clear-display sdl:*black*)

;;                (let* ((l (mapcar #'(lambda(x)(world->screen x))
;;                                  (corners (patch-aabb patch))))

;;                       (left   (elt l 0))
;;                       (bottom (elt l 1))
;;                       (right  (elt l 3))
;;                       (top    (elt l 4))
;;                       (split-x (if (and (patch-kd-tree patch)
;;                                         (not (is-leaf (patch-kd-tree patch))))
;;                                    (world->screen (node-split (patch-kd-tree patch)))
;;                                    left)))

;;                  ;; (sdl:with-font (sdl:*Font-6x12*)
;;                  ;;   (sdl:draw-string-solid-* "Font is 8x8." 10 10))

                 ;; (sdl:with-color (color (sdl:color :r 150 :g 75 :b 0))
                 ;;   (iter (for i from 0 below (array-dimension (patch-is patch) 0))
                 ;;         (sdl:draw-polygon (affine (get-xy-by-triangle-number i)) :aa t))
                   
;;                    (sdl:with-color (color (sdl:color :r 150 :g 0 :b 0))

;;                      (sdl:draw-line (sdl:point :x split-x :y (1- bottom))
;;                                     (sdl:point :x split-x :y (1+ top))))

;;                    (and (patch-kd-tree patch)
;;                         (not (is-leaf (patch-kd-tree patch)))
;;                         (node-left  (patch-kd-tree patch))
;;                         (node-right (patch-kd-tree patch))
;;                         (not (is-leaf (node-left  (patch-kd-tree patch))))
;;                         (not (is-leaf (node-right (patch-kd-tree patch))))
;;                         (let ((split-y-left  (world->screen (node-split (node-left (patch-kd-tree patch)))))
;;                               (split-y-right (world->screen (node-split (node-right (patch-kd-tree patch))))))
;;                           (sdl:with-color (color (sdl:color :r 0 :g 150 :b 0))
;;                             (sdl:draw-line (sdl:point :x left    :y split-y-left)
;;                                            (sdl:point :x split-x :y split-y-left))
;;                             (sdl:draw-line (sdl:point :x split-x :y split-y-right)
;;                                            (sdl:point :x right   :y split-y-right)))))))

;;                (sdl:update-display))))))

(defparameter light-x 1)
(defparameter light-y 1)
(defparameter light-z 1)
(defparameter light-position (list light-x light-y light-z 0))

(defclass glut-patch-window (glut:window)
  ()
  (:default-initargs :width 640 :height 480 :title "patch view"
                     :mode '(:single :rgb :depth)))

(defun-with-dbg emit-patch (patch)
  "Generates the OpenGL model of the provided patch."  
  ;;; {{ NEW
  ;; (progn
  ;;  (gl:light :light0 :position #(5.0 5.0 10.0 0.0))
  ;;  (gl:enable :cull-face :lighting :light0 :depth-test)
  ;;  (setq gear1 (gl:gen-lists 1))
  ;;  (gl:with-new-list (gear1 :compile)
  ;;    (gl:material :front :ambient-and-diffuse #(0.8 0.1 0.0 1.0)) ; red
  ;;    (gl:shade-model :flat)
  ;;    (gl:normal 0 0 1)
  ;;    (gl:with-primitives :triangles
  ;;      (iter (for i from 0 below (array-dimension (patch-is patch) 0))
  ;;            (sdl:draw-polygon (affine (get-xy-by-triangle-number i)) :aa t))
       
  ;;      (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))))
  ;;  (gl:enable :normalize))
  ;;; }} NEW
  )

(defmethod glut:display-window :before ((w glut-patch-window))
  (gl:clear-color    0 0 0 0)
  (gl:cull-face      :back)
  (gl:depth-func     :less)
  (gl:disable        :dither)
  (gl:shade-model    :smooth)
  (gl:light-model    :light-model-local-viewer 1)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:enable         :light0 :lighting :cull-face :depth-test))

(defmethod glut:display ((window glut-patch-window))
  (gl:load-identity)
  (gl:translate  0 0 -5)
  (gl:rotate     0 1 1 0)
  (gl:light      :light0 :position light-position)
  (gl:light      :light0 :diffuse '(0.2 0.4 0.6 0))
  (gl:clear      :color-buffer :depth-buffer)
  (gl:color      1 1 1)
  (gl:front-face :cw)
  (glut:solid-teapot 1.3) ;; bwag-ha-ha
  (gl:front-face :ccw)
  (gl:flush)
  
  ;;; {{NEW
  ;; (progn
  ;;  (gl:clear :color-buffer :depth-buffer)
  ;;  (gl:with-pushed-matrix
  ;;    (gl:rotate view-rotx 1 0 0)
  ;;    (gl:rotate view-roty 0 1 0)
  ;;    (gl:rotate view-rotz 0 0 1)
  ;;    (gl:with-pushed-matrix             ; gear1
  ;;      (gl:translate -3 -2 0)
  ;;      (gl:rotate angle 0 0 1)
  ;;      (gl:call-list gear1))
  ;;    (gl:with-pushed-matrix             ; gear2
  ;;      (gl:translate 3.1 -2 0)
  ;;      (gl:rotate (- (* -2 angle) 9) 0 0 1)
  ;;      (gl:call-list gear2))
  ;;    (gl:with-pushed-matrix             ; gear3
  ;;      (gl:translate -3.1 4.2 0.0)
  ;;      (gl:rotate (- (* -2 angle) 25) 0 0 1)
  ;;      (gl:call-list gear3)))
  ;;  (glut:swap-buffers)
  ;;  (print-frame-rate window))
  ;;; }} NEW
  )

(defmethod glut:reshape ((window glut-patch-window) width height)
  (gl:viewport     0 0 width height)
  (gl:matrix-mode  :projection)
  (gl:load-identity)
  (glu:perspective 50 (/ width height) 0.5 20)
  (gl:matrix-mode  :modelview)
  (gl:load-identity))

(defmethod glut:keyboard ((window glut-patch-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defmethod glut:passive-motion ((w glut-patch-window) x y)
  (let* ((x (* 0.0015625 (- x 320))) ;; 1/640
         (y (* 0.0020833 (- y 240))) ;; 1/480
         (norma (/ 3.0 (sqrt (+ (* x x)
                                (* y y)
                                (* light-z light-z))))))
    (setf (elt light-position 0) (* x norma))
    (setf (elt light-position 1) (* -1 y norma)))
  (glut:post-redisplay))

(glut:display-window (make-instance 'glut-patch-window))