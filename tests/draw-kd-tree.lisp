;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(in-package #:kd)



(defun draw-patch (patch)
  "Draw a patch."
  (declare (patch patch))
  (gl:shade-model :flat)
  (gl:with-primitives :triangles
    (dotimes (triangle-index (array-dimension (patch-is patch) 0))
      (gl:normal (+ (random 0.2) 0.34)
                 (+ (random 0.2) 0.34)
                 (+ (random 0.2) 0.34))
      (dotimes (vertex 3)
        (gl:vertex (get-coord-by-indexes patch triangle-index vertex 0)
                   (get-coord-by-indexes patch triangle-index vertex 1)
                   (get-coord-by-indexes patch triangle-index vertex 2))))))

(defclass patch-window (glut:window)
  ((view-rotx :initform 20.0)
   (view-roty :initform 30.0)
   (view-rotz :initform 0.0)
   patch
   (angle :initform 0.0)
   (count :initform 1)
   (t0 :initform 0))
  (:default-initargs :title "Patch" :mode '(:double :rgb :depth)))

(defmethod glut:display-window :before ((window patch-window))
  (with-slots (patch) window
    (gl:light :light0 :position #(5.0 5.0 10.0 0.0))
    (gl:enable :cull-face :lighting :light0 :depth-test)
    ;; patch
    (setq patch (gl:gen-lists 1))
    (gl:with-new-list (patch :compile)
      (gl:material :front :ambient-and-diffuse #(0.2 0.2 1.0 1.0)) ; blue
      (draw-patch *test-patch*))
    (gl:enable :normalize)))

(defun print-frame-rate (window)
  "Prints the frame rate every ~5 seconds."
  (with-slots (count t0) window
    (incf count)
    (let ((time (get-internal-real-time)))
      (when (= t0 0)
        (setq t0 time))
      (when (>= (- time t0) (* 5 internal-time-units-per-second))
        (let* ((seconds (/ (- time t0) internal-time-units-per-second))
               (fps (/ count seconds)))
          (format *terminal-io* "~D frames in ~3,1F seconds = ~6,3F FPS~%"
                  count seconds fps))
        (setq t0 time)
        (setq count 0)))))

(defmethod glut:display ((window patch-window))
  (with-slots (view-rotx view-roty view-rotz angle patch)
      window
    (gl:clear :color-buffer :depth-buffer)
    (gl:with-pushed-matrix
      (gl:rotate view-rotx 1 0 0)
      (gl:rotate view-roty 0 1 0)
      (gl:rotate view-rotz 0 0 1)
      (gl:with-pushed-matrix
        (gl:translate -3.1 4.2 0.0)
        (gl:rotate (- (* -2 angle) 25) 0 0 1)
        (gl:call-list patch)))
    (glut:swap-buffers)
    (print-frame-rate window)))

(defmethod glut:idle ((window patch-window))
  (incf (slot-value window 'angle) 0.1)
  (glut:post-redisplay))

(defmethod glut:keyboard ((window patch-window) key x y)
  (declare (ignore x y))
  (case key
    (#\z (incf (slot-value window 'view-rotz) 5.0)
         (glut:post-redisplay))
    (#\Z (decf (slot-value window 'view-rotz) 5.0)
         (glut:post-redisplay))
    (#\Esc (glut:destroy-current-window))))

(defmethod glut:special ((window patch-window) special-key x y)
  (declare (ignore x y))
  (with-slots (view-rotx view-roty) window
    (case special-key
      (:key-up (incf view-rotx 5.0))
      (:key-down (decf view-rotx 5.0))
      (:key-left (incf view-roty 5.0))
      (:key-right (decf view-roty 5.0)))
    (glut:post-redisplay)))

(defmethod glut:reshape ((w patch-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((h (/ height width)))
    (gl:frustum -1 1 (- h) h 5 60))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate 0 0 -40))

(defmethod glut:visibility ((w patch-window) state)
  (case state
    (:visible (glut:enable-event w :idle))
    (t (glut:disable-event w :idle))))

(glut:display-window (make-instance 'patch-window
                                    :pos-x 160 :pos-y 10
                                    :width 640 :height 480))