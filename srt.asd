;; -*- mode: lisp; indent-tabs-mode: nil -*-

(defsystem srt
  :description "Sandbox RayTracer."
  :author "Paul Alkhimov <alkhimov@gmail.com>"
  :version "0.0.1"
  :licence "BSD"
  :depends-on (:cl-ppcre :parse-number)
  :components
  ((:module types
            :serial t
            :components
            ((:file "packages")
             (:file "base-types")
             (:file "debug")
             (:file "aabb")
             (:file "kd-tree")
             (:file "kd-tree-impl")))
   (:module patch
            :depends-on (types)
            :serial t
            :components
            ((:file "patch")
             (:file "patch-impl")))
   (:module obj
            :depends-on (patch)
            :components
            ((:file "obj-file")))
   (:module traverse
            :depends-on (patch)
            :components
            ((:file "traverse-kd-tree")))))

