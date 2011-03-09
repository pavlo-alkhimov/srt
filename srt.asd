;; -*- mode: lisp; indent-tabs-mode: nil -*-

(defsystem srt
  :description "Sandbox RayTracer."
  :author "Paul Alkhimov <alkhimov@gmail.com>"
  :version "0.0.1"
  :licence "BSD"
  :depends-on (:cl-ppcre :parse-number)
  :components
  ((:module packages
            :components
            ((:file "packages")))
   (:module debug
            :depends-on (packages)
            :components
            ((:file "debug")))
   (:module types
            :depends-on (packages debug) 
            :serial t
            :components
            ((:file "base-types")
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
            ((:file "traverse-kd-tree")))
   (:module tests
            :depends-on (obj traverse)
            :components
            ((:file "init-tests")
             (:file "kd-build")
             (:file "kd-traverse")))))

