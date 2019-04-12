(defpackage space-game.asd
  (:use :cl :asdf))
(in-package :space-game.asd)

(defsystem "space-game"
  :version "0.0.0"
  :author "gefjon"
  :depends-on (:cl-annot
               :alexandria
               :sketch
               :str)
  :serial t
  :components
  ((:file "orbital-body")
   (:file "earths-solar-system")
   (:file "space-game")))
