(defpackage space-game.earths-solar-system
  (:use :cl :space-game.orbital-body))
(in-package :space-game.earths-solar-system)
(annot:enable-annot-syntax)

;;; a lot of this math is coming from
;;; https://www.astro.umass.edu/~weinberg/a114/handouts/concept1.pdf
;;; and the numbers are mostly from
;;; https://en.wikipedia.org/wiki/Planetary_mass
;;; but probably shouldn't be trusted

@export
(defvar *mercury* (make-instance 'orbital-body
                                 :mass 	0.16601d-6
                                 :orbital-radius 0.387098
                                 :name 'mercury))

@export
(defvar *venus* (make-instance 'orbital-body
                               :mass 2.4478383d-6
                               :orbital-radius 0.723332
                               :name 'venus))

@export
(defvar *earth* (make-instance 'orbital-body
                               :mass 3.00348959632d-6
                               :orbital-radius 1.0
                               :name 'earth))

@export
(defvar *mars* (make-instance 'orbital-body
                              :mass 0.3227151d-6
                              :orbital-radius 1.523679
                              :name 'mars))

@export
(defvar *jupiter* (make-instance 'orbital-body
                                 :mass 954.79194d-6
                                 :orbital-radius 5.2044
                                 :name 'jupiter))

@export
(defvar *saturn* (make-instance 'orbital-body
                                :mass 285.8860d-6
                                :orbital-radius 9.5826
                                :name 'saturn))

@export
(defvar *uranus* (make-instance 'orbital-body
                                :mass 43.66244d-6
                                :orbital-radius 19.2184
                                :name 'uranus))

@export
(defvar *neptune* (make-instance 'orbital-body
                                 :mass 51.51389d-6
                                 :orbital-radius 30.11
                                 :name 'neptune))

@export
(defvar *sun* (make-instance 'orbital-body
                             :mass 1.0d0
                             :name 'sun
                             :children (list *mercury*
                                             *venus*
                                             *earth*
                                             *mars*
                                             *jupiter*
                                             *saturn*
                                             *uranus*
                                             *neptune*)))
