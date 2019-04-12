(defpackage space-game
  (:use
   :cl
   :alexandria
   :sketch
   :space-game.orbital-body
   :space-game.earths-solar-system)
  (:shadowing-import-from :sketch :rotate))
(in-package :space-game)
(annot:enable-annot-syntax)

(defvar *time-step* (coerce (/ 365)
                     'double-float))

(defun orbital-body-add-child (orbital-body new-child)
  "you should probably just construct your trees right the first ~
   time, but this is useful for hacking"
  (let ((children (orbital-body-children orbital-body))) 
    (unless (member new-child (orbital-body-children orbital-body))
      (setf (orbital-body-children orbital-body)
            (cons new-child
                  children)))))

(defun orbital-period (radius center-mass)
  "from RADIUS (au) and CENTER-MASS (solar mass), compute an orbital ~
   period (years)

   Recall that Kepler's Third Law for a small child and a large parent is
   M P^2 = a^3
   where M is the mass of the parent,
         P is the period,
     and a is the length of the semimajor axis
   a little algebra then takes us to
   P = sqrt( a^3 / M )"
  (sqrt (/ (expt radius 3)
           center-mass)))

(defmethod orbital-body-period ((parent orbital-body) (child orbital-body))
  "the period (years) of the orbit of CHILD around PARENT"
  (let ((radius (orbital-body-orbital-radius child))
        (center-mass (orbital-body-mass parent)))
    (orbital-period radius center-mass)))

(defmethod orbit-advance ((this orbital-body) dt)
  (dolist (child (orbital-body-children this))
    (incf (orbital-body-angle child)
          (/ dt (orbital-body-period this child))))
  this                                  ; return THIS, so that running
                                        ; ORBIT-ADVANCE in the repl causes it
                                        ; to print the result
  )

(defsketch space-game
    ((title "space-game")
     (width 640)
     (height 480)
     (root-object *sun*))
  (text (orbital-body-to-pretty-string
         (orbit-advance root-object *time-step*))
        20 20))

(defvar *the-game* (make-instance 'space-game)
  )
