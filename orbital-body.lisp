(defpackage space-game.orbital-body
  (:use :cl :annot.class :iterate)
  (:nicknames :obody))
(in-package :space-game.orbital-body)
(annot:enable-annot-syntax)

@export-class
(defclass orbital-body ()
  ((name
    :initarg :name
    :accessor orbital-body-name)

   (mass
    :documentation "(solar mass) mass, as a ratio relative to the sun"
    :type 'double-float
    :accessor orbital-body-mass
    :initform (error "mass is required")
    :initarg :mass)
   
   (orbital-radius
    :type '(or double-float null)       ; has to be nullable because
                                        ; the default is NIL
    :documentation "(au) the distance between this and its parent"
    :accessor orbital-body-orbital-radius
    :initform nil                       ; i don't think there's a
                                        ; reasonable initform for
                                        ; this. not all ORBITAL-BODYs
                                        ; will have an ORBITAL-RADIUS.
    :initarg :orbital-radius)
   
   (angle
    :documentation "(radians) the current angle of this' orbit ~
                    around its parent"
    :type 'double-float
    :accessor orbital-body-angle
    :initform 0.0d0                     ; starting at 0 orbit seems
                                        ; perfectly reasonable, though
                                        ; it seems suspicious that
                                        ; this leaves the root with a
                                        ; valid ANGLE
    :initarg :angle)
   
   (children
    :documentation "a list of ORBITAL-BODYs which orbit this"
    :accessor orbital-body-children
    :initform '()                       ; the default case of having
                                        ; no children seems intuitive
    :initarg :children)))

(defmethod print-object ((obj orbital-body) stream)
  (print-unreadable-object (obj stream :type t :identity t)))

(defmacro nl (stream)
  "just print a fucking newline"
  `(pprint-newline :mandatory ,stream))

(defun pretty-print-orbital-body (this &optional (stream *standard-output*))
  (with-slots (name mass orbital-radius angle children) this
    (labels ((print-name (stream)
               (write-string (string name) stream))

             (print-property (prop val stream)
               (nl stream)
               (write val :stream stream)
               (write-char #\space stream)
               (write-string prop stream))
             
             (print-children (stream)
               (nl stream)
               (pprint-logical-block (stream children)
                 (write-string "children:" stream)
                 (pprint-indent :block 1 stream)
                 (iterate (for child next (pprint-pop))
                          (after-each (unless child (terminate))
                                      (nl stream)
                                      (pretty-print-orbital-body
                                       child
                                       stream))))))
      
      (pprint-logical-block (stream nil)
        (print-name stream)
        (pprint-indent :block 1 stream)
        (print-property "M☉" mass stream)
        
        (when orbital-radius
          (print-property "au orbit" orbital-radius stream)
          (print-property "rad orbit" angle stream))
        
        (when children (print-children stream))))))

@export
(defun orbital-body-to-pretty-string (this)
  (let* ((*print-pretty* t)
         (s (make-string-output-stream)))
    (pretty-print-orbital-body this s)
    (get-output-stream-string s)))
