;; -*- Lisp -*-
;; Test some MOP-like CLOS features

#-CMU
(progn
  (defstruct rectangle1 (x 0.0) (y 0.0))
  (defclass counted1-class (structure-class)
    ((counter :initform 0)))
  (defclass counted1-rectangle (rectangle1) () (:metaclass counted1-class))
  (defmethod make-instance :after ((c counted1-class) &rest args)
    (incf (slot-value c 'counter)))
  (slot-value (find-class 'counted1-rectangle) 'counter)
  (make-instance 'counted1-rectangle)
  (slot-value (find-class 'counted1-rectangle) 'counter))
#-CMU
1

#-(or CMU SBCL)
(progn
  (defclass rectangle2 ()
    ((x :initform 0.0 :initarg x) (y :initform 0.0 :initarg y)))
  (defclass counted2-class (standard-class)
    ((counter :initform 0)))
  (defclass counted2-rectangle (rectangle2) () (:metaclass counted2-class))
  (defmethod make-instance :after ((c counted2-class) &rest args)
    (incf (slot-value c 'counter)))
  (slot-value (find-class 'counted2-rectangle) 'counter)
  (make-instance 'counted2-rectangle)
  (slot-value (find-class 'counted2-rectangle) 'counter))
#-(or CMU SBCL)
1

#+(or CLISP ALLEGRO)
(progn
  (defclass counter ()
    ((count :allocation :class :initform 0 :reader how-many)))
  (defclass counted-object (counter) ((name :initarg :name)))
  (defmethod initialize-instance :after ((obj counter) &rest args)
    (incf (slot-value obj 'count)))
  (list (how-many (make-instance 'counted-object :name 'foo))
        (how-many (#+(or CLISP ALLEGRO) clos:class-prototype (find-class 'counter)))
        (how-many (make-instance 'counted-object :name 'bar))
        (how-many (#+(or CLISP ALLEGRO) clos:class-prototype (find-class 'counter)))))
#+(or CLISP ALLEGRO)
(1 1 2 2)
