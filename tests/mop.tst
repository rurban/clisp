;; -*- Lisp -*-
;; Test some MOP-like CLOS features

; Make the MOP symbols accessible from package CLOS.
#-(or CLISP ALLEGRO LISPWORKS)
(let ((packname
         #+SBCL "SB-PCL" ; or "SB-MOP"?
         #+CMU "PCL" ; or "MOP"?
         #+OPENMCL "CCL" ; ?
         ))
  #+SBCL (unlock-package packname)
  (rename-package packname packname (cons "CLOS" (package-nicknames packname))))

#-CMU18
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
#-CMU18
1

#-CMU18
(progn
  (defclass rectangle2 ()
    ((x :initform 0.0 :initarg x) (y :initform 0.0 :initarg y)))
  (defclass counted2-class (standard-class)
    ((counter :initform 0)))
  #-CLISP
  (defmethod clos:validate-superclass ((c1 counted2-class) (c2 standard-class))
    t)
  (defclass counted2-rectangle (rectangle2) () (:metaclass counted2-class))
  (defmethod make-instance :after ((c counted2-class) &rest args)
    (incf (slot-value c 'counter)))
  (slot-value (find-class 'counted2-rectangle) 'counter)
  (make-instance 'counted2-rectangle)
  (slot-value (find-class 'counted2-rectangle) 'counter))
#-CMU18
1

(progn
  (defclass counter ()
    ((count :allocation :class :initform 0 :reader how-many)))
  (defclass counted-object (counter) ((name :initarg :name)))
  (defmethod initialize-instance :after ((obj counter) &rest args)
    (incf (slot-value obj 'count)))
  (list (how-many (make-instance 'counted-object :name 'foo))
        (how-many (clos:class-prototype (find-class 'counter)))
        (how-many (make-instance 'counted-object :name 'bar))
        (how-many (clos:class-prototype (find-class 'counter)))))
(1 1 2 2)

;; Check that the slot :accessor option works also on structure-class.
(progn
  (defclass structure01 () ((x :initarg :x :accessor structure01-x))
    (:metaclass structure-class))
  (let ((object (make-instance 'structure01 :x 17)))
    (list (typep #'structure01-x 'generic-function)
          (structure01-x object)
          (progn (incf (structure01-x object)) (structure01-x object)))))
(t 17 18)
