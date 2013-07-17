;;; 
;;; This file demonstrates the usage of custom wbtree types. Since
;;; I hate boring abstract examples, the actual example code is 
;;; embedded into some kind of "real-world" use case: value-added-tax
;;; rates.
;;;
;;; We maintain two types of wbtrees in this module:
;;;
;;; - a string-tree is used as index of all defined VAT classes
;;; - a timestamp-tree is used in each class to keep track of individual rates
;;;
;;; To spice things up a little bit, we make things thread-safe.
;;;
;;; Stuff demonstrated here:
;;;
;;; - define-wbtree
;;; - wbtree-map
;;; - wbtree-update
;;; - wbtree-find
;;; - wbtree-lower-boundary-node
;;;
;;; Oh, btw., the rates used in this example are actual ones in use
;;; in Germany since the 1968ies. No guarantees, though!

#-(and) (ql:quickload "local-time")
#-(and) (ql:quickload "bordeaux-threads")

(defpackage "WBTREE-EXAMPLES"
  (:use "COMMON-LISP" "DARTS.LIB.WBTREE" "LOCAL-TIME" "BORDEAUX-THREADS") 
  (:export "VAT-CLASS" "VAT-CLASS-NAME" "VAT-CLASS-RATES" "VAT-RATE" "VAT-RATE-CLASS"
           "VAT-RATE-VALUE" "VAT-RATE-VALID-FROM" "ALL-VAT-CLASSES" "FIND-VAT-CLASS"
           "FIND-VAT-RATE" "MAP-VAT-CLASSES" "MAP-VAT-RATES" "DEFINE-VAT-CLASS"
           "DEFINE-VAT-CLASS*"))

(in-package "WBTREE-EXAMPLES")

(defparameter +date-only-format+ '((:year 4) #\- (:month 2) #\- (:day 2)))

(define-wbtree timestamp-tree timestamp<)
(define-wbtree string-tree string<)

(defvar *classes-lock* (make-lock "VAT Classes Lock"))
(defvar *all-classes* (string-tree))

(defclass vat-class ()
  ((name
     :type string :initarg :name
     :reader vat-class-name
     :documentation "The unique name of this class")
   (rates
     :type timestamp-tree :initform (timestamp-tree)
     :accessor vat-class-rates
     :documentation "Tree of associated rates"))
  (:documentation "A named collection of individual VAT rates. All classes
    are kept in a search tree keyed by their names. Instances of this class
    are immutable after construction."))
   

(defclass vat-rate ()
  ((class
     :type vat-class :initarg :class
     :reader vat-rate-class
     :documentation "Class, this rate belongs to")
   (valid-from
     :type timestamp :initarg :valid-from
     :reader vat-rate-valid-from
     :documentation "The date, when this rate becomes valid")
   (value
     :type ratio :initarg :value
     :reader vat-rate-value
     :documentation "The actual rate value"))
  (:documentation "An individual VAT rate. Each rate belongs to a
    single class. All rates of a class are kept in a binary search
    tree keyed by their valid-from timestamps. Given a transaction 
    timestamp R, in order to find the VAT rate applicable at R,
    find the rate entry, whose valid-from timestamp is the one with
    the largest key <= R. Instances of this class are immutable
    after construction."))


(defmethod print-object ((object vat-class) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (vat-class-name object))))


(defmethod print-object ((object vat-rate) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~A ~F"
            (vat-class-name (vat-rate-class object))
            (format-timestring nil (vat-rate-valid-from object) :format +date-only-format+)
            (vat-rate-value object))))


;;; On implementations, which have CAS support, this could actually be
;;; done event better. We fake CAS here using a global lock.


(defun compare-and-set-vat-classes (old-classes new-classes)
  (with-lock-held (*classes-lock*)
    (and (eq old-classes *all-classes*)
         (progn (setf *all-classes* new-classes) t))))


(defun all-vat-classes ()
  (with-lock-held (*classes-lock*)
    *all-classes*))
        

(defun define-vat-class* (name rates &key (if-exists :supersede) (if-does-not-exist :create))
  (labels ((parse-time (value)
             (etypecase value
               (timestamp value)
               (string (parse-timestring value))))
           (parse-rate (spec)
             (destructuring-bind (valid-from* . value) spec
               (make-instance 'vat-rate :valid-from (parse-time valid-from*) :value value)))
           (copy-with-new-rates (class list)
             (loop
                :with seed := (timestamp-tree) 
                  :and new-class := (make-instance 'vat-class :name (vat-class-name class))
                :for rate :in list
                  :do (setf (slot-value rate 'class) new-class)
                      (setf seed (wbtree-update (vat-rate-valid-from rate) rate seed))
                :finally (setf (slot-value new-class 'rates) seed)
                         (return new-class)))
           (create-with-new-rates (list)
             (loop
                :with seed := (timestamp-tree) 
                  :and new-class := (make-instance 'vat-class :name name)
                :for rate :in list
                  :do (setf (slot-value rate 'class) new-class)
                      (setf seed (wbtree-update (vat-rate-valid-from rate) rate seed))
                :finally (setf (slot-value new-class 'rates) seed)
                         (return new-class))))
    (let ((name (string name))
          (rates (mapcar #'parse-rate rates)))
      (loop
         :named define
         :do (let* ((all-classes (all-vat-classes))
                    (present (wbtree-find name all-classes)))
               (if present
                   (ecase if-exists
                     ((:error) (error "a VAT class ~S already exists" name))
                     ((:ignore) (return-from define (values present nil)))
                     ((:supersede) 
                      (let* ((new-class (copy-with-new-rates present rates))
                             (new-class-tree (wbtree-update name new-class all-classes)))
                        (when (compare-and-set-vat-classes all-classes new-class-tree)
                          (return-from define (values new-class :replaced))))))
                   (ecase if-does-not-exist
                     ((:error) (error "there is no VAT class ~S" name))
                     ((:ignore) (return-from define (values nil nil)))
                     ((:create)
                      (let* ((new-class (create-with-new-rates rates))
                             (new-class-tree (wbtree-update name new-class all-classes)))
                        (when (compare-and-set-vat-classes all-classes new-class-tree)
                          (return-from define (values new-class :created))))))))))))
               

(defun find-vat-class (value &optional (errorp t) default)
  (if (typep value 'vat-class)
      (values value t)
      (let ((class (wbtree-find value (all-vat-classes))))
        (cond
          (class (values class t))
          ((not errorp) (values default nil))
          (t (error "there is no VAT class matching ~S" value))))))


(defun find-vat-rate (class &optional (as-of (now)) (errorp t) default)
  (let* ((class* (find-vat-class class nil nil))
         (rate (and class* (wbtree-lower-boundary-node as-of (vat-class-rates class*)))))
    (cond
      (rate (values (wbtree-node-value rate) t))
      ((not errorp) (values default nil))
      (t (error "there is no defined rate for VAT class ~S as of ~S" class as-of)))))


(defun map-vat-classes (function)
  (wbtree-map (lambda (node) (funcall function (wbtree-node-value node))) 
              (all-vat-classes)))


(defun map-vat-rates (function &optional class)
  (flet ((map-rates (class)
           (wbtree-map (lambda (node) (funcall function (wbtree-node-value node)))
                       (vat-class-rates class))))
    (if class
        (map-rates (find-vat-class class))
        (map-vat-classes #'map-rates))))



(defmacro define-vat-class (name &body rates)
  `(define-vat-class* ',name
       (list ,@(loop
                  :for (time-form value-form) :in rates
                  :collecting `(cons ,time-form ,value-form)))
     :if-exists :supersede
     :if-does-not-exist :create))


(define-vat-class :none)

(define-vat-class :full
  ("1968-01-01T12:00:00.000Z" 10/100)
  ("1968-07-01T12:00:00.000Z" 11/100)
  ("1978-01-01T12:00:00.000Z" 12/100)
  ("1979-07-01T12:00:00.000Z" 13/100)
  ("1983-07-01T12:00:00.000Z" 14/100)
  ("1993-01-01T12:00:00.000Z" 15/100)
  ("1998-04-01T12:00:00.000Z" 16/100)
  ("2007-01-01T12:00:00.000Z" 19/100))
  
(define-vat-class :half
  ("1968-01-01T12:00:00.000Z" 50/1000)
  ("1968-07-01T12:00:00.000Z" 55/1000)
  ("1978-01-01T12:00:00.000Z" 60/1000)
  ("1979-07-01T12:00:00.000Z" 65/1000)
  ("1983-07-01T12:00:00.000Z" 70/1000)
  #-(and) ("1993-01-01T12:00:00.000Z" 70/1000)
  #-(and) ("1998-04-01T12:00:00.000Z" 70/1000)
  #-(and) ("2007-01-01T12:00:00.000Z" 70/1000))

