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

(defpackage "TIMETREE"
  (:use "COMMON-LISP" "DARTS.LIB.WBTREE" "LOCAL-TIME")
  (:export))

(in-package "TIMETREE")

(defparameter +date-only-format+ '((:year 4) #\- (:month 2) #\- (:day 2)))

(define-wbtree timestamp-tree timestamp<)
(define-wbtree string-tree string<)

(defvar *all-classes* (string-tree))

(defclass vat-class ()
  ((name
     :type string :initarg :name
     :reader vat-class-name)
   (rates
     :type timestamp-tree :initform (timestamp-tree)
     :accessor vat-class-rates)))


(defclass vat-rate ()
  ((class
     :type vat-class :initarg :class
     :reader vat-rate-class)
   (valid-from
     :type timestamp :initarg :valid-from
     :reader vat-rate-valid-from)
   (value
     :type ratio :initarg :value
     :reader vat-rate-value)))


(defmethod print-object ((object vat-class) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (vat-class-name object))))


(defmethod print-object ((object vat-rate) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~A ~F"
            (vat-class-name (vat-rate-class object))
            (format-timestring nil (vat-rate-valid-from object) :format +date-only-format+)
            (vat-rate-value object))))


(defun add-vat-class (name)
  (let ((present (wbtree-find name *all-classes*)))
    (if present
        (error "there is already a VAT class named ~S" name)
        (let ((class (make-instance 'vat-class :name name)))
          (setf *all-classes* (wbtree-update name class *all-classes*))
          class))))


(defun add-vat-rate (value valid-from class)
  (let* ((rates (vat-class-rates class))
         (present (wbtree-find valid-from rates)))
    (if present
        (error "there is already a rate valid-from ~S in ~S" valid-from class)
        (let ((rate (make-instance 'vat-rate :class class :valid-from valid-from :value value)))
          (setf (vat-class-rates class) (wbtree-update valid-from rate rates))
          rate))))


(defun map-vat-rates (function &optional class)
  (flet ((map-rates (class)
           (wbtree-map (lambda (node) (funcall function (wbtree-node-value node)))
                       class)))
    (if class
        (map-rates class)
        (wbtree-map (lambda (node) (map-rates (wbtree-node-value node)))
                    *all-classes*))))


(defun map-vat-classes (function)
  (wbtree-map (lambda (node) (funcall function (wbtree-node-value node))) 
              *all-classes*))


(defun find-vat-class (value &optional (errorp t) default)
  (if (typep value 'vat-class)
      (values value t)
      (let ((class (wbtree-find value *all-classes*)))
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


(defun define-vat-class* (name &optional rates)
  (let* ((name (string name))
         (class (let ((present (find-vat-class name nil)))
                  (if (not present)
                      (add-vat-class name)
                      (progn
                        (setf (vat-class-rates present) (timestamp-tree))
                        present)))))
    (flet ((parse-time (value)
             (etypecase value
               (timestamp value)
               (string (parse-timestring value)))))
      (loop
         :for (valid-from* . value) :in rates
         :for valid-from := (parse-time valid-from*)
         :do (add-vat-rate value valid-from class))
      class)))


(defmacro define-vat-class (name &body rates)
  `(define-vat-class* ',name
       (list ,@(loop
                  :for (time-form value-form) :in rates
                  :collecting `(cons ,time-form ,value-form)))))


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
  ("1993-01-01T12:00:00.000Z" 70/1000)
  ("1998-04-01T12:00:00.000Z" 70/1000)
  ("2007-01-01T12:00:00.000Z" 70/1000))
