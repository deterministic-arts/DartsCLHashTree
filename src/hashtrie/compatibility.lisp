#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Hash Tree
  Copyright (c) 2013 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package "DARTS.LIB.HASHTRIE")


(defstruct (hash-control
             (:copier nil)
             (:predicate hash-control-p)
             (:constructor %make-hash-control (test hash)))
  (test (error "missing equality test") :read-only t)
  (hash (error "missing hash function") :read-only t)
  (%empty nil))

(defstruct (hashtree 
             (:copier nil)
             (:predicate hashtreep)
             (:constructor %make-hashtree (control node)))
  (control (error "missing hash control (bug)") :read-only t)
  (node nil :read-only t))



(defun make-hash-control (test hash)
  "Creates a new \"hash controller\". A hash controller is basically
a pair of two functions `test` and `hash`. The `test` function is a
predicate `(lambda (a b) ...) => boolean`, which tests two values `a`
and `b` for equality. The default `test` function is `eql`. The `hash`
function has the signature `(lambda (x) ...) => integer`. It is called
in order to compute a hash value for the given value `x`. The default
hash function is `sxhash`."
  (let ((control (%make-hash-control test hash)))
    (setf (hash-control-%empty control) (%make-hashtree control (make-node 0 ())))
    control))


(defun make-hashtree (&key (test #'eql) (hash #'sxhash))
  "Creates a new hash tree. The hash tree will use the function supplied
as the value of the :test parameter as its equality test predicate, and
the value passed as :hash as its hash function. If omitted, the default
equality test is eql, and the default hash function is sxhash."
  (hash-control-%empty (make-hash-control test hash)))


(defun hashtree-fold (function initial-value map)
  "Reduces a hash tree into a single summary value. The function FUNCTION
must have the signature (lambda (key value summary) ...). It is called for
each key/value pair present in MAP. On the first invocation, the summary
value will be INITIAL-VALUE, and on each subsequent invocation it will be 
the primary return value of FUNCTION from the previous invocation. After 
all pairs have been processed, that last return value of FUNCTION is returned.
If MAP is the empty tree, then this function returns INITIAL-VALUE.

Note, that the order, in which this function visits the key/value pairs in
MAP, is undefined."
  (declare (dynamic-extent function))
  (hashtrie-fold initial-value (lambda (x y z) (funcall function y z x)) (hashtree-node map)))


(defun hashtree-map (function map)
  "Maps a function across all elements in a hashtree. This function applies
FUNCTION to each element in MAP; FUNCTION must accept two arguments and will
be called with an element's key as first, and its associated value as second
argument. The result of this function is undefined. 

Note, that the order, in which this function visits the key/value pairs in
MAP, is undefined."
  (declare (dynamic-extent function))
  (hashtrie-map function (hashtree-node map)))


(defun hashtree-keys (map)
  "Computes a list of the keys of all key/value pairs in MAP. Note, that 
the order of the values in the resulting list is undefined."
  (hashtree-fold 
    (lambda (key value list) (declare (ignore value)) (cons key list))
    nil
    map))


(defun hashtree-values (map)
  "Computes a list of the values of all key/value pairs in MAP. Note, that 
the order of the values in the resulting list is undefined."
  (hashtree-fold
    (lambda (key value list) (declare (ignore key)) (cons value list))
    nil
    map))


(defun hashtree-pairs (map)
  "Computes a list of all key/value pairs in MAP. The result is a list
of conses `(key . value)`. Note, that the order of the values in the 
resulting list is undefined."
  (hashtree-fold
    (lambda (key value list) (cons (cons key value) list))
    nil 
    map))


(defmacro do-hashtree ((key value) tree &body body)
  "Evaluates the form TREE, which must yield a hash tree instance. 
Visits each key/value pair in that tree, binding the variable named KEY 
to the pair's key, and VALUE to the pair's value, and evaluates the forms
in BODY like progn does. There is an implicit anonymous block surrounding
the expansion of this form, which may be used to stop the iteration before
all elements have been visited. The result value of this form is nil, unless 
an explicit result value is specified in BODY by doing a `return`."
  `(block nil
     (hashtree-map #'(lambda (,key ,value) ,@body) 
                   ,tree)
     nil))


(defun hashtree-count (map)
  "Counts the number of entries in a hash tree. Note, that this function
has a runtime complexity of `O(n)`, with `n` being the number of key/value
pairs in MAP."
  (hashtrie-count (hashtree-node map)))


(defmethod print-object ((ob hashtree) stream)
  (print-unreadable-object (ob stream :type t)
    (format stream "~S ~S ~S ~S ~S ~D"
       :test (hash-control-test (hashtree-control ob))
       :hash (hash-control-hash (hashtree-control ob))
       :count (hashtree-count ob))))


(defun hashtree-test (map)
  "Obtains the test function used by the given hash tree MAP."
  (hash-control-test (hashtree-control map)))


(defun hashtree-hash (map)
  "Obtains the hash function used by the given hash tree MAP."
  (hash-control-hash (hashtree-control map)))


(defun hashtree-empty-p (map)
  "Tests, whether the given hash tree MAP is empty."
  (darts.lib.hashtrie::node-empty-p (hashtree-node map)))


;; (declaim (ftype (function (t function) (values (unsigned-byte 32))) compute-hash)
;;         (inline compute-hash))

;; (defun compute-hash (value fn)
;;  (the (unsigned-byte 32) 
;;    (logand #xffffffff (funcall fn value))))


(defun hashtree-get (key map &optional default)
  "Obtains the value associated with a given key KEY in MAP. If 
no matching association exists, returns DEFAULT instead. This function
returns as secondary value a boolean, which indicates, whether a
matching key/value pair was found (T) or not (NIL)."
  (let ((ctl (hashtree-control map)))
    (hashtrie-get* key (hashtree-node map)
                   (hash-control-hash ctl)
                   (hash-control-test ctl)
                   default)))


(defun hashtree-remove (key map)
  "Removes the association of KEY from hash tree MAP. Returns a 
copy of MAP without the key/value pair. No guarantees are made as
to whether this function returns MAP unchanged or a copy of MAP,
if no matching pair is found; the only guarantee made is, that
the resulting value will be equivalent to MAP. 

This function returns as a secondary value a boolean flag, which
indicates, whether KEY was found and subsequently removed (T) or
not (NIL)."
  (let ((ctl (hashtree-control map)))
    (multiple-value-bind (new-node old-value found)
        (hashtrie-remove* key (hashtree-node map)
           (hashtree-node (hash-control-%empty ctl))
           #'make-node
           (hash-control-hash ctl)
           (hash-control-test ctl))
      (declare (ignore old-value))
      (if (not found) 
          (values map nil)
          (values (%make-hashtree ctl new-node) t)))))        


(defun hashtree-update (key value map)
  "Updates the hash tree MAP, adding an association of KEY to
VALUE. Any previous association of KEY in MAP is replaced. This 
function returns a copy of MAP with the new association as primary
value. The secondary value is one of 

  :added     there was no previous association of KEY in MAP
  :replaced  a former association of KEY has been replaced."
  (let ((ctl (hashtree-control map)))
    (multiple-value-bind (new-node old-value action)
        (hashtrie-update* key value (hashtree-node map)
           #'make-node
           (hash-control-hash ctl)
           (hash-control-test ctl))
      (declare (ignore old-value))
      (values (%make-hashtree ctl new-node) action))))
                 

(defmacro define-hashtree-constructor (name &key (test '#'eql) (hash '#'sxhash))
  "Defines a factory function for hash trees using a dedicated 
pair of equality predicate TEST and hash function HASH. The forms
TEST and HASH are evaluated and must yield funcallable values (i.e.,
symbols or functions).

The primary effect of this macro is the definition of a new 
function NAME, which has the signature (lambda (&rest args) ...).
It must be called with an even number of arguments. The arguments
at indices 2k (for k in 0, 1, ...) are taken to be the keys, and
arguments at indices 2k + 1 are the associated values. The result
of calling NAME is a hash tree containing the key/value pairs 
passed as arguments.

Example:

> (define-hashtree-constructor integer-tree :test #'= :hash #'identity)
INTEGER-TREE

> (integer-tree 1 :one 2 :two 3 :three)
#<HASHTREE ...>

> (hashtree-get 2 *)
:TWO
T
"
  (let ((control (gensym "HASHTREE-CONTROLLER-")))
    `(progn
       (defparameter ,control (make-hash-control ,test ,hash))
       (defun ,name (&rest args)
         (loop
            :with tree := (hash-control-%empty ,control)
            :for link :on args :by #'cddr
            :for key := (car link)
            :for value := (cadr link)
            :do (setf tree (hashtree-update key value tree))
            :finally (return tree)))
       ',name)))


(defparameter *simple-hashtree-controller*
  (make-hash-control #'eql #'sxhash))

(defun hashtree (&rest args)
  "hashtree &rest ARGS => TREE

Constructs a new hash tree using the default test and hash function
pair (i.e., eql and sxhash). Remaining arguments are used to initialize
the new hash tree; an even number of arguments must be supplied. The 
arguments on positions 2k (with k = 0, 1, ...) are taken as the keys
and the values at positions 2k + 1 (with k = 0, 1, ...) are used as
the associated values.

Example:

  (hashtree-get 2 (hashtree 1 :one 2 :two 3 :three))
  ==> :TWO 
  ==> T

This function is provided as convenience constructor for hash trees
using the standard hash control functions."
  (loop
     :with tree := (hash-control-%empty *simple-hashtree-controller*)
     :for link :on args :by #'cddr
     :for key := (car link)
     :for value := (cadr link)
     :do (setf tree (hashtree-update key value tree))
     :finally (return tree)))