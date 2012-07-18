#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Hash Tree
  Copyright (c) 2012 Dirk Esser

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

(in-package "DARTS.LIB.HASHTREE")

(defstruct (node 
             (:copier nil)))

(defstruct (empty
             (:include node)
             (:predicate emptyp)
             (:copier nil)))

(defparameter +empty+ (make-empty))

(defstruct (leaf 
             (:include node)
             (:constructor make-leaf (hash buckets)))
  (hash 0 :type (unsigned-byte 32) :read-only t)
  (buckets () :type list :read-only t))
    
(defstruct (subtable
             (:include node)
             (:constructor make-subtable (mask vector)))
  (mask 0 :type (unsigned-byte 32) :read-only t)
  (vector '#() :type (vector node) :read-only t))

(defparameter +root+
  (make-subtable 0 '#()))

(defstruct (hash-control
             (:copier nil)
             (:predicate hash-control-p)
             (:constructor make-hash-control (test hash)))
  (test (error "missing equality test") :read-only t)
  (hash (error "missing hash function") :read-only t))

(defstruct (hashtree 
             (:copier nil)
             (:predicate hashtreep)
             (:constructor %make-hashtree (control &optional (node +empty+))))
  (control (error "missing hash control (bug)") :read-only t)
  (node (error "missing root node (bug)") :read-only t))


(defmethod print-object ((ob hashtree) stream)
  (print-unreadable-object (ob stream :type t)
    (format stream "~S ~S ~S ~S ~S ~D"
       :test (hash-control-test (hashtree-control ob))
       :hash (hash-control-hash (hashtree-control ob))
       :count (hashtree-count ob))))


(defun make-hashtree (&key (test #'eql) (hash #'sxhash))
  "Creates a new hash tree. The hash tree will use the function supplied
as the value of the :test parameter as its equality test predicate, and
the value passed as :hash as its hash function. If omitted, the default
equality test is eql, and the default hash function is sxhash."
  (%make-hashtree (make-hash-control test hash)))


(defun hashtree-map (function map)
  "Maps a function across all elements in a hashtree. This function applies
FUNCTION to each element in MAP; FUNCTION must accept two arguments and will
be called with an element's key as first, and its associated value as second
argument. The result of this function is undefined."
  (hashtree-fold #'(lambda (key value unused) (funcall function key value) unused) nil map))


(defun hashtree-fold (function initial-value map)
  (declare (dynamic-extent function))
  (labels
      ((invoke (seed pair)
         (funcall function (car pair) (cdr pair) seed))
       (walk (seed node)
         (etypecase node
           (empty seed)
           (subtable (reduce #'walk (subtable-vector node) :initial-value seed))
           (leaf (reduce #'invoke (leaf-buckets node) :initial-value seed)))))
    (walk initial-value (hashtree-node map))))

(defun hashtree-keys (map)
  (hashtree-fold 
    (lambda (key value list) (declare (ignore value)) (cons key list))
    nil
    map))

(defun hashtree-values (map)
  (hashtree-fold
    (lambda (key value list) (declare (ignore value)) (cons value list))
    nil
    map))

(defun hashtree-pairs (map)
  (hashtree-fold
    (lambda (key value list) (cons (cons key value) list))
    nil 
    map))

(defmacro do-hashtree ((key value) tree &body body)
  `(hashtree-map #'(lambda (,key ,value) ,@body) ,tree))


(defun hashtree-count (map)
  "Counts the number of entries in a hash tree."
  (hashtree-fold
    (lambda (key value count) (declare (ignore key value)) (1+ count))
    0 
    map))

(defun hashtree-test (map)
  "Obtains the test function used by the given hash tree"
  (hash-control-test (hashtree-control map)))

(defun hashtree-hash (map)
  "Obtains the hash function used by the given hash tree"
  (hash-control-hash (hashtree-control map)))

(defun hashtree-empty-p (map)
  "Tests, whether the given hash tree is empty."
  (emptyp (hashtree-node map)))

(declaim (ftype (function (t function) (values (unsigned-byte 32))) compute-hash)
         (inline compute-hash))

(defun compute-hash (value fn)
  (logand #xffffffff (funcall fn value)))

(defun hashtree-get (key map &optional default)
  "Obtains the value associated with a given key."
  (let ((root (hashtree-node map)))
    (if (emptyp root) 
        (values default nil)
        (let* ((control (hashtree-control map))
               (test (hash-control-test control))
               (hash (hash-control-hash control))
               (full-hash (compute-hash key hash)))
          (labels
              ((lookup (node code)
                 (etypecase node
                   (empty (values default nil))
                   (leaf 
                    (if (/= code (leaf-hash node))
                        (values default nil)
                        (loop 
                           :for (k . v) :in (leaf-buckets node)
                           :when (funcall test k key)
                           :do (return (values v t))
                           :finally (return (values default nil)))))
                   (subtable 
                    (let* ((nx (logand code #b11111))
                           (bt (ash 1 nx))
                           (tm (subtable-mask node)))
                      (if (zerop (logand tm bt))
                          (values default nil)
                          (let* ((emask (- bt 1))
                                 (count (logcount (logand emask tm))))
                            (lookup (aref (subtable-vector node) count) 
                                    (ash code -5)))))))))
            (lookup root full-hash))))))

(defun hashtree-remove (key map)
  (let* ((control (hashtree-control map))
         (test (hash-control-test control))
         (hash (hash-control-hash control)))
    (labels
        ((copy-except (position table-vector)
           (let* ((new-length (- (length table-vector) 1))
                  (new-vector (make-array new-length)))
             (loop
                :for k :upfrom 0 :below position
                :do (setf (aref new-vector k) (aref table-vector k)))
             (loop
                :for k :upfrom position :below new-length
                :do (setf (aref new-vector k) (aref table-vector (1+ k))))
             new-vector))
         (copy-replace (position table-vector element)
           (let* ((length (length table-vector))
                  (new-vector (make-array length)))
             (loop
                :for k :upfrom 0 :below position
                :do (setf (aref new-vector k) (aref table-vector k)))
             (setf (aref new-vector position) element)
             (loop
                :for k :upfrom (1+ position) :below length
                :do (setf (aref new-vector k) (aref table-vector k)))
             new-vector))
         (hunt-down (node code)
           (etypecase node
             (empty node)
             (leaf 
               (if (/= code (leaf-hash node))
                   node
                   (let* ((old-list (leaf-buckets node))
                          (pair (assoc key old-list :test test)))
                     (if (not pair) 
                         node
                         (let ((new-list (remove pair old-list :test #'eq)))
                           (if (null new-list)
                               +empty+
                               (make-leaf (leaf-hash node) new-list)))))))
             (subtable 
               (let* ((index (logand code #b11111))
                      (bit (ash 1 index))
                      (table-mask (subtable-mask node)))
                 (if (zerop (logand bit table-mask))
                     node
                     (let* ((dispatch (logcount (logand table-mask (- bit 1))))
                            (table-vector (subtable-vector node))
                            (old-child (aref table-vector dispatch))
                            (new-child (hunt-down old-child (ash code -5))))
                       (cond 
                         ((eq old-child new-child) node)
                         ((not (emptyp new-child)) 
                          (let ((new-vector (copy-replace dispatch table-vector new-child)))
                            (make-subtable table-mask new-vector)))
                         ((= (length table-vector) 1) +empty+)
                         (t (let* ((new-mask (logand table-mask (lognot bit)))
                                   (new-vector (copy-except dispatch table-vector)))
                              (make-subtable new-mask new-vector)))))))))))
      (let* ((old-root (hashtree-node map))
             (new-root (hunt-down old-root (compute-hash key hash))))
        (if (eq old-root new-root) 
            (values map nil)
            (values (%make-hashtree control new-root) t))))))


(defun hashtree-update (key value map)
  (let* ((control (hashtree-control map))
         (test (hash-control-test control))
         (hash (hash-control-hash control)))
    (labels
        ((extend-leaf (node)
           (let* ((old-list (leaf-buckets node))
                  (present (assoc key old-list :test test))
                  (new-list  (if present
                                 (substitute (cons key value) present old-list :count 1 :test #'eq)
                                 (cons (cons key value) old-list))))
             (values (make-leaf (leaf-hash node) new-list)
                     (if present :replaced :added))))
         
         (split-leaf (node)
           (let* ((old-code (leaf-hash node))
                  (old-index (logand old-code #b11111))
                  (old-bit (ash 1 old-index))
                  (old-buckets (leaf-buckets node))
                  (new-code (ash old-code -5))
                  (new-subtable (make-subtable old-bit (vector (make-leaf new-code old-buckets)))))
             new-subtable))

         (inject-into (vector position element)
           (let* ((length (length vector))
                  (new-vector (make-array (1+ length))))
             (loop
                :for k :upfrom 0 :below position
                :do (setf (aref new-vector k) (aref vector k)))
             (setf (aref new-vector position) element)
             (loop
                :for k :upfrom position :below length
                :do (setf (aref new-vector (1+ k)) (aref vector k)))
             new-vector))                 

         (insert (node code)
           (etypecase node
             (empty (values (make-leaf code (list (cons key value))) :added))
             (subtable 
               (let* ((index (logand #b11111 code))
                      (bit (ash 1 index))
                      (table-mask (subtable-mask node))
                      (table-vector (subtable-vector node)))
                 (if (zerop (logand table-mask bit))
                     (let* ((new-item (make-leaf (ash code -5) (list (cons key value))))
                            (new-mask (logior table-mask bit))
                            (new-position (logcount (logand new-mask (- bit 1))))
                            (new-vector (inject-into table-vector new-position new-item)))
                       (values (make-subtable new-mask new-vector) t))
                     (let* ((table-index (logcount (logand table-mask (- bit 1))))
                            (child-node (aref table-vector table-index)))
                       (multiple-value-bind (new-child added) (insert child-node (ash code -5))
                         (let ((new-vector (copy-seq table-vector)))
                           (setf (aref new-vector table-index) new-child)
                           (values (make-subtable table-mask new-vector) added)))))))
             (leaf 
               (if (= code (leaf-hash node))
                   (extend-leaf node)
                   (insert (split-leaf node) code))))))

      (multiple-value-bind (new-root added) (insert (hashtree-node map) (compute-hash key hash))
        (values (%make-hashtree control new-root)
                added)))))
                 
