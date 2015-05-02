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


;;; Helpers
;;;

(defmacro recurr (name bindings &body body)
  (let* ((names (mapcar #'car bindings))
         (restart (gensym "RESTART-"))
         (temp1 (gensym))
         (temp2 (gensym))
         (shadows (mapcar (lambda (name) (declare (ignore name)) (gensym)) names)))
    `(block ,name
       (let ,bindings
         (macrolet ((,name ,shadows
                      (list 'progn
                            (cons 'psetq
                               (loop
                                  :for ,temp1 :in ',names
                                  :for ,temp2 :in (list ,@shadows)
                                  :nconcing (list ,temp1 ,temp2)))
                            (list 'go ',restart))))
           (tagbody
              ,restart
              (progn ,@body)))))))                




;;; Substrate
;;;
;;; A hashtrie is composed of nodes. Each node is either
;;;
;;; - the empty node (payload is nil)
;;; - a leaf node (payload is a list)
;;; - a dispatch node (payload is a vector)
;;;
;;; The tag of a node is
;;;
;;; - a bitmask (in case of dispatch nodes)
;;; - the remaining portion of the hash (leaf nodes)
;;; - ignored (empty node)
;;;
;;; Specialized hash tries inherit (via :include) from
;;; node. By using a very special conc-name, we try not
;;; to pollute the caller's namespace.

(defstruct (node
             (:conc-name nil)
             (:constructor make-node (tag payload))
             (:predicate nodep))
  (node-tag 0 :type (unsigned-byte 32) :read-only t)
  (node-payload nil :read-only t))

(defmethod print-object ((ob node) stream)
  (print-unreadable-object (ob stream :type t)
    (let ((payload (node-payload ob)))
      (cond
        ((null payload) (prin1 :empty stream))
        ((vectorp payload) (format stream "~S ~B" :dispatch (node-tag ob)))
        (t (format stream "~S ~D" :leaf (length payload)))))))

(declaim (ftype (function (node) (values boolean)) node-empty-p node-leaf-p node-dispatch-p)
         (inline node-empty-p node-leaf-p node-dispatch-p))

(defun node-empty-p (node)
  (null (node-payload node)))

(defun node-leaf-p (node)
  (listp (node-payload node)))

(defun node-dispatch-p (node)
  (vectorp (node-payload node)))


;;; Pattern matching on nodes. Evaluates NODE-FORM and 
;;; dispatches to the appropriate clause depending on the
;;; type of node (empty, dispatch, leaf). Each clause is
;;; of the form (KEY (HASH PAYLOAD) BODY...) where KEY
;;; is one of the keywords
;;;
;;; :empty, :leaf, :dispatch
;;;
;;; HASH is a variable which will be bound to the value 
;;; of the `tag` field of the node, and PAYLOAD is a variable
;;; which will be bound to the PAYLOAD field of the node.
;;;
;;; Either of HASH and PAYLOAD may be _, in which case 
;;; no binding is established for that node member.

(defmacro node-type-case (node-form &body clauses)
  (let ((empty-clause nil)
        (dispatch-clause nil)
        (leaf-clause nil))
    (loop
       :for clause :in clauses
       :do (destructuring-bind (keyword (&optional (p1 '_) (p2 '_)) &rest body) clause
             (ecase keyword
               ((:empty) 
                (when empty-clause (error "multiple :EMPTY clauses found"))
                (unless (and (string= p1 "_") (string= p2 "_")) (error "empty nodes have nothing to bind"))
                (setf empty-clause (cons (cons '_ '_) body)))
               ((:leaf)
                (when leaf-clause (error "multiple :LEAF clauses found"))
                (setf leaf-clause (cons (cons p1 p2) body)))
               ((:dispatch)
                (when dispatch-clause (error "multiple :DISPATCH clauses found"))
                (setf dispatch-clause (cons (cons p1 p2) body))))))
    (let* ((payload (gensym))
           (node (gensym))
           (seed `(error "BUG: not all bases were covered: ~S" ,payload)))
      (labels ((full-binder (clause &optional type-decl)
                 (let* ((params (car clause))
                        (body (cdr clause))
                        (hash-var (car params))
                        (payload-var (cdr params)))
                   (cond
                     ((string= hash-var "_")
                      (if (string= payload-var "_")
                          `(progn ,@body)
                          `(let ((,payload-var ,payload)) 
                             ,@(when type-decl (list `(declare (type ,type-decl ,payload-var))))
                             ,@body)))
                     ((string= payload-var "_") 
                      `(let ((,hash-var (node-tag ,node)))
                         (declare (type (unsigned-byte 32) ,hash-var))
                         ,@body))
                     (:else `(let ((,payload-var ,payload) 
                                   (,hash-var (node-tag ,node)))
                               (declare (type (unsigned-byte 32) ,hash-var)
                                        ,@(when type-decl (list `(type ,type-decl ,payload-var))))
                               ,@body))))))

      (when dispatch-clause
        (setf seed 
              `(if (vectorp ,payload)
                   ,(full-binder dispatch-clause 'simple-vector)
                   ,seed)))
      (when leaf-clause
        (setf seed
              `(if (consp ,payload)
                   ,(full-binder leaf-clause 'list)
                   ,seed)))
      (when empty-clause
        (setf seed 
              `(if (null ,payload)
                   ,(full-binder empty-clause 'null)
                   ,seed)))
      `(let* ((,node ,node-form)
              (,payload (node-payload ,node)))
         ,seed)))))



;;; The following operations are generic in the sense, that
;;; they don't need to know anything about the hash function
;;; and/or the test. So we provide them once, for all actual
;;; derivations.


(defun hashtriep (value)
  "Tests, whether VALUE is a hash trie of any flavour. Yields
true, if that's the case, and false otherwise."
  (nodep value))


(defun hashtrie-empty-p (value)
  "Tests, whether the hash trie VALUE is empty. Yields true,
if that's the case, and false otherwise."
  (node-empty-p value))


(defun hashtrie-fold (initial-value func node)
  "Invokes FUNC for each key/value pair in hash trie NODE, 
in order to produce a summary value. The function is called
with three arguments: 

- the current summary value, i.e., the value returned by
  the function on the previous invocation, or INITIAL-VALUE,
  if this is the first invocation
- the key of the key/value pair
- the associated value of the key/value pair

This function returns the final summary value of FUNC, i.e.,
the value returned by FUNC on the last invocation made, or
INITIAL-VALUE, if the trie is empty.

Note, that the order of key/value pairs in the traversal
is unspecified but consistent, i.e., two consecutive calls
of this function with the same has trie will visit the pairs
in the same order."
  (declare (optimize (speed 3) (debug 0))
           (dynamic-extent func))
  (labels
      ((invoke (seed pair) (funcall func seed (car pair) (cdr pair)))
       (walk-reduce (seed node)
         (node-type-case node
           (:empty () seed)
           (:leaf (_ payload) (reduce #'invoke payload :initial-value seed))
           (:dispatch (_ payload) (reduce #'walk-reduce payload :initial-value seed)))))
    (walk-reduce initial-value node)))


(defun hashtrie-count (node)
  "Counts the key/value pairs in hash trie NODE. Returns a 
single integer value, which represents the number of pairs
present."
  (declare (optimize (speed 3) (debug 0)))
  (labels 
      ((count-em (node)
         (node-type-case node
           (:dispatch (_ payload) (reduce #'+ payload :initial-value 0 :key #'count-em))
           (:empty () 0)
           (:leaf (_ payload) (length payload)))))
    (count-em node)))


(defun hashtrie-map (func node)
  "Applies FUNC to each key/value pair in the trie NODE. For
each key/value pair present in NODE, FUNC is called with the
key as first, and the value as second argument. The return 
value of FUNC is ignored.

The return value of this function is unspecified.

Note, that the order of key/value pairs in the traversal
is unspecified but consistent, i.e., two consecutive calls
of this function with the same has trie will visit the pairs
in the same order."
  (declare (optimize (speed 3) (debug 0))
           (dynamic-extent func))
  (labels
      ((invoke (pair) (funcall func (car pair) (cdr pair)))
       (walk (node)
         (node-type-case node
           (:empty () nil)
           (:leaf (_ payload) (map nil #'invoke payload))
           (:dispatch (_ payload) (map nil #'walk payload)))))
    (walk node)))


(defun hashtrie-keys (trie)
  (hashtrie-fold () (lambda (list key value) (declare (ignore value)) (cons key list)) trie))

(defun hashtrie-values (trie)
  (hashtrie-fold () (lambda (list key value) (declare (ignore key)) (cons value list)) trie))

(defun hashtrie-pairs (trie)
  (hashtrie-fold () (lambda (list key value)  (cons (cons key value) list)) trie))

(defmacro do-hashtrie ((key value trie) &body body)
  `(block nil (hashtrie-map (lambda (,key ,value) ,@body) ,trie)))



(defun hashtrie-find-1 (key key-hash tree test default)
  (declare (dynamic-extent test))
  (recurr hunt-down ((node tree) 
                     (hash (logand #xffffffff key-hash)))
    (node-type-case node
      (:empty () (return-from hunt-down (values default nil)))
      (:leaf (code alist)
        (if (/= code hash)
            (return-from hunt-down (values default nil))
            (let ((pair (assoc key alist :test test)))
              (if pair 
                  (return-from hunt-down (values (cdr pair) t))
                  (return-from hunt-down (values default nil))))))
      (:dispatch (mask table)
        (let* ((nx (logand hash #b11111))
               (bt (ash 1 nx)))
          (if (zerop (logand mask bt))
              (return-from hunt-down (values default nil))
              (let* ((emask (- bt 1))
                     (count (logcount (logand emask mask))))
                (hunt-down (svref table count)
                           (ash hash -5)))))))))


(defun hashtrie-update-1 (key key-hash value map make-node test)
  (declare (optimize (speed 3) (debug 0))
           (dynamic-extent test)
           (type node map)
           (type (function ((unsigned-byte 32) t) (values node)) make-node))
  (labels
      ((make-node (arg1 arg2) (funcall make-node arg1 arg2))
       (extend-leaf (node)
         (let* ((old-list (node-payload node))
                (present (assoc key old-list :test test))
                (new-list  (if present
                                 (substitute (cons key value) present old-list :count 1 :test #'eq)
                                 (cons (cons key value) old-list))))
           (values (make-node (node-tag node) new-list)
                   (if present (cdr present) nil)
                   (if present :replaced :added))))
         (split-leaf (node)
           (let* ((old-code (node-tag node))
                  (old-index (logand old-code #b11111))
                  (old-bit (ash 1 old-index))
                  (old-buckets (node-payload node))
                  (new-code (ash old-code -5))
                  (new-subtable (make-node old-bit (vector (make-node new-code old-buckets)))))
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
           (declare (type (unsigned-byte 32) code))
           (node-type-case node
             (:empty () (values (make-node code (list (cons key value))) :added))
             (:leaf (lhash _)
               (if (= lhash code)
                   (extend-leaf node)
                   (insert (split-leaf node) code)))
             (:dispatch (table-mask table-vector)
               (let* ((index (logand #b11111 code))
                      (bit (ash 1 index)))
                 (if (zerop (logand table-mask bit))
                     (let* ((new-item (make-node (ash code -5) (list (cons key value))))
                            (new-mask (logior table-mask bit))
                            (new-position (logcount (logand new-mask (- bit 1))))
                            (new-vector (inject-into table-vector new-position new-item)))
                       (values (make-node new-mask new-vector) nil :added))
                     (let* ((table-index (logcount (logand table-mask (- bit 1))))
                            (child-node (svref table-vector table-index)))
                       (multiple-value-bind (new-child old-value added) (insert child-node (ash code -5))
                         (let ((new-vector (copy-seq table-vector)))
                           (setf (svref new-vector table-index) new-child)
                           (values (make-node table-mask new-vector) old-value added))))))))))
    (insert map (logand #xffffffff key-hash))))


(defun hashtrie-remove-1 (key key-hash map empty-node make-node test)
  (declare (optimize (speed 3) (debug 0)) 
           (dynamic-extent test))
  (labels
      ((copy-except (position table-vector)
         (let* ((new-length (- (length table-vector) 1))
                (new-vector (make-array new-length)))
           (loop
              :for k :upfrom 0 :below position
              :do (setf (svref new-vector k) (svref table-vector k)))
           (loop
              :for k :upfrom position :below new-length
              :do (setf (svref new-vector k) (svref table-vector (1+ k))))
           new-vector))
       (copy-replace (position table-vector element)
         (let* ((length (length table-vector))
                (new-vector (make-array length)))
           (loop
              :for k :upfrom 0 :below position
              :do (setf (svref new-vector k) (svref table-vector k)))
           (setf (svref new-vector position) element)
           (loop
              :for k :upfrom (1+ position) :below length
              :do (setf (svref new-vector k) (svref table-vector k)))
           new-vector))
       (make-node (a b) (funcall make-node a b))
       (hunt-down (node code)
         (declare (type (unsigned-byte 32) code))
         (node-type-case node
           (:empty () (values node nil nil))
           (:leaf (leaf-hash leaf-buckets)
             (if (/= code leaf-hash)
                 (values node nil nil)
                 (let* ((old-list leaf-buckets)
                        (pair (assoc key old-list :test test)))
                   (if (not pair)
                       (values node nil nil)
                       (let ((new-list (remove pair old-list :test #'eq)))
                         (if (null new-list)
                             (values empty-node (cdr pair) t)
                             (values (make-node leaf-hash new-list)
                                     (cdr pair) t)))))))
           (:dispatch (table-mask table-vector)
             (let* ((index (logand code #b11111))
                    (bit (ash 1 index)))
               (if (zerop (logand bit table-mask))
                   (values node nil nil)
                   (let* ((dispatch (logcount (logand table-mask (- bit 1))))
                          (old-child (aref table-vector dispatch)))
                     (multiple-value-bind (new-child old-value found) (hunt-down old-child (ash code -5))
                       (cond 
                         ((not found) (values node nil nil))
                         ((not (node-empty-p new-child)) 
                          (let ((new-vector (copy-replace dispatch table-vector new-child)))
                            (values (make-node table-mask new-vector)
                                    old-value t)))
                         ((= (length table-vector) 1) (values empty-node old-value t))
                         (t (let* ((new-mask (logand table-mask (lognot bit)))
                                   (new-vector (copy-except dispatch table-vector)))
                              (values (make-node new-mask new-vector)
                                      old-value t))))))))))))
    (hunt-down map (logand #xffffffff key-hash))))


(defgeneric hashtrie-find (key trie &optional default)
  (:argument-precedence-order trie key))

(defgeneric hashtrie-update (key value trie)
  (:argument-precedence-order trie key value))

(defgeneric hashtrie-remove (key trie)
  (:argument-precedence-order trie key))
  


(defmacro define-hashtrie (name &body options &environment env)
  (labels
      ((the-only-form (clause)
         (if (and (cadr clause) (null (cddr clause)))
             (cadr clause)
             (error "malformed ~S clause ~S" 'define-hashtrie clause)))
       (the-bindable-symbol (value usage)
         (if (and (symbolp value) (not (keywordp value)) (not (constantp value env)))
             value
             (error "~S cannot be used as ~A" value usage)))
       (string-concat-1 (value buffer)
         (cond
           ((characterp value) (vector-push-extend value buffer))
           ((symbolp value) (string-concat-1 (symbol-name value) buffer))
           ((typep value 'sequence) (map nil (lambda (value) (string-concat-1 value buffer)) value))
           (t (error 'simple-type-error
                     :datum value :expected-type '(or character symbol string sequence)
                     :format-control "cannot create a string from ~S" 
                     :format-arguments (list value)))))
       (string-concat (&rest value)
         (let ((buffer (make-array 16 :element-type 'character :adjustable t :fill-pointer 0)))
           (string-concat-1 value buffer)
           (coerce buffer 'simple-string))))
    (let* ((test-function nil)
           (hash-function nil)
           (predicate-name nil)
           (constructor-name nil)
           (documentation-string nil)
           (compile-time-info (get name 'hashtrie-compile-time-info))
           (empty-variable (if compile-time-info (first compile-time-info) (gensym (string-concat "*EMPTY-" name "*"))))
           (node-constructor (if compile-time-info (second compile-time-info) (gensym (string-concat "MAKE-" name "-NODE")))))
      (loop
        :for clause :in options
        :for (key . more-forms) := clause
        :do (ecase key
              ((:documentation) (setf documentation-string (the-only-form clause)))
              ((:test) (setf test-function (the-only-form clause)))
              ((:hash) (setf hash-function (the-only-form clause)))
              ((:predicate) (setf predicate-name (the-bindable-symbol (the-only-form clause) "predicate name")))
              ((:constructor) (setf constructor-name (the-bindable-symbol (the-only-form clause) "constructor name")))))
      (let ((actual-constructor (if (null constructor-name) (intern (string-concat "MAKE-" name)) constructor-name))
            (actual-predicate (if (null predicate-name) (intern (string-concat name (if (position #\- (symbol-name name)) "-P" "P"))) predicate-name))
            (trie (gensym))
            (key (gensym))
            (value (gensym))
            (default (gensym))
            (pairs (gensym)))
        `(progn
           
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (setf (get ',name 'hashtrie-compile-time-info) (list ',empty-variable ',node-constructor)))

           (defstruct (,name (:conc-name nil)
                             (:include node)
                             (:predicate ,actual-predicate)
                             (:constructor ,node-constructor (node-tag node-payload)))
             ,@(when documentation-string
                 (list documentation-string)))
           
           (defparameter ,empty-variable (,node-constructor 0 ()))
           
           (defun ,actual-constructor (&rest ,pairs)
             (loop
               :with ,trie := ,empty-variable
               :for (,key ,value) :on ,pairs :by #'cddr
               :do (setf ,trie (hashtrie-update-1 ,key (,hash-function ,key) ,value ,trie #',node-constructor #',test-function))
               :finally (return ,trie)))

           (defmethod hashtrie-update (,key ,value (,trie ,name))
             (hashtrie-update-1 ,key (,hash-function ,key) ,value ,trie #',node-constructor #',test-function))

           (defmethod hashtrie-find (,key (,trie ,name) &optional ,default)
             (hashtrie-find-1 ,key (,hash-function ,key) ,trie #',test-function ,default))

           (defmethod hashtrie-remove (,key (,trie ,name))
             (hashtrie-remove-1 ,key (,hash-function ,key) ,trie ,empty-variable #',node-constructor #',test-function)))))))

           
           
(define-hashtrie simple-hashtrie
  (:constructor simple-hashtrie)
  (:hash sxhash)
  (:test eql))

  


