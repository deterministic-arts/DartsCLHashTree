#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Hash Tree
  Copyright (c) 2013, 2015, 2018 Dirk Esser

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


(in-package "DARTS.LIB.WBTREE")

(defconstant +weight+ 4)


(defstruct (wbtree 
             (:conc-name nil)
             (:copier nil)
             (:predicate wbtreep))
  "Weight balanced binary tree. This structure defines the basic
   building blocks for the nodes of a WB tree. Each node consists
   of the following information:

   - key: the node's key value. The value of this field is undefined
     for the node representing an empty tree. 
   - value: the value associated with `key'. The actual value is
     undefined, if the node is representing the empty tree.
   - count: number of key/value pairs in this node, i.e., 0, if this
     is the empty node, and 1 + left-count + right-count otherwise.
   - left: the left subtree. Either the empty tree, or a proper WB
     tree such, that all keys are \"less than\" this node's key.
   - right: the right subtree. Either the empty tree, or a proper WB
     tree such, that all keys are \greater than\" this node's key.

   Applications deal only with struct types, which are derived from
   this one via `:include'."
  (node-key nil :read-only t)
  (node-value nil :read-only t)
  (node-count 0 :read-only t)
  (node-left nil :read-only t)
  (node-right nil :read-only t))


(defmethod print-object ((ob wbtree) stream)
  (print-unreadable-object (ob stream :type t)
    (if (zerop (node-count ob))
        (write-string "EMPTY" stream)
        (format stream "~S ~S ~D" 
                (node-key ob) (node-value ob)
                (node-count ob)))))


(defmacro with-node ((key value count left right) node &body body)
  (let ((node-var (gensym "NODE-")))
    `(let ((,node-var ,node))
       (declare (ignorable ,node-var))
       (let (,@(unless (string= "_" key) (list `(,key (node-key ,node-var))))
             ,@(unless (string= "_" value) (list `(,value (node-value ,node-var))))
             ,@(unless (string= "_" count) (list `(,count (node-count ,node-var))))
             ,@(unless (string= "_" left) (list `(,left (node-left ,node-var))))
             ,@(unless (string= "_" right) (list `(,right (node-right ,node-var)))))
         ,@body))))


(defun wbtree-empty-p (node)
  "Tests, whether `node' represents an empty tree."
  (zerop (node-count node)))

(defun wbtree-size (node)
  "Answers the number of valid key/value pairs contained in `tree'"
  (node-count node))

(defun wbtree-node-value (node)
  "Obtains the value associated with `node'. If `node' is the empty
   tree, raises a condition of type `simple-error'."
  (if (zerop (node-count node)) (error "attempt to get value of empty node")
      (node-value node)))

(defun wbtree-node-key (node)
  "Obtains the key associated with `node' If `node' is the empty
   tree, raises a condition of type `simple-error'."
  (if (zerop (node-count node)) (error "attempt to get key of empty node")
      (node-key node)))

(defun wbtree-node-left-subtree (node)
  "Obtains the left subtree of `node' or `node' itself, if it is 
   the empty tree"
  (if (zerop (node-count node)) node
      (node-left node)))

(defun wbtree-node-right-subtree (node)
  "Obtains the right subtree of `node' or `node' itself, if it is 
   the empty tree"
  (if (zerop (node-count node)) node
      (node-right node)))


(defmacro with-function ((name &optional (init-form name)) &body body)
  (let ((temp (gensym))
        (args (gensym)))
    `(let ((,temp ,init-form))
       (macrolet ((,name (&rest ,args) (list* 'funcall ',temp ,args)))
         (let ((,name ,temp))
           (declare (ignorable ,name))
           ,@body)))))


(defun wbtree-minimum-node (tree)
  "Answers the node with the smallest (in the sense of the subtype's
   comparison function) key present in `tree'."
  (if (wbtree-empty-p tree)
      nil
      (loop 
         :for previous = tree :then current
         :for current = (node-left tree) :then (node-left current)
         :until (wbtree-empty-p current)
         :finally (return previous))))


(defun wbtree-maximum-node (tree)
  "Answers the node with the largest (in the sense of the subtype's
   comparison function) key present in `tree'."
  (if (wbtree-empty-p tree)
      nil
      (loop
         :for previous = tree :then current
         :for current = (node-right tree) :then (node-right current)
         :until (wbtree-empty-p current)
         :finally (return previous))))


(defun wbtree-ceiling-node-1 (key tree lessp)
  (with-function (lessp lessp)
    (labels ((walk (nd best)
               (if (wbtree-empty-p nd) best
                   (with-node (nkey _ _ left right) nd
                     (cond
                       ((lessp nkey key) (walk right best))
                       ((lessp key nkey) (walk left nd))
                       (t nd))))))
      (walk tree nil))))


(defun wbtree-floor-node-1 (key tree lessp)
  (with-function (lessp lessp)
    (labels ((walk (nd best)
               (if (wbtree-empty-p nd) best
                   (with-node (nkey _ _ left right) nd
                     (cond
                       ((lessp nkey key) (walk right nd))
                       ((lessp key nkey) (walk left best))
                       (t nd))))))
      (walk tree nil))))


(defun wbtree-find-node-1 (key tree lessp)
  (with-function (lessp lessp)
    (labels ((walk (node)
               (if (wbtree-empty-p node) nil
                   (let ((ref (node-key node)))
                     (cond
                       ((lessp key ref) (walk (node-left node)))
                       ((lessp ref key) (walk (node-right node)))
                       (t node))))))
      (walk tree))))


(defgeneric wbtree-floor-node (key tree)
  (:argument-precedence-order tree key)
  (:documentation "Answers the node in `tree`, whose key is the largest key
    less than or equal to `key`. Returns nil, if there is no such node in the
    given tree."))

(defgeneric wbtree-ceiling-node (key tree)
  (:argument-precedence-order tree key)
  (:documentation "Answers the node in `tree`, whose key is the smallest key
    greater than or equal to `key`. Returns nil, if there is no such node in
    the given tree."))

(defgeneric wbtree-find-node (key tree)
  (:argument-precedence-order tree key)
  (:documentation "Answers the node of `tree`, whose key matches the given
    `key` value or nil, if no matching node exists."))

(defgeneric wbtree-update (key value tree &optional test)
  (:argument-precedence-order tree key value)
  (:documentation "Returns a copy of tree, in which `key' has been associated with
    `value', potentially replacing any previous binding for `key' in
    `tree'. If there is already an association of `key' in tree with
    a value equal to `value' (which is determined via predicate 
    function `test'), then the original tree is returned instead. The
    default value of `test' is eql.

    This function returns a secondary value, which indicates, which
    changes have been performed in the resulting tree when compared
    to the original `tree'. Possible values are:

      - `nil', if there was already a suitable association for `key' and
        `value' in `tree'.
      - `t', if the key was not present in `tree', and a new assocation
        has been added.
      - a node object, which is the node, that has been replaced by
        one containing the new association."))

(defgeneric wbtree-remove (key tree)
  (:argument-precedence-order tree key)
  (:documentation "Answers a copy of `tree', in which any association of `key' 
    has been removed. Returns `tree' instead, if there is no entry
    matching `key' in `tree'.

    This function returns as secondary the WB tree node, which
    has been removed in the copy returned as primary value, or nil,
    if no matching node was found."))

(defgeneric wbtree-fold (function tree 
                         &key direction associativity
                              initial-value start
                              end)
  (:argument-precedence-order tree function)
  (:documentation "Applies `function' to all nodes in `tree'. 

    If `direction' is `:forward', then only those nodes are visited, 
    whose keys are greater than or equal to `start' and less than `end';
    traversal will be in proper tree order. This is the default.

    If `direction' is `:backward', then only those nodes are visited,
    whose keys are less then or equal to `start', and greater then `end',
    and the traversal will happen in the opposite of the normal tree
    order.

    If `associativity' is `:left' (the default), the function is called
    with the value of its last invocation as first, and the current 
    node as second argument. If `associativity' is `:right', then the
    arguments are exchanged, i.e., the node will be the first argument,
    and the accumulated value will be the second. On the first invocation,
    the function receives the `initial-value' as accumulator value.

    This function returns the last value returned by `function', or
    `initial-value', if the function is not called at all (e.g., because
    the selected key range is empty).

    Example:

      (wbtree-fold #'cons some-tree :associativity :right :direction :backward)

    will yield a list of all tree nodes in proper tree order."))

(defgeneric wbtree-map (function tree 
                        &key direction collectp start
                             end)
  (:argument-precedence-order tree function)
  (:documentation "Maps `function' across all nodes of `tree'. If the value of
    `direction' is :forward (the default), then the nodes are visited
    in proper tree order (i.e., in ascending key order as determined
    by the tree's comparison function). If the value is :backward, then
    the nodes are visited in the opposite order.

    If `start' is provided, traversal starts at the smallest key, which
    is greater than or equal to `start', when direction is :forward, or
    with the node, whose key is the largest still less than or equal to 
    `start', if `direction' is :backward. The start node is passed to
    `function'.

    If `end' is provided, traversal stops at the node with smallest key,
    which is larger than or equal to `end' when iterating in :forward
    direction, or at the node with the largest key still smaller than or
    equal to `end', when iterating backwards. In either case, the stop
    node is not passed down to `function'.

    If `collectp', the primary return values of each invocation of 
    `function' are collected into a list, which is then returned as 
    primary (and only) value of `wbtree-map' itself. If not `collectp',
    return values of `function' are ignored, and wbtree-map returns nil."))

(defgeneric wbtree-union (tree1 tree2 &key combiner)
  (:argument-precedence-order tree1 tree2)
  (:documentation "Answers the tree, which is the union of `tree1' and `tree2',
    which must both be WB trees of the same subtype. The resulting
    tree contains entries for all keys, which are present in either
    `tree1' or `tree2' (or both). If a key is present in both trees,
    the resulting tree will associate it with the value, which is
    chosen by function `combiner', which must accept three arguments:

      1. the key present in both trees
      2. the associated value in `tree1'
      3. the associated value in `tree2'.

    The value returned by `combiner' will then be used as the value
    to associate with the overlapping key in the result tree. The
    default combiner function always yields the value in `tree2', and
    discards the value from `tree1'."))

(defgeneric wbtree-intersection (tree1 tree2 &key combiner)
  (:argument-precedence-order tree1 tree2)
  (:documentation "Answers the tree, which is the intersection of `tree1' and 
   `tree2', which must both be WB trees of the same subtype. The 
   resulting tree contains entries for all keys, which are present 
   in `tree1' as well as `tree2'.

   The `combiner' function determines, which value will be associated
   with the keys in the resulting tree. It is called with three arguments,
   the key, the associated value in `tree1', and the associated value in
   `tree2'. Whatever value it returns will be used as the value for the
   key in the resulting tree.

   The default combiner function always answers the value from `tree2',
   discarding the value in `tree1'."))

(defgeneric wbtree-difference (tree1 tree2)
  (:argument-precedence-order tree1 tree2)
  (:documentation "Answers a copy of `tree1', in which all entries have been
    removed, which match keys present in `tree2'."))

(defgeneric wbtree-equal (tree1 tree2 &key test node-test)
  (:argument-precedence-order tree1 tree2))


(defun wbtree-find (key tree &optional default)
  "Finds the value associated with `key' in `tree'. This function
   returns two values: the value associated with `key' as primary
   value or `default', if no matching node could be found. The second 
   value is a generalized boolean value, which indicates, whether 
   the node was found or not.

   This function can be used with `setf'. In this case, the form
   of the `tree' argument must be itself a `setf'-able place, which
   will be updated to hold the modified copy of the tree; the original 
   tree value is not modified in any way."
  (let ((node (wbtree-find-node key tree)))
    (if node
        (values (node-value node) node)
        (values default nil))))


(defun rotate-once (direction make-node key value left right)
  (with-function (make-node)
    (ecase direction
      ((:left)
       (with-node (key* value* _ left* right*) right
         (make-node key* value*
                    (make-node key value left left*)
                    right*)))
      ((:right)
       (with-node (key* value* _ left* right*) left
         (make-node key* value* 
                    left*
                    (make-node key value right* right)))))))
                    

(defun rotate-twice (direction make-node key value left right)
  (with-function (make-node)
    (ecase direction
      ((:left)
       (with-node (key* value* _ left* right*) right
         (with-node (key** value** _ left** right**) left*
           (make-node key** value**
                      (make-node key value left left**)
                      (make-node key* value* right** right*)))))
      ((:right)
       (with-node (key* value* _ left* right*) left
         (with-node (key** value** _ left** right**) right*
           (make-node key** value**
                      (make-node key* value* left* left**)
                      (make-node key value right** right))))))))


(defun rebalance (make-node key value left right)
  (with-function (make-node)
    (let ((ln (node-count left))
          (rn (node-count right)))
      (cond ((< (+ ln rn) 2) (make-node key value left right))
            ((> rn (* +weight+ ln)) 
             (with-node (_ _ _ rl rr) right
               (let ((rln (node-count rl))
                     (rrn (node-count rr)))
                 (if (< rln rrn)
                     (rotate-once :left make-node key value left right)
                     (rotate-twice :left make-node key value left right)))))
            ((> ln (* +weight+ rn))
             (with-node (_ _ _ ll lr) left
               (let ((lln (node-count ll))
                     (lrn (node-count lr)))
                 (if (< lrn lln)
                     (rotate-once :right make-node key value left right)
                     (rotate-twice :right make-node key value left right)))))
            (t (make-node key value left right))))))


(defun wbtree-update-1 (key value tree test lessp make-node empty-node)
  (with-function (lessp)
    (with-function (make-node)
      (labels ((insert (node)
                 (if (wbtree-empty-p node) 
                     (values (make-node key value empty-node empty-node) t)
                     (with-node (nkey nvalue _ left right) node
                       (cond
                         ((lessp key nkey) 
                          (multiple-value-bind (new-left change) (insert left)
                            (if (not change)
                                (values node nil)
                                (values (rebalance make-node nkey nvalue new-left right)
                                        change))))
                         ((lessp nkey key) 
                          (multiple-value-bind (new-right change) (insert right)
                            (if (not change)
                                (values node nil)
                                (values (rebalance make-node nkey nvalue left new-right)
                                        change))))
                         (t (if (funcall test value nvalue)
                                (values node nil)
                                (values (make-node key value left right)
                                        node))))))))
        (insert tree)))))


(defun delete-minimum (make-node node)
  (with-node (key value _ left right) node
    (if (wbtree-empty-p left) right
        (rebalance make-node key value (delete-minimum make-node left) right))))


(defun delete* (make-node left right)
  (cond
    ((wbtree-empty-p left) right)
    ((wbtree-empty-p right) left)
    (t (let* ((min-node (wbtree-minimum-node right))
              (min-key (node-key min-node))
              (min-value (node-value min-node)))
         (rebalance make-node min-key min-value left 
                    (delete-minimum make-node right))))))


(defun wbtree-remove-1 (key tree lessp make-node)
  (with-function (lessp)
    (labels ((drop (node)
               (if (wbtree-empty-p node)
                   (values node nil)
                   (with-node (nkey nvalue _ left right) node
                     (cond
                       ((lessp key nkey) 
                        (multiple-value-bind (new-node old-node) (drop left)
                          (if (not old-node)
                              (values node nil)
                              (values (rebalance make-node nkey nvalue new-node right)
                                      old-node))))
                       ((lessp nkey key) 
                        (multiple-value-bind (new-node old-node) (drop right)
                          (if (not old-node)
                              (values node nil)
                              (values (rebalance make-node nkey nvalue left new-node)
                                      old-node))))
                       (t (values (delete* make-node left right)
                                  node)))))))
      (drop tree))))


(defun wbtree-fold-1 (function tree lessp transform
                      &key (direction :forward) (associativity :left)
                           (initial-value nil)
                           (start nil have-start)
                           (end nil have-end))

  (declare (optimize (speed 3) (debug 0)))
  (let ((forward (ecase direction ((:forward t) t) ((:backward nil) nil)))
        (function (coerce function 'function))
        (lessp (coerce lessp 'function))
        (start (and have-start (funcall transform start)))
        (end (and have-end (funcall transform end)))
        (value initial-value))
    (with-function (lessp)
      (labels ((reduce-node (node) 
                 (if (eq associativity :left)
                     (setf value (funcall function value node))
                     (setf value (funcall function node value))))
               (walk-forward (node)
                 (unless (wbtree-empty-p node)
                   (let ((start-in (not (and have-start (lessp (node-key node) start))))
                         (end-in (or (not have-end) (lessp (node-key node) end))))
                     (when start-in
                       (walk-forward (node-left node))
                       (when end-in (reduce-node node)))
                     (when end-in (walk-forward (node-right node))))))
               (walk-backward (node)
                 (unless (wbtree-empty-p node)
                   (let ((start-in (not (and have-start (lessp start (node-key node)))))
                         (end-in (or (not have-end) (lessp end (node-key node)))))
                     (when start-in
                       (walk-backward (node-right node))
                       (when end-in (reduce-node node)))
                     (when end-in (walk-backward (node-left node)))))))
        (if forward
            (walk-forward tree)
            (walk-backward tree))
        value))))


(defun wbtree-map-1 (function tree lessp transformation
                     &key (direction :forward) 
                          (collectp nil)
                          (start nil have-start)
                          (end nil have-end))
  
  (let ((head nil) (tail nil)
        (lessp (coerce lessp 'function))
        (start (and have-start (funcall transformation start)))
        (end (and have-end (funcall transformation end)))
        (forward (ecase direction ((:forward t) t) ((:backward nil) nil))))
    (with-function (lessp)
      (labels ((walk-forward (node func)
                 (unless (wbtree-empty-p node)
                   (let ((start-in (not (and have-start (lessp (node-key node) start))))
                         (end-in (or (not have-end) (lessp (node-key node) end))))
                     (when start-in
                       (walk-forward (node-left node) func)
                       (when end-in (funcall func node)))
                     (when end-in (walk-forward (node-right node) func)))))
               (walk-backward (node func)
                 (unless (wbtree-empty-p node)
                   (let ((start-in (not (and have-start (lessp start (node-key node)))))
                         (end-in (or (not have-end) (lessp end (node-key node)))))
                     (when start-in
                       (walk-backward (node-right node) func)
                       (when end-in (funcall func node)))
                     (when end-in (walk-backward (node-left node) func)))))
               (collect (node)
                 (let ((result (funcall function node)))
                   (if (null head)
                       (setf head (setf tail (cons result nil)))
                       (setf tail (setf (cdr tail) (cons result nil)))))))
        (if forward
            (walk-forward tree (if collectp #'collect function))
            (walk-backward tree (if collectp #'collect function)))
        head))))


(defmacro do-wbtree (((key value) tree &rest options) &body body)
  (let ((node (gensym "NODE-")))
    (loop
       :for (k) :on options :by #'cddr
       :do (unless (member k '(:start :end :direction)) (error "invalid option to ~S: ~S" 'do-wbtree k)))
    `(block nil
       (wbtree-map (lambda (,node)
                     (let ((,key (node-key ,node))
                           (,value (node-value ,node)))
                       ,@body))
                   ,tree ,@options))))


          



(defun concat-3 (key value left right lessp make-node empty-node)
  (with-function (make-node)
    (cond ((wbtree-empty-p left) (wbtree-update-1 key value right #'eql lessp make-node empty-node))
          ((wbtree-empty-p right) (wbtree-update-1 key value left #'eql lessp make-node empty-node))
          (t (with-node (k1 v1 n1 l1 r1) left
               (with-node (k2 v2 n2 l2 r2) right
                 (cond ((< (* +weight+ n1) n2) (rebalance make-node k2 v2 (concat-3 key value left l2 lessp make-node empty-node) r2))
                       ((< (* +weight+ n2) n1) (rebalance make-node k1 v1 l1 (concat-3 key value r1 right lessp make-node empty-node)))
                       (t (make-node key value left right)))))))))


(defun split-lt (key tree lessp make-node empty-node)
  (with-function (lessp)
    (if (wbtree-empty-p tree) tree
        (with-node (k v _ l r) tree
          (cond
            ((lessp key k) (split-lt key l lessp make-node empty-node))
            ((lessp k key) (concat-3 k v l (split-lt key r lessp make-node empty-node) lessp make-node empty-node))
            (t l))))))


(defun split-gt (key tree lessp make-node empty-node)
  (with-function (lessp)
    (if (wbtree-empty-p tree) tree
        (with-node (k v _ l r) tree
          (cond
            ((lessp key k) (concat-3 k v (split-gt key l lessp make-node empty-node) r lessp make-node empty-node))
            ((lessp k key) (split-gt key r lessp make-node empty-node))
            (t r))))))


(defun concat (tree1 tree2 lessp make-node empty-node)
  (cond ((wbtree-empty-p tree1) tree2)
        ((wbtree-empty-p tree2) tree1)
        (t (with-node (k1 v1 n1 l1 r1) tree1
             (with-node (k2 v2 n2 l2 r2) tree2
               (cond ((< (* +weight+ n1) n2) (rebalance make-node k2 v2 (concat tree1 l2 lessp make-node empty-node) r2))
                     ((< (* +weight+ n2) n1) (rebalance make-node k1 v1 l1 (concat r1 tree2 lessp make-node empty-node)))
                     (t (let* ((min-node (wbtree-minimum-node tree2))
                               (min-key (node-key min-node))
                               (min-value (node-value min-node)))
                          (rebalance make-node min-key min-value tree1 (delete-minimum make-node tree2))))))))))



(defun choose-right (key left right)
  (declare (ignore key left))
  right)



(defun wbtree-union-1 (tree1 tree2 combiner lessp make-node empty-node)
  (with-function (lessp)
    (with-function (make-node) 
      (with-function (combiner)
        (labels
            ((conc-3 (key value left right) 
               (concat-3 key value left right lessp make-node empty-node))
             (split-lt (key value tree)
               (if (wbtree-empty-p tree) 
                   (values tree value)
                   (with-node (k v _ l r) tree
                     (cond
                       ((lessp key k) (split-lt key value l))
                       ((lessp k key) 
                        (multiple-value-bind (nr val) (split-lt key value r)
                          (values (conc-3 k v l nr) val)))
                       (t (values l (combiner key v value)))))))
             (split-gt (key tree)
               (if (wbtree-empty-p tree) tree
                   (with-node (k v _ l r) tree
                     (cond
                       ((lessp key k) (conc-3 k v (split-gt key l) r))
                       ((lessp k key) (split-gt key r))
                       (t r)))))
             (union* (tree1 tree2)
               (cond 
                 ((eq tree1 tree2) tree1)
                 ((wbtree-empty-p tree2) tree1)
                 ((wbtree-empty-p tree1) tree2)
                 (t (with-node (k v _ l r) tree2
                      (multiple-value-bind (l* v*) (split-lt k v tree1)
                        (let ((r* (split-gt k tree1)))
                          (conc-3 k v*
                                  (union* l* l)
                                  (union* r* r)))))))))
          (union* tree1 tree2))))))


(defun wbtree-intersection-1 (tree1 tree2 combiner lessp make-node empty-node)
  (with-function (lessp)
    (labels ((memberp (key value tree)
               (if (wbtree-empty-p tree) (values nil value)
                   (let ((key* (node-key tree)))
                     (cond
                       ((lessp key key*) (memberp key value (node-left tree)))
                       ((lessp key* key) (memberp key value (node-right tree)))
                       (t (values t (funcall combiner key (node-value tree) value)))))))
             (intersect* (tree1 tree2)
               (cond 
                 ((eq tree1 tree2) tree1)
                 ((wbtree-empty-p tree1) empty-node)
                 ((wbtree-empty-p tree2) empty-node)
                 (t (with-node (k v _ l r) tree2
                      (let ((l* (split-lt k tree1 lessp make-node empty-node))
                            (r* (split-gt k tree1 lessp make-node empty-node)))
                        (multiple-value-bind (member value) (memberp k v tree1)
                          (if member
                              (concat-3 k value (intersect* l* l) (intersect* r* r) lessp make-node empty-node)
                              (concat (intersect* l* l) (intersect* r* r) lessp make-node empty-node)))))))))
      (intersect* tree1 tree2))))



(defun wbtree-difference-1 (tree1 tree2 lessp make-node empty-node)
  (labels ((difference* (tree1 tree2)
             (cond 
               ((eq tree1 tree2) empty-node)
               ((wbtree-empty-p tree1) tree1)
               ((wbtree-empty-p tree2) tree1)
               (t (with-node (k _ _ l r) tree2
                    (let ((l* (split-lt k tree1 lessp make-node empty-node))
                          (r* (split-gt k tree1 lessp make-node empty-node)))
                      (concat (difference* l* l)
                              (difference* r* r)
                              lessp make-node empty-node)))))))
    (difference* tree1 tree2)))


(defun wbtree-iterator (tree &key (direction :forward))
  (let ((forward (ecase direction ((:forward t) t) ((:backward nil) nil))))
    (let ((left (if forward #'node-left #'node-right))
          (right (if forward #'node-right #'node-left)))
      (labels ((left (nd) (funcall left nd))
               (right (nd) (funcall right nd))
               (goleft (nd stack)
                 (if (wbtree-empty-p nd) 
                     stack
                     (goleft (left nd) (cons nd stack)))))
        (let ((stack (goleft tree '())))
          #'(lambda ()
              (if (null stack) nil
                  (let ((head (car stack))
                        (tail (cdr stack)))
                    (setf stack (goleft (right head) tail))
                    head))))))))

(defun wbtree-equal-1 (tree1 tree2 test lessp)
  (cond 
    ((eq tree1 tree2) t)
    ((wbtree-empty-p tree1) (wbtree-empty-p tree2))
    ((wbtree-empty-p tree2) nil)
    (t (with-function (lessp)
         (labels ((not-equal (x y) (or (lessp x y) (lessp y x)))
                  (walk-iter (key1 value1 iter1 key2 value2 iter2)
                    (if (or (not-equal key1 key2) 
                            (not (funcall test value1 value2)))
                        nil
                        (let ((node1 (funcall iter1))
                              (node2 (funcall iter2)))
                          (if (null node1) 
                              (null node2)
                              (and node2
                                   (walk-iter (node-key node1) (node-value node1) iter1 
                                              (node-key node2) (node-value node2) iter2)))))))
         (let ((iter1 (wbtree-iterator tree1))
               (iter2 (wbtree-iterator tree2)))
           (let ((node1 (funcall iter1))
                 (node2 (funcall iter2)))
             (walk-iter (node-key node1) (node-value node1) iter1 
                        (node-key node2) (node-value node2) iter2))))))))
  
(defun wbtree-equal-2 (tree1 tree2 node-test lessp)
  (cond 
    ((eq tree1 tree2) t)
    ((wbtree-empty-p tree1) (wbtree-empty-p tree2))
    ((wbtree-empty-p tree2) nil)
    (t (with-function (lessp)
         (labels ((not-equal (x y) (or (lessp x y) (lessp y x)))
                  (walk-iter (node1 iter1 node2 iter2)
                    (let ((key1 (node-key node1))
                          (key2 (node-key node2)))
                      (if (or (not-equal key1 key2) 
                              (not (funcall node-test node1 node2)))
                          nil
                          (let ((node1 (funcall iter1))
                                (node2 (funcall iter2)))
                            (if (null node1) 
                                (null node2)
                                (and node2
                                     (walk-iter node1 iter1 
                                                node2 iter2))))))))
         (let ((iter1 (wbtree-iterator tree1))
               (iter2 (wbtree-iterator tree2)))
           (let ((node1 (funcall iter1))
                 (node2 (funcall iter2)))
             (walk-iter node1 iter1 
                        node2 iter2))))))))

(defun wbtree-check-invariants-1 (tree lessp)
  (with-function (lessp)
    (labels
        ((recurse (tree)
           (unless (wbtree-empty-p tree)
             (with-node (key _ count left right) tree
               (unless (wbtree-empty-p left)
                 (let ((left-key (node-key left)))
                   (unless (lessp left-key key)
                     (cerror "try remaining nodes" "left child key is >= parent key")))
                 (recurse left))
               (unless (wbtree-empty-p right)
                 (let ((right-key (node-key right)))
                   (unless (lessp key right-key)
                     (cerror "try remaining nodes" "right child key is <= parent key")))
                 (recurse right))
               (let ((nleft (wbtree-size left))
                     (nright (wbtree-size right)))
                 (unless (= count (+ 1 nleft nright))
                   (cerror "try remaining nodes" "invalid tree size counter"))
                 (when (> (+ nleft nright) 2)
                   (unless (or (>= (* +weight+ nleft) nright)
                               (>= (* +weight+ nright) nleft))
                     (cerror "try remaining nodes" "weight invariant violated for node"))))))))
      (recurse tree))))

(defgeneric wbtree-check-invariants (tree))

(defgeneric wbtree-rebalance (tree)
  (:documentation "Generates a fully balanced tree from `tree'. This function answers a
   tree of the same kind as `tree', which contains the same key/value pairs
   as `tree'. However, the copy returned is fully balanced. Note, that this
   optimization often does not really pay off."))


(defun wbtree-rebalance-1 (tree constructor empty)
  (let* ((size (wbtree-size tree))
         (array (make-array size :element-type t :adjustable nil :fill-pointer nil)))
    (let ((pointer 0))
      (wbtree-map (lambda (node)
                    (setf (aref array pointer) node)
                    (incf pointer))
                  tree))
    (labels ((recurse (start end)
               (let ((count (- end start)))
                 (cond 
                   ((not (plusp count)) empty)
                   ((= count 1) 
                    (let* ((node (aref array start))
                           (key (node-key node))
                           (value (node-value node)))
                      (funcall constructor key value empty empty)))
                   ((= count 2)
                    (let* ((inner (aref array start))
                           (inner-key (node-key inner))
                           (inner-value (node-value inner))
                           (outer (aref array (1+ start)))
                           (outer-key (node-key outer))
                           (outer-value (node-value outer)))
                      (funcall constructor inner-key inner-value empty
                               (funcall constructor outer-key outer-value empty empty 1)
                               2)))
                   (t (let* ((middle (ash (+ start end) -1))
                             (node (aref array middle))
                             (key (node-key node))
                             (value (node-value node))
                             (right (recurse (1+ middle) end))
                             (left (recurse start middle)))
                        (funcall constructor key value 
                                 left right)))))))
      (recurse 0 size))))


(defun wbtree-load-form (tree constructor-cell empty-cell)
  ;;; I am not sure, whether this whole rebalancing business is actually
  ;;; a good idea. It would be simpler to just generate the moral equivalent
  ;;; of make-load-form-saving-slots.
  (let* ((size (node-count tree))
         (array (make-array size :fill-pointer 0)))
    (wbtree-map (lambda (node) (vector-push-extend node array)) tree)
    (labels
        ((recurse (start end)
           (let ((count (- end start)))
             (cond
               ((not (plusp count)) empty-cell)
               ((= count 1)
                (let* ((node (aref array start))
                       (key (node-key node))
                       (value (node-value node)))
                  `(,constructor-cell ',key ',value ,empty-cell ,empty-cell 1)))
               (t (let* ((middle (ash (+ start end) -1))
                         (node (aref array middle))
                         (key (node-key node))
                         (value (node-value node)))
                    `(,constructor-cell ',key ',value 
                                        ,(recurse start middle)
                                        ,(recurse (1+ middle) end))))))))
      (recurse 0 size))))



(defmacro define-wbtree (name &body rest)
  ;; There is simply too much code lying around, which relies on
  ;; the old style DEFINE-WBTREE macro syntax. Luckily, we can easily
  ;; detect, whether the caller expects the old syntax and translate
  ;; it accordingly.
  (if (or (symbolp (car rest))
          (and (consp (car rest)) (not (keywordp (caar rest)))))
      `(define-wbtree-really ,name
         (:test ,(car rest))
         (:constructor nil)
         (:spread-constructor ,name)
         ,@(when (cdr rest) `((:documentation ,(cadr rest)))))
      `(define-wbtree-really ,name ,@rest)))


(defmacro define-wbtree-really (name &body clauses)
  ;; This whole GENERATED-NAMES hack exists only, so that we do not
  ;; spam the image with newly consed up names for the node constructor
  ;; and the empty node variable. It would work just as well without,
  ;; but would be wasteful when a tree type is ever redefined.
  (let* ((generated-names (or (get name 'wbtree-compile-time-info) (list (gensym "MAKE-NODE-") (gensym "+EMPTY-NODE+"))))
         (node-constructor (first generated-names))
         (empty-node (second generated-names))
         (default-constructor-name (format nil "MAKE-~A" (symbol-name name)))
         (default-spread-constructor-name (format nil "MAKE-~A*" (symbol-name name)))
         (default-type-predicate-name (format nil "~A~A" (symbol-name name) (if (position #\- (symbol-name name)) "-P" "P")))
         (type-predicate-symbol 't)
         (constructor-symbol 't)
         (spread-constructor-symbol nil)
         (documentation-string nil)
         (key-transformation nil)
         (lessp-function nil))
    (loop
      :for clause :in clauses
      :for (key . more-forms) := clause
      :do (ecase key
            ((:documentation) (setf documentation-string (list (first more-forms))))
            ((:test) (setf lessp-function (first more-forms)))
            ((:key) (setf key-transformation (first more-forms)))
            ((:constructor) (setf constructor-symbol (first more-forms)))
            ((:spread-constructor) (setf spread-constructor-symbol (first more-forms)))
            ((:predicate) (setf type-predicate-symbol (first more-forms)))))
    (setf constructor-symbol 
          (cond
            ((eq constructor-symbol 't) (intern default-constructor-name *package*))
            ((null constructor-symbol) nil)
            (t constructor-symbol)))
    (setf spread-constructor-symbol
          (cond
            ((eq spread-constructor-symbol 't) (intern default-spread-constructor-name))
            ((null spread-constructor-symbol) nil)
            (t spread-constructor-symbol)))
    (setf type-predicate-symbol
          (cond
            ((eq type-predicate-symbol 't) (intern default-type-predicate-name))
            ((null type-predicate-symbol) nil)
            (t type-predicate-symbol)))
    (let* ((key-var (gensym "KEY-"))
           (raw-key-var (if key-transformation (gensym "RAW-KEY-") key-var))
           (value-var (gensym "VALUE-"))
           (pairs-var (gensym "PAIRS-"))
           (test-var (gensym "TEST-"))
           (full-test-var (gensym "FULL-TEST-"))
           (have-test-var (gensym "HAVE-TEST-"))
           (func-var (gensym "FUNCTION-"))
           (other-tree-var (gensym "OTHER-TREE-"))
           (combiner-var (gensym "COMBINER-"))                           
           (tree-var (gensym "TREE-")))
      (flet ((add-key-transformation (body)
               (if (not key-transformation)
                   body
                   `(let ((,key-var (,key-transformation ,raw-key-var)))
                      ,body))))
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (setf (get ',name 'wbtree-compile-time-info) (list ',node-constructor ',empty-node)))
           
           (defstruct (,name (:include wbtree)
                             (:copier nil)
                             (:conc-name nil)
                             ,@(when type-predicate-symbol `((:predicate ,type-predicate-symbol)))
                             (:constructor ,node-constructor (node-key node-value node-left node-right 
                                                              &optional (node-count (+ (node-count node-left) (node-count node-right) 1)))))
             ,@documentation-string)

           (defparameter ,empty-node (,node-constructor nil nil nil nil 0))

           ,@(when constructor-symbol
               `((defun ,constructor-symbol (&optional ,pairs-var (,test-var #'eql))
                   (loop
                     :with ,tree-var := ,empty-node
                     :for (,raw-key-var ,value-var) :on ,pairs-var :by #'cddr
                     ,@(when key-transformation `(:for ,key-var := (,key-transformation ,raw-key-var)))
                     :do (setf ,tree-var (wbtree-update-1 ,key-var ,value-var ,tree-var ,test-var #',lessp-function #',node-constructor ,empty-node))
                     :finally (return ,tree-var)))))

           ,@(when spread-constructor-symbol
               `((defun ,spread-constructor-symbol (&rest ,pairs-var)
                   (loop
                     :with ,tree-var := ,empty-node
                     :for (,raw-key-var ,value-var) :on ,pairs-var :by #'cddr
                     ,@(when key-transformation `(:for ,key-var := (,key-transformation ,raw-key-var)))
                     :do (setf ,tree-var (wbtree-update-1 ,key-var ,value-var ,tree-var #'eql #',lessp-function #',node-constructor ,empty-node))
                     :finally (return ,tree-var)))))

           (defmethod wbtree-floor-node (,raw-key-var (,tree-var ,name))
             ,(add-key-transformation `(wbtree-floor-node-1 ,key-var ,tree-var #',lessp-function)))

           (defmethod wbtree-ceiling-node (,raw-key-var (,tree-var ,name))
             ,(add-key-transformation `(wbtree-ceiling-node-1 ,key-var ,tree-var #',lessp-function)))

           (defmethod wbtree-find-node (,raw-key-var (,tree-var ,name))
             ,(add-key-transformation `(wbtree-find-node-1 ,key-var ,tree-var #',lessp-function)))

           (defmethod wbtree-update (,raw-key-var ,value-var (,tree-var ,name) &optional (,test-var #'eql))
             ,(add-key-transformation `(wbtree-update-1 ,key-var ,value-var ,tree-var ,test-var #',lessp-function #',node-constructor ,empty-node)))

           (defmethod wbtree-remove (,raw-key-var (,tree-var ,name))
             ,(add-key-transformation `(wbtree-remove-1 ,key-var ,tree-var #',lessp-function #',node-constructor)))

           (defmethod wbtree-fold (,func-var (,tree-var ,name) &rest ,pairs-var)
             (apply #'wbtree-fold-1 
                    ,func-var ,tree-var #',lessp-function 
                    ,(if key-transformation `#',key-transformation '#'identity)
                    ,pairs-var))

           (defmethod wbtree-map (,func-var (,tree-var ,name) &rest ,pairs-var)
             (apply #'wbtree-map-1 
                    ,func-var ,tree-var #',lessp-function 
                    ,(if key-transformation `#',key-transformation '#'identity)
                    ,pairs-var))

           (defmethod wbtree-union ((,tree-var ,name) (,other-tree-var ,name) &key ((:combiner ,combiner-var) #'choose-right))
             (wbtree-union-1 ,tree-var ,other-tree-var ,combiner-var #',lessp-function #',node-constructor ,empty-node))

           (defmethod wbtree-intersection ((,tree-var ,name) (,other-tree-var ,name) &key ((:combiner ,combiner-var) #'choose-right))
             (wbtree-intersection-1 ,tree-var ,other-tree-var ,combiner-var #',lessp-function #',node-constructor ,empty-node))

           (defmethod wbtree-difference ((,tree-var ,name) (,other-tree-var ,name))
             (wbtree-difference-1 ,tree-var ,other-tree-var #',lessp-function #',node-constructor ,empty-node))

           (defmethod wbtree-check-invariants ((,tree-var ,name))
             (wbtree-check-invariants-1 ,tree-var #',lessp-function))

           (defmethod wbtree-rebalance ((,tree-var ,name))
             (wbtree-rebalance-1 ,tree-var #',node-constructor ,empty-node))

           (defmethod make-load-form ((,tree-var ,name) &optional environment)
             (declare (ignore environment))
             (wbtree-load-form ,tree-var ',node-constructor ',empty-node))

           (defmethod wbtree-equal ((,tree-var ,name) (,other-tree-var ,name)
                                    &key ((:test ,test-var) #'eql ,have-test-var)
                                         ((:node-test ,full-test-var) nil))
             (if ,full-test-var
                 (if ,have-test-var
                     (error "cannot use both, ~S and ~S" :test :full-test)
                     (wbtree-equal-2 ,tree-var ,other-tree-var ,full-test-var #',lessp-function))
                 (wbtree-equal-1 ,tree-var ,other-tree-var ,test-var #',lessp-function))))))))


;; More compatibility stuff, until I get to fixing all places,
;; which are still using the old names

(defun wbtree-lower-boundary-node (key tree)
  (wbtree-floor-node key tree))

(defun wbtree-upper-boundary-node (key tree)
  (wbtree-ceiling-node key tree))


(define-setf-expander wbtree-find (key-form place &optional (default nil have-default)
                                   &environment env)
  (multiple-value-bind (vars vals newval setter getter) (get-setf-expansion place env)
    (let ((key-temp (gensym))
          (default-temp (if have-default (gensym) nil))
          (value-temp (gensym))
          (new-value (car newval)))
      (if (cdr newval) (error "cannot expand form")
          (values (if have-default
                      (list* key-temp default-temp vars)
                      (cons key-temp vars))
                  (if have-default
                      (list* key-form default vals)
                      (cons key-form vals))
                  (list value-temp)
                  `(let ((,new-value (wbtree-update ,key-temp ,value-temp ,getter)))
                     ,setter
                     ,value-temp)
                  `(wbtree-find ,key-temp ,getter ,@(when have-default (list default-temp))))))))
