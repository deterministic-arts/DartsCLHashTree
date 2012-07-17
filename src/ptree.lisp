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

(in-package "DARTS.LIB.PTREE")

(defconstant +weight+ 4)

(defstruct (ptree
			 (:copier nil)
			 (:predicate ptreep)))

(defstruct (empty
			 (:include ptree)
			 (:predicate ptree-empty-p)))

(defparameter +empty-ptree+ (make-empty))

(defstruct (node
			 (:include ptree)
			 (:constructor make-node (key value 
									  &optional (left +empty-ptree+) 
									            (right +empty-ptree+)
									  &aux (count (+ (if (ptree-empty-p left) 0 (node-count left))
													 (if (ptree-empty-p right) 0 (node-count right))
													 1)))))
  (key nil :read-only t)
  (value nil :read-only t)
  (count 1 :type (integer 1) :read-only t)
  (left +empty-ptree+ :read-only t)
  (right +empty-ptree+ :read-only t))

(defmethod print-object ((ob empty) stream)
  (print-unreadable-object (ob stream :type nil)
	(format stream "~A 0" :ptree)))

(defmethod print-object ((ob node) stream)
  (print-unreadable-object (ob stream :type nil)
	(format stream "~A ~D ~S~@[ ~S~]" :ptree 
			(node-count ob)
			(node-key ob)
			(node-value ob))))


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


(defun ptree-size (tree)
  (if (ptree-empty-p tree) 0 (node-count tree)))

(defun ptree-key (tree)
  (if (ptree-empty-p tree) (values nil nil) (values (node-key tree) t)))

(defun ptree-value (tree)
  (if (ptree-empty-p tree) (values nil nil) (values (node-value tree) t)))

(defun ptree-left (tree)
  (if (ptree-empty-p tree) (values nil nil) (values (node-left tree) t)))

(defun ptree-right (tree)
  (if (ptree-empty-p tree) (values nil nil) (values (node-right tree) t)))

(defun ptree-minimum (tree)
  (if (ptree-empty-p tree)
	  nil
	  (loop 
		 :for previous = tree :then current
		 :for current = (node-left tree) :then (node-left current)
		 :until (ptree-empty-p current)
		 :finally (return previous))))

(defun ptree-maximum (tree)
  (if (ptree-empty-p tree)
	  nil
	  (loop
		 :for previous = tree :then current
		 :for current = (node-right tree) :then (node-right current)
		 :until (ptree-empty-p current)
		 :finally (return previous))))

(defun ptree-smallest (key tree)
  (labels ((walk (nd best)
			 (if (ptree-empty-p nd) best
				 (with-node (nkey _ _ left right) nd
				   (cond
					 ((string< nkey key) (walk right best))
					 ((string< key nkey) (walk left nd))
					 (t nd))))))
	(walk tree nil)))

(defun ptree-largest (key tree)
  (labels ((walk (nd best)
			 (if (ptree-empty-p nd) best
				 (with-node (nkey _ _ left right) nd
				   (cond
					 ((string< nkey key) (walk right nd))
					 ((string< key nkey) (walk left best))
					 (t nd))))))
	(walk tree nil)))



(defun ptree-find (key tree)
  (labels ((walk (node)
			 (if (ptree-empty-p node) nil
				 (let ((ref (node-key node)))
				   (cond
					 ((string< key ref) (walk (node-left node)))
					 ((string< ref key) (walk (node-right node)))
					 (t node))))))
	(walk tree)))


(defun ptree-get (key tree &optional default)
  (let ((node (ptree-find key tree)))
	(if node 
		(values (node-value node) t)
		(values default nil))))


(defun rotate-once (direction key value left right)
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
					(make-node key value right* right))))))
					

(defun rotate-twice (direction key value left right)
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
					  (make-node key value right** right)))))))


(defun rebalance (key value left right)
  (let ((ln (ptree-size left))
		(rn (ptree-size right)))
	(cond ((< (+ ln rn) 2) (make-node key value left right))
		  ((> rn (* +weight+ ln)) 
		   (with-node (_ _ _ rl rr) right
			 (let ((rln (ptree-size rl))
				   (rrn (ptree-size rr)))
			   (if (< rln rrn)
				   (rotate-once :left key value left right)
				   (rotate-twice :left key value left right)))))
		  ((> ln (* +weight+ rn))
		   (with-node (_ _ _ ll lr) left
			 (let ((lln (ptree-size ll))
				   (lrn (ptree-size lr)))
			   (if (< lrn lln)
				   (rotate-once :right key value left right)
				   (rotate-twice :right key value left right)))))
		  (t (make-node key value left right)))))


(defun ptree-insert (key value tree &optional (test #'eq))
  (declare (dynamic-extent test))
  (let ((changed nil))
	(labels ((insert (node)
			   (if (ptree-empty-p node) 
				   (make-node key value)
				   (with-node (nkey nvalue _ left right) node
					 (cond
					   ((string< key nkey) (rebalance nkey nvalue (insert left) right))
					   ((string< nkey key) (rebalance nkey nvalue left (insert right)))
					   (t (if (funcall test value nvalue)
							  (return-from ptree-insert (values tree node))
							  (progn (setf changed node) (make-node key value left right)))))))))
	  (let ((new-tree (insert tree)))
		(values new-tree changed)))))


(defun ptree-update (key tree update)
  (declare (dynamic-extent update))
  (labels ((insert (node)
			 (if (ptree-empty-p node)
				 (multiple-value-bind (new-value modifyp) (funcall update nil)
				   (if (not modifyp) 
					   (values node nil)
					   (values (make-node key new-value +empty-ptree+ +empty-ptree+) nil)))
				 (with-node (nkey nvalue _ left right) node
				   (cond
					 ((string< key nkey)
					  (multiple-value-bind (new-node changed) (insert left)
						(if (eq left new-node) 
							(values node nil)
							(values (rebalance nkey nvalue new-node right) changed))))
					 ((string< nkey key)
					  (multiple-value-bind (new-node changed) (insert right)
						(if (eq right new-node)
							(values node nil)
							(values (rebalance nkey nvalue left new-node) changed))))
					 (t
					  (multiple-value-bind (new-value modifyp) (funcall update node)
						(if (or (not modifyp) (eq new-value nvalue))
							(values node nil)
							(values (make-node key new-value left right)
									node)))))))))
	(insert tree)))


(defun delete-minimum (node)
  (with-node (key value _ left right) node
	(if (ptree-empty-p left) right
		(rebalance key value (delete-minimum left) right))))


(defun delete* (left right)
  (if (ptree-empty-p left) right
	  (if (ptree-empty-p right) left
		  (let* ((min-node (ptree-minimum right))
				 (min-key (node-key min-node))
				 (min-value (node-value min-node)))
			(rebalance min-key min-value left (delete-minimum right))))))


(defun ptree-remove (key tree)
  (let ((changed nil))
	(labels ((drop (node)
			   (if (ptree-empty-p node)
				   (return-from ptree-remove (values tree nil))
				   (with-node (nkey nvalue _ left right) node
					 (cond
					   ((string< key nkey) (rebalance nkey nvalue (drop left) right))
					   ((string< nkey key) (rebalance nkey nvalue left (drop right)))
					   (t (setf changed node) (delete* left right)))))))
	  (let ((new-tree (drop tree)))
		(values new-tree changed)))))


(defun ptree-check-invariants (tree)
  (unless (ptree-empty-p tree)
	(with-node (key _ count left right) tree
	  (unless (ptree-empty-p left)
		(let ((left-key (node-key left)))
		  (unless (string< left-key key)
			(cerror "try remaining nodes" "left child key is >= parent key")))
		(ptree-check-invariants left))
	  (unless (ptree-empty-p right)
		(let ((right-key (node-key right)))
		  (unless (string< key right-key)
			(cerror "try remaining nodes" "right child key is <= parent key")))
		(ptree-check-invariants right))
	  (let ((nleft (ptree-size left))
			(nright (ptree-size right)))
		(unless (= count (+ 1 nleft nright))
		  (cerror "try remaining nodes" "invalid tree size counter"))
		(when (> (+ nleft nright) 2)
		  (unless (or (>= (* +weight+ nleft) nright)
					  (>= (* +weight+ nright) nleft))
			(cerror "try remaining nodes" "weight invariant violated for node"))))))
  tree)


(defun ptree-map (function tree 
				   &key (direction :forward) 
				        (collectp nil)
				        (start nil have-start)
				        (end nil have-end))
  (let ((head nil) (tail nil)
		(forward (ecase direction ((:forward t) t) ((:backward nil) nil))))
	(labels ((walk-forward (node func)
			   (unless (ptree-empty-p node)
				 (let ((start-in (not (and have-start (string< (node-key node) start))))
					   (end-in (or (not have-end) (string< (node-key node) end))))
				   (when start-in
					 (walk-forward (node-left node) func)
					 (when end-in (funcall func node)))
				   (when end-in (walk-forward (node-right node) func)))))
			 (walk-backward (node func)
			   (unless (ptree-empty-p node)
				 (let ((start-in (not (and have-start (string< start (node-key node)))))
					   (end-in (or (not have-end) (string< end (node-key node)))))
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
	  head)))


(defun ptree-fold (function initial-value tree 
				   &key (direction :forward have-dir) 
				        (start nil have-start)
				        (end nil have-end))
  (let ((value seed))
	(apply #'ptree-map 
		   (lambda (node) (setf value (funcall node value)))
		   (nconc (when have-dir (list :direction direction))
				  (when have-start (list :start start))
				  (when have-end (list :end end))))
	value))

(defun ptree-pairs (tree)
  (ptree-fold (lambda (node list) (cons (cons (node-key node) (node-value node)) list))
			  nil tree))

(defun ptree-keys (tree)
  (ptree-fold (lambda (node list) (cons (node-key node) list))
			  nil tree))

(defun ptree-values (tree)
  (ptree-fold (lambda (node list) (cons (node-value node) list))
			  nil tree))


(defun concat-3 (key value left right)
  (cond ((ptree-empty-p left) (ptree-insert key value right))
		((ptree-empty-p right) (ptree-insert key value left))
		(t (with-node (k1 v1 n1 l1 r1) left
			 (with-node (k2 v2 n2 l2 r2) right
			   (cond ((< (* +weight+ n1) n2) (rebalance k2 v2 (concat-3 key value left l2) r2))
					 ((< (* +weight+ n2) n1) (rebalance k1 v1 l1 (concat-3 key value r1 right)))
					 (t (make-node key value left right))))))))


(defun split-lt (key tree)
  (if (ptree-empty-p tree) tree
	  (with-node (k v _ l r) tree
		(cond
		  ((string< key k) (split-lt key l))
		  ((string< k key) (concat-3 k v l (split-lt key r)))
		  (t l)))))


(defun split-gt (key tree)
  (if (ptree-empty-p tree) tree
	  (with-node (k v _ l r) tree
		(cond
		  ((string< key k) (concat-3 k v (split-gt key l) r))
		  ((string< k key) (split-gt key r))
		  (t r)))))



(defun concat (tree1 tree2)
  (cond ((ptree-empty-p tree1) tree2)
		((ptree-empty-p tree2) tree1)
		(t (with-node (k1 v1 n1 l1 r1) tree1
			 (with-node (k2 v2 n2 l2 r2) tree2
			   (cond ((< (* +weight+ n1) n2) (rebalance k2 v2 (concat tree1 l2) r2))
					 ((< (* +weight+ n2) n1) (rebalance k1 v1 l1 (concat r1 tree2)))
					 (t (let* ((min-node (ptree-minimum tree2))
							   (min-key (node-key min-node))
							   (min-value (node-value min-node)))
						  (rebalance min-key min-value tree1 (delete-minimum tree2))))))))))


(defun ptree-union (tree1 tree2)
  (labels ((union* (tree1 tree2)
			 (cond 
			   ((eq tree1 tree2) tree1)
			   ((ptree-empty-p tree2) tree1)
			   ((ptree-empty-p tree1) tree2)
			   (t (with-node (k v _ l r) tree2
					(let ((l* (split-lt k tree1))
						  (r* (split-gt k tree1)))
					  (concat-3 k v 
								(union* l* l)
								(union* r* r))))))))
	(union* tree1 tree2)))


(defun ptree-difference (tree1 tree2)
  (labels ((difference* (tree1 tree2)
			 (cond 
			   ((eq tree1 tree2) +empty-ptree+)
			   ((ptree-empty-p tree1) tree1)
			   ((ptree-empty-p tree2) tree1)
			   (t (with-node (k _ _ l r) tree2
					(let ((l* (split-lt k tree1))
						  (r* (split-gt k tree1)))
					  (concat (difference* l* l)
							  (difference* r* r))))))))
	(difference* tree1 tree2)))


(defun ptree-intersection (tree1 tree2)
  (labels ((memberp (key tree)
			 (if (ptree-empty-p tree) nil
				 (let ((key* (node-key tree)))
				   (cond
					 ((string< key key*) (memberp key (node-left tree)))
					 ((string< key* key) (memberp key (node-right tree)))
					 (t t)))))
		   (intersect* (tree1 tree2)
			 (cond 
			   ((eq tree1 tree2) tree1)
			   ((ptree-empty-p tree1) +empty-ptree+)
			   ((ptree-empty-p tree2) +empty-ptree+)
			   (t (with-node (k v _ l r) tree2
					(let ((l* (split-lt k tree1))
						  (r* (split-gt k tree1)))
					  (if (memberp k tree1)
						  (concat-3 k v (intersect* l* l) (intersect* r* r))
						  (concat (intersect* l* l) (intersect* r* r)))))))))
	(intersect* tree1 tree2)))


(defun ptree-iterator (tree &key (direction :forward))
  (let ((forward (ecase direction ((:forward t) t) ((:backward nil) nil))))
	(let ((left (if forward #'node-left #'node-right))
		  (right (if forward #'node-right #'node-left)))
	  (labels ((left (nd) (funcall left nd))
			   (right (nd) (funcall right nd))
			   (goleft (nd stack)
				 (if (ptree-empty-p nd) 
					 stack
					 (goleft (left nd) (cons nd stack)))))
		(let ((stack (goleft tree '())))
		  #'(lambda ()
			  (if (null stack) nil
				  (let ((head (car stack))
						(tail (cdr stack)))
					(setf stack (goleft (right head) tail))
					head))))))))


(defun ptree-equal (tree1 tree2)
  (cond 
	((eq tree1 tree2) t)
	((ptree-empty-p tree1) (ptree-empty-p tree2))
	((ptree-empty-p tree2) nil)
	(t (labels ((walk-iter (key1 iter1 key2 iter2)
				  (if (string/= key1 key2) nil
					  (let ((node1 (funcall iter1))
							(node2 (funcall iter2)))
						(if (null node1) 
							(null node2)
							(and node2
								 (walk-iter (node-key node1) iter1 
											(node-key node2) iter2)))))))
		 (let ((iter1 (ptree-iterator tree1))
			   (iter2 (ptree-iterator tree2)))
		   (walk-iter (node-key (funcall iter1)) iter1 (node-key (funcall iter2)) 
					  iter2))))))


(defun ptree (&rest pairs)
  (loop
	 :with seed := +empty-ptree+
	 :for link :on pairs :by #'cddr
	 :for key := (car link)
	 :for value := (cadr link)
	 :do (setf seed (ptree-insert key value seed))
	 :finally (return seed)))
