

(in-package "DARTS.LIB.WBTREE")

(defconstant +weight+ 4)


(defstruct (wbtree 
             (:conc-name "NODE-")
             (:predicate wbtreep))
  (key nil :read-only t)
  (value nil :read-only t)
  (count 0 :read-only t)
  (left nil :read-only t)
  (right nil :read-only t))


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
  (zerop (node-count node)))

(defun wbtree-size (node)
  (node-count node))

(defun wbtree-node-value (node)
  (if (zerop (node-count node)) (error "attempt to get value of empty node")
      (node-value node)))

(defun wbtree-node-key (node)
  (if (zerop (node-count node)) (error "attempt to get key of empty node")
      (node-key node)))

(defun wbtree-node-left-subtree (node)
  (if (zerop (node-count node)) node
      (node-left node)))

(defun wbtree-node-right-subtree (node)
  (if (zerop (node-count node)) node
      (node-right node)))

(defgeneric wbtree-information (node))
  ;; => predicate constructor empty-node type-name


(defmacro with-function ((name &optional (init-form name)) &body body)
  (let ((temp (gensym))
        (args (gensym)))
    `(let ((,temp ,init-form))
       (macrolet ((,name (&rest ,args) (list* 'funcall ',temp ,args)))
         (let ((,name ,temp))
           (declare (ignorable ,name))
           ,@body)))))


(defun wbtree-minimum-node (tree)
  (if (wbtree-empty-p tree)
      nil
      (loop 
         :for previous = tree :then current
         :for current = (node-left tree) :then (node-left current)
         :until (wbtree-empty-p current)
         :finally (return previous))))


(defun wbtree-maximum-node (tree)
  (if (wbtree-empty-p tree)
      nil
      (loop
         :for previous = tree :then current
         :for current = (node-right tree) :then (node-right current)
         :until (wbtree-empty-p current)
         :finally (return previous))))


(defun wbtree-upper-boundary-node (key tree)
  (with-function (lessp (wbtree-information tree))
    (labels ((walk (nd best)
               (if (wbtree-empty-p nd) best
                   (with-node (nkey _ _ left right) nd
                     (cond
                       ((lessp nkey key) (walk right best))
                       ((lessp key nkey) (walk left nd))
                       (t nd))))))
      (walk tree nil))))


(defun wbtree-lower-boundary-node (key tree)
  (with-function (lessp (wbtree-information tree))
    (labels ((walk (nd best)
               (if (wbtree-empty-p nd) best
                   (with-node (nkey _ _ left right) nd
                     (cond
                       ((lessp nkey key) (walk right nd))
                       ((lessp key nkey) (walk left best))
                       (t nd))))))
      (walk tree nil))))


(defun wbtree-find-node (key tree)
  (with-function (lessp (wbtree-information tree))
    (labels ((walk (node)
               (if (wbtree-empty-p node) nil
                   (let ((ref (node-key node)))
                     (cond
                       ((lessp key ref) (walk (node-left node)))
                       ((lessp ref key) (walk (node-right node)))
                       (t node))))))
      (walk tree))))


(defun wbtree-find (key tree)
  (let ((node (wbtree-find-node key tree)))
    (if node
        (values (node-value key) node)
        (values nil nil))))





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


(defun wbtree-update* (key value tree test lessp make-node empty-node)
  (with-function (lessp)
    (with-function (make-node)
      (labels ((insert (node)
                 (if (wbtree-empty-p node) 
                     (values (make-node key value empty-node empty-node) :added)
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
                                        :replaced))))))))
        (insert tree)))))


(defun wbtree-update (key value tree &optional (test #'eql))
  (multiple-value-bind (lessp make-node empty-node) (wbtree-information tree)
    (wbtree-update* key value tree test lessp make-node empty-node)))




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


(defun ptree-remove* (key tree lessp make-node)
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


(defun wbtree-remove (key tree)
  (multiple-value-bind (lessp make-node) (wbtree-information tree)
    (ptree-remove* key tree lessp make-node)))


(defun wbtree-map (function tree 
                   &key (direction :forward) 
                        (collectp nil)
                        (start nil have-start)
                        (end nil have-end))
  (let ((head nil) (tail nil)
        (forward (ecase direction ((:forward t) t) ((:backward nil) nil))))
    (with-function (lessp (wbtree-information tree))
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



(defun concat-3 (key value left right lessp make-node empty-node)
  (with-function (make-node)
    (cond ((wbtree-empty-p left) (wbtree-update* key value right #'eql lessp make-node empty-node))
          ((wbtree-empty-p right) (wbtree-update* key value left #'eql lessp make-node empty-node))
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


(defun wbtree-union (tree1 tree2)
  (multiple-value-bind (lessp make-node empty-node) (wbtree-information tree1)
    (labels ((union* (tree1 tree2)
               (cond 
                 ((eq tree1 tree2) tree1)
                 ((wbtree-empty-p tree2) tree1)
                 ((wbtree-empty-p tree1) tree2)
                 (t (with-node (k v _ l r) tree2
                      (let ((l* (split-lt k tree1 lessp make-node empty-node))
                            (r* (split-gt k tree1 lessp make-node empty-node)))
                        (concat-3 k v 
                                  (union* l* l)
                                  (union* r* r)
                                  lessp make-node empty-node)))))))
    (union* tree1 tree2))))


(defun wbtree-difference (tree1 tree2)
  (multiple-value-bind (lessp make-node empty-node) (wbtree-information tree1)
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
      (difference* tree1 tree2))))


(defun wbtree-intersection (tree1 tree2)
  (multiple-value-bind (lessp make-node empty-node) (wbtree-information tree1)
    (with-function (lessp)
      (labels ((memberp (key tree)
                 (if (wbtree-empty-p tree) nil
                     (let ((key* (node-key tree)))
                       (cond
                         ((lessp key key*) (memberp key (node-left tree)))
                         ((lessp key* key) (memberp key (node-right tree)))
                         (t t)))))
               (intersect* (tree1 tree2)
                 (cond 
                   ((eq tree1 tree2) tree1)
                   ((wbtree-empty-p tree1) empty-node)
                   ((wbtree-empty-p tree2) empty-node)
                   (t (with-node (k v _ l r) tree2
                        (let ((l* (split-lt k tree1 lessp make-node empty-node))
                              (r* (split-gt k tree1 lessp make-node empty-node)))
                          (if (memberp k tree1)
                              (concat-3 k v (intersect* l* l) (intersect* r* r) lessp make-node empty-node)
                              (concat (intersect* l* l) (intersect* r* r) lessp make-node empty-node))))))))
        (intersect* tree1 tree2)))))


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


(defun wbtree-equal (tree1 tree2 &key (test #'eql))
  (cond 
    ((eq tree1 tree2) t)
    ((wbtree-empty-p tree1) (wbtree-empty-p tree2))
    ((wbtree-empty-p tree2) nil)
    (t (with-function (lessp (wbtree-information tree1))
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




(defmacro define-wbtree (name lessp)
  (let* ((info (get name 'wbtree-information))
         (constructor (if info (car info) (gensym)))
         (empty (if info (cdr info) (gensym)))
         (conc-name (format nil "~A Do Not Use.." (symbol-name name))))
    `(progn
       (defstruct (,name
                    (:include node)
                    (:constructor ,constructor (key value left right 
                                                &optional (count (+ (node-count left) (node-count right) 1))))
                    (:conc-name ,conc-name)))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'wbtree-information) (cons ',constructor ',empty)))
       (defparameter ,empty (,constructor nil nil nil nil 0))
       (defmethod wbtree-information ((tree ,name)) (values #',lessp #',constructor ,empty ',name))
       (defun ,name (&rest pairs)
         (loop
            :with tree := ,empty
            :for (key value) :on pairs :by #'cddr
            :do (setf tree (wbtree-update* key value tree #'eql #',lessp #',constructor ,empty))
            :finally (return tree))))))
