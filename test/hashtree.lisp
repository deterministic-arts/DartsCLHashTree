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

(in-package "DARTS.LIB.HASHTREE.TEST")

(defun any-hash-table-element (table)
  (loop
     :for key :being :each :hash-key :in table :using (:hash-value value)
     :do (return-from any-hash-table-element (values key value t)))
  (values nil nil nil))

(defun sorted-keys-unique-p (sorted-list)
  (if (null sorted-list) t
      (loop
         :with previous := (car sorted-list)
         :for element :in (cdr sorted-list)
         :do (progn (when (equal previous element) (return-from sorted-keys-unique-p nil))
                    (setf previous element))
         :finally (return t))))
 

(in-root-suite)

(defsuite hashtree-test)

(in-suite hashtree-test)

(deftest empty-tree-properties ()
  (let ((tree (make-hashtree)))
    (is (zerop (hashtree-count tree)))
    (is (hashtree-empty-p tree))
    (is (null (hashtree-pairs tree)))
    (is (null (hashtree-values tree)))
    (is (null (hashtree-keys tree)))
    (is (zerop (hashtree-fold (lambda (a b c) (declare (ignore a b)) (1+ c)) 0 tree))))) 
        
(deftest incremental-changes ()
  (let* ((key-range 100000)
         (iterations 1000)
         (reference (make-hash-table :test 'equal))
         (random (make-random-state t))
         (missing (cons 'missing 'element))
         (tree (loop
                  :with tree := (make-hashtree)
                  :for count :downfrom iterations :above 0
                  :for key := (random key-range random)
                  :for value := (incf (gethash key reference 0))
                  :do (setf tree (hashtree-update key value tree))
                  :finally (return tree))))
    (is (= (hash-table-count reference) (hashtree-count tree)))
    (is (zerop (loop
                  :for key :being :each :hash-key :in reference :using (:hash-value value)
                  :for found := (hashtree-get key tree)
                  :counting (not (equal value found)))))
    (loop
       :repeat iterations
       :for key := (random key-range random) :then (random key-range random)
       :for expected := (gethash key reference)
       :if expected
         :do (progn 
               (remhash key reference)
               (setf tree (hashtree-remove key tree)))
       :else
         :do (progn
               (setf (gethash key reference) 1)
               (setf tree (hashtree-update key 1 tree))))
    (is (= (hash-table-count reference) (hashtree-count tree)))
    (is (zerop (loop
                  :for key :being :each :hash-key :in reference :using (:hash-value value)
                  :for found := (hashtree-get key tree)
                  :counting (not (equal value found)))))))



(deftest tree-iteration ()
  (let* ((reference (make-hash-table :test 'equal))
         (random (make-random-state t))
         (missing (cons 'missing 'element))
         (tree (loop
                  :with tree := (make-hashtree)
                  :for count :downfrom 1000 :above 0
                  :for key := (random 100000 random)
                  :for value := (incf (gethash key reference 0))
                  :do (setf tree (hashtree-update key value tree))
                  :finally (return tree))))
    (let* ((keys-seen (hashtree-fold (lambda (key value keys-seen)
                                      (let ((expected-value (gethash key reference missing)))
                                        (cons key keys-seen)))
                                    ()
                                    tree))
           (expected-keys (loop :for key :being :each :hash-key :of reference :collecting key))
           (sorted-expected-keys (sort expected-keys #'<))
           (sorted-keys-seen (sort keys-seen #'<)))
      ;; Make sure, that we have actually seen every key, and 
      ;; that each key was visited exactly once
      (is (= (hash-table-count reference) (length sorted-keys-seen)))
      (is (sorted-keys-unique-p sorted-keys-seen))
      (is (equal sorted-expected-keys sorted-keys-seen)))))


(deftest tree-iteration-2 ()
  (let* ((reference (make-hash-table :test 'equal))
         (random (make-random-state t))
         (missing (cons 'missing 'element))
         (tree (loop
                  :with tree := (make-hashtree)
                  :for count :downfrom 1000 :above 0
                  :for key := (random 100000 random)
                  :for value := (incf (gethash key reference 0))
                  :do (setf tree (hashtree-update key value tree))
                  :finally (return tree))))
    (let ((visited 0))
      (do-hashtree (key value) tree
        (incf visited)
        (multiple-value-bind (ref-value found) (gethash key reference missing)
          (is (not (eq ref-value missing)))
          (is found)
          (is (= value ref-value))))
      (is (= visited (hash-table-count reference))))
    ;; Unless the iteration is stopped via `return` from the implicit
    ;; anonymous block, the result is `nil`.
    (is (not (do-hashtree (key value) tree (cons key value))))
    ;; FIXME: we need a few tests for the block stuff here!
    nil))
