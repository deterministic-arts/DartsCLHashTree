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

(in-package "DARTS.LIB.HASHTRIE.TEST")

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

(defsuite hashtrie-test-suite)

(in-suite hashtrie-test-suite)

(deftest empty-tree-properties ()
  (let ((tree (simple-hashtrie)))
    (is (zerop (hashtrie-count tree)))
    (is (hashtrie-empty-p tree))
    (is (null (hashtrie-pairs tree)))
    (is (null (hashtrie-values tree)))
    (is (null (hashtrie-keys tree)))
    (is (zerop (hashtrie-fold 0 (lambda (a b c) (declare (ignore a b)) (1+ c)) tree))))) 


(deftest incremental-changes ()
  (let* ((key-range 100000)
         (iterations 1000)
         (reference (make-hash-table :test 'equal))
         (random (make-random-state t))
         (tree (loop
                  :with tree := (simple-hashtrie)
                  :for count :downfrom iterations :above 0
                  :for key := (random key-range random)
                  :for value := (incf (gethash key reference 0))
                  :do (setf tree (hashtrie-update key value tree))
                  :finally (return tree))))
    (is (= (hash-table-count reference) (hashtrie-count tree)))
    (is (zerop (loop
                  :for key :being :each :hash-key :in reference :using (:hash-value value)
                  :for found := (hashtrie-find key tree)
                  :counting (not (equal value found)))))
    (loop
       :repeat iterations
       :for key := (random key-range random) :then (random key-range random)
       :for expected := (gethash key reference)
       :if expected
         :do (progn 
               (remhash key reference)
               (setf tree (hashtrie-remove key tree)))
       :else
         :do (progn
               (setf (gethash key reference) 1)
               (setf tree (hashtrie-update key 1 tree))))
    (is (= (hash-table-count reference) (hashtrie-count tree)))
    (is (zerop (loop
                  :for key :being :each :hash-key :in reference :using (:hash-value value)
                  :for found := (hashtrie-find key tree)
                  :counting (not (equal value found)))))))

(deftest tree-iteration ()
  (let* ((reference (make-hash-table :test 'equal))
         (random (make-random-state t))
         (missing (cons 'missing 'element))
         (tree (loop
                  :with tree := (simple-hashtrie)
                  :for count :downfrom 1000 :above 0
                  :for key := (random 100000 random)
                  :for value := (incf (gethash key reference 0))
                  :do (setf tree (hashtrie-update key value tree))
                  :finally (return tree))))
    (let* ((keys-seen (hashtrie-fold ()
                                     (lambda (keys-seen key value)
                                      (let ((expected-value (gethash key reference missing)))
                                        (is (= value expected-value))
                                        (cons key keys-seen)))
                                    tree))
           (expected-keys (loop :for key :being :each :hash-key :of reference :collecting key))
           (sorted-expected-keys (sort expected-keys #'<))
           (sorted-keys-seen (sort keys-seen #'<)))
      ;; Make sure, that we have actually seen every key, and 
      ;; that each key was visited exactly once
      (is (= (hash-table-count reference) (length sorted-keys-seen)))
      (is (sorted-keys-unique-p sorted-keys-seen))
      (is (equal sorted-expected-keys sorted-keys-seen)))))
