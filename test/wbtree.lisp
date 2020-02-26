#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Hash Tree
  Copyright (c) 2013, 2015, 2020 Dirk Esser

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

(in-package #:darts.lib.wbtree.test)

(define-wbtree inttree 
  (:test <)
  (:spread-constructor inttree))

(in-root-suite)

(defsuite wbtree-test-suite)

(in-suite wbtree-test-suite)

(deftest build-from-sorted-sequence ()
  (loop
     :with seed := (inttree)
     :for k :upfrom 0 :below 100
     :do (setf seed (wbtree-update k k seed))
     :finally (wbtree-check-invariants seed)
              (is (not (wbtree-empty-p seed)))
              (is (eql 100 (wbtree-size seed)))
              (loop
                 :for k :upfrom 0 :below 100
                 :do (multiple-value-bind (value indicator) (wbtree-find k seed)
                       (is indicator)
                       (is (eql value k))))
              (multiple-value-bind (value indicator) (wbtree-find -1 seed)
                (is (not indicator))
                (is (not value)))
              (multiple-value-bind (value indicator) (wbtree-find 100 seed)
                (is (not indicator))
                (is (not value)))))
     
         
(deftest lower-boundaries ()
  (let ((tree (inttree 1 :a 2 :b 3 :c 4 :d)))
    (let ((lower-0 (wbtree-lower-boundary-node 0 tree))
          (lower-1 (wbtree-lower-boundary-node 1 tree))
          (lower-4 (wbtree-lower-boundary-node 4 tree))
          (lower-5 (wbtree-lower-boundary-node 5 tree)))
      (is (not lower-0))
      (is (and lower-1 (eql (wbtree-node-key lower-1) 1)))
      (is (and lower-4 (eql (wbtree-node-key lower-4) 4)))
      (is (eq lower-5 lower-4)))))


(deftest upper-boundaries ()
  (let ((tree (inttree 1 :a 2 :b 3 :c 4 :d)))
    (let ((lower-0 (wbtree-upper-boundary-node 0 tree))
          (lower-1 (wbtree-upper-boundary-node 1 tree))
          (lower-4 (wbtree-upper-boundary-node 4 tree))
          (lower-5 (wbtree-upper-boundary-node 5 tree)))
      (is (and lower-1 (eql (wbtree-node-key lower-1) 1)))
      (is (and lower-4 (eql (wbtree-node-key lower-4) 4)))
      (is (eq lower-0 lower-1))
      (is (not lower-5)))))
