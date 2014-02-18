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

(defpackage "DARTS.LIB.WBTREE"
  (:use "COMMON-LISP")
  (:export "WBTREE" "WBTREEP" "WBTREE-EMPTY-P" "WBTREE-SIZE" "WBTREE-NODE-VALUE" 
           "WBTREE-NODE-KEY" "WBTREE-NODE-LEFT-SUBTREE" "WBTREE-NODE-RIGHT-SUBTREE" 
           "WBTREE-UPDATE" "WBTREE-REMOVE" "WBTREE-MAP" "WBTREE-FIND-NODE" 
           "WBTREE-FIND" "WBTREE-DIFFERENCE" "WBTREE-UNION" "WBTREE-INTERSECTION" 
           "WBTREE-ITERATOR" "WBTREE-EQUAL" "DEFINE-WBTREE" "WBTREE-LOWER-BOUNDARY-NODE"
           "WBTREE-UPPER-BOUNDARY-NODE" "WBTREE-CHECK-INVARIANTS" "WBTREE-REBALANCE"
           "WBTREE-FOLD")
  (:documentation "Generalized weight-balanced binary search trees. This
package provides a variant of the weight-balanced binary trees implemented
in package DARTS.LIB.PTREE. The variant exposed here can be used with arbitrary
key types, provided, a total order is defined on the keys, for which a suitable
predicate function (in the sense of #'<) exists."))
