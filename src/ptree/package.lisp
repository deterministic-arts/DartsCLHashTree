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


(defpackage "DARTS.LIB.PTREE"
  (:use "COMMON-LISP")
  (:export "PTREE" "PTREEP" "+EMPTY-PTREE+" "PTREE-EMPTY-P" "PTREE-KEY"
           "PTREE-VALUE" "PTREE-SIZE" "PTREE-GET" "PTREE-INSERT" "PTREE-LEFT"
           "PTREE-RIGHT" "PTREE-MINIMUM" "PTREE-MAXIMUM" "PTREE-SMALLEST"
           "PTREE-LARGEST" "PTREE-REMOVE" "PTREE-MAP" "PTREE-UPDATE"
           "PTREE-UNION" "PTREE-INTERSECTION" "PTREE-DIFFERENCE" "PTREE-EQUAL"
           "PTREE-ITERATOR" "PTREE-FIND" "PTREE-FOLD" "PTREE-VALUES" "PTREE-KEYS"
           "PTREE-PAIRS")
  (:documentation "Provides a simple binary search tree implementation, 
which uses strings as keys. The tree is balanced. This code is based on the
paper Implementing Sets Efficiently in a Functional Language by S. Adams.
Short of renaming a few things and adapting the code to Common Lisp, the
algorithms presented there were used pretty much as-is."))
