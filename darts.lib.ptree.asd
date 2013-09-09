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

(in-package "COMMON-LISP-USER")
(defpackage "DARTS.ASDF" (:use "COMMON-LISP" "ASDF"))
(in-package "DARTS.ASDF")

(defsystem :darts.lib.ptree
  :name "darts.lib.ptree"
  :author "Dirk Esser"
  :version "0.1"
  :maintainer "Dirk Esser"
  :licence "MIT"
  :description "Weight-balanced binary tree (string keys only)"
  :long-description "This system exists mainly for compatibility. It provides
   a weight balanced binary tree, which is specialized for string keys, using
   a slightly different interface compared to the full `darts.lib.wbtree'
   package. Users of this system should consider using the `darts.lib.wbtree'
   package instead, since I am going to drop support for the ptree stuff
   soon."
  :depends-on ()
  :components
  ((:module :src
    :components
    ((:module :ptree
      :components
      ((:file "package")
       (:file "implementation" :depends-on ("package"))))))))
