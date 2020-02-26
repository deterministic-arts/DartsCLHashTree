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


(defpackage #:darts.lib.hashtrie
  (:use #:common-lisp)
  (:export 
    #:hashtrie #:hashtriep #:hashtrie-empty-p #:hashtrie-fold #:hashtrie-map 
    #:hashtrie-count #:define-hashtrie #:hashtrie-control #:hashtrie-find 
    #:hashtrie-update #:hashtrie-remove #:do-hashtrie #:hashtrie-keys
    #:hashtrie-values #:hashtrie-pairs #:simple-hashtrie #:make-simple-hashtrie
    #:simple-hashtrie-p)
  
  (:documentation "Purely functional hash-based map structure 

This package provides a purely functional data structure for mapping keys 
to values. The underlying algorithms are hash-based, and modelled after 
the paper ``Ideal Hash Trees´´ by Phil Bagwell. A few differences to Common 
Lisp's standard hash tables:

  - all objects are immutable after construction
  - you can specify any equality test predicate you need
  - works with any user supplied hash function

By being immutable, these structures are automatically thread-safe and
can be shared across any number of concurrently running threads safely.
That fact was actually the main motivation for the development of this
package.

Note, that hash trees as implemented here are not necessarily a simple
drop-in replacement for Common Lisp's standard hash tables. In particular,
hash trees work best with equal based equality (as opposed to eq). Also,
many Lisp implementations provide weak hash tables, which is not supported
by hash trees at all."))


