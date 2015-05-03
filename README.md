
Functional Maps and Trees
==========================

This project provides a few purely functional data structures.
Right now, it provides:

  - Hash Tries: purely functional map, based on hashing. The
    implementation is derived from Phil Bagwell's paper "Ideal Hash 
    Trees"

  - Weight-balanced Tree: a purely functional (sorted) map from 
    strings to arbitrary values. The implementation is derived
    from S. Adams' paper "Implementing Sets Efficiently in a Functional 
    Language"

The tree structures provided by this project are immutable, i.e.,
cannot be modified once they have been constructed. Mutation 
operations return newly constructed tree instances (which may 
actually share state with the original tree, to which the 
operation was applied).


Weight-Balanced Binary Trees
-----------------------------

The system `darts.lib.wbtree` provides support for weight-balanced
binary trees, modelled after the paper "Implementing Sets Efficiently 
in a Functional Language" by S. Adams.

Applications can define their own subtypes of the wbtree type, with
a specialized comparison predicate for the actual key type.


Hash Tries
----------

A hash trie is a purely functional data structure, which provides
a mapping from keys to values using hashing. Unlike Common Lisp's
standard hash tables, hash tries are immutable. Any operation,
which changes a hash trie returns a copy. Also, hash tries can 
use any equivalence predicate and hash function you care to provide. 
The default equivalence predicate is `eql`, and the default hash
function is `sxhash`.

The implementation of hash tries can be found in package `DARTS.LIB.HASHTRIE`.

Defining new hash trie flavours
................................

- macro define-hashtrie name &body clauses ... 

  Supported clauses are:

  - `(:hash function)`

    Declares `function` as the function, which computes 
    the hash values. The `function` must be name a function
    taking a single argument and returning a positive integer
    in the range of `(unsigned-byte 32)` (well, the implementation
    uses the bottom-most 32 bits only...).

    The default hash function is `sxhash`.

  - `(:test function)`

    Declares the function used to test, whether two keys are
    equal. The default test function is `eql`.

  - `(:predicate name)`

    Declares the name of the generated type predicate to be
    `name`. The predicate can be used (in addition to or instead
    of) `(typep ... 'name)` to test, whether a value is an
    instance of the newly defined hash trie type.

    You can use `nil` as `name` in order to suppress the 
    generation of an additional type predicate. By using `t`
    as name, you get a predicate with the default name (which
    is also the standard behaviour)

  - `(:constructor name)`

    Declares the name of the generated standard constructor 
    to be `name`. The constructor is a function of a single
    optional argument, which is a list of key/value pairs in
    property list style. The default constructor is named
    `make-NAME`. You may generate a constructor with the
    default name by omitting this option or using a name of
    `t`. By specifying this option with a name of `nil`, you
    can suppress the generation of a constructor function.

  - `(:spread-constructor name)`

    Declares the name of the generated "spread" constructor.
    This function is just like the regular constructor above,
    but takes the initial members as `&rest` argument. The
    default is to not generate a spread constructor, unless
    this option is specified explicitly.

    If you use a name of `t`, the spread constructor is
    generated using its default name, which is `make-NAME*`.
    By giving a name of `nil` (the default), generation of
    the spread constructor is disabled.

  - `(:documentation string)`

    Adds the given `string` as documentation string to the
    structure type definition, the macro expands into.

  After this macro's expansion has been evaluated, `name` 
  names a valid lisp structure type; in particular, the
  name can be used with `typep` as well as for CLOS method
  dispatch.

  Example:

      (define-hashtrie integer-htrie
        (:hash identity)
        (:test eql)
        (:constructor make-integer-htrie)
        (:documentation "A simple hash trie, whose keys
          are integers. We use the keys directly as their
          own hashes."))

  Note, that the values given to the `:test` and `:hash` options
  must both be suitable for having `function` wrapped around them.
  Literal `lambda` expressions are ok, and so are symbols naming
  functions.

- function `hashtriep value` => `boolean`

  Answers true, if `value` is a hash trie instance, and false
  otherwise. Note, that concrete hash trie implementations have
  their own specific predicates, too.

- function `hashtrie-empty-p trie` => `boolean`

  Answers true, if hash trie `trie` is empty, and false, if it
  contains at least one key/value pair.

- function `hashtrie-count trie` => `integer`

  Answers the number of key/value pairs contained in the given
  hash trie `trie`.

- function `hashtrie-fold seed function trie` => `value`

  Invokes `function` for each key/value pair in hash trie `trie`,
  passing three arguments along: the value returned by the
  function on the last invocation (or `seed` at the first call),
  the key, and its associated value. `Hashtrie-fold` returns
  the value of the last invocation of `function` or `seed`,
  if the `trie` is empty, and `function` is never called.

- function `hashtrie-map function trie` => `unspecified`

  Invokes `function` once for each key/value pair in `trie`,
  discarding any results.

- function `hashtrie-find key trie &optional default` => `value indicator`

  Answers the value associated with `key` in `trie`, or `default`,
  if there is no mapping for `key` in `trie`. The secondary value
  is a boolean indicator, which is true, if the key has been found,
  and false otherwise.

  This function defines a `setf` form just in the `ldb` (for example)
  does, i.e., if used with `setf`, the `trie` must indicate a valid
  place, which gets updated to hold the updated trie.

      (defvar *trie* (simple-hashtrie))
      
      ;; The trie is initially empty (no parameters have been 
      ;; handed down to the constructor).

      (hashtrie-find 1 *trie*)                 ;; Yields nil as 
                                               ;; result value

      (setf (hashtrie-find 1 *trie*) "First")  ;; Yields "First" as
                                               ;; result value

      ;; Now, the hash trie has been updated to contain a
      ;; mapping with key 1

      (hashtrie-find 1 *trie*)                 ;; Yields "First" as
                                               ;; result value

- function `hashtrie-update key value trie` => `new-trie old-value indicator`

  Answers a copy of `trie`, in which `key` is associated with
  `value`. 

- function `hashtrie-remove key trie` => `new-trie old-value indicator`

  Answers a copy of `trie`, from which any association of `key`
  has been removed.

- macro `do-hashtrie (key value trie) &body body => whatever

  Enumerates the key/value pairs in the hash trie, form `trie`
  evaluates to. In each iteration, `key` and `value` are bound
  to each pair's key and value, and the forms in `body` are
  evaluated sequentially with these bindings in place.

  The whole expansion is wrapped into an anonymous `block`,
  allowing the `body` to abort the iteration by using `return`.
  This is the only way to provide a non-nil result value for
  the whole `do-hashtrie` form.