
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

  - type `wbtree`

    This is the base type for all weight-balanced binary trees. Every
    new tree type introduced by a `define-wbtree` form is a subtype
    of this type.

  - macro `define-wbtree`

    This macro introduces a new subtype of `wbtree`, as well as a
    bunch of functions. This macro accepts two kinds of usage. The
    simplified form exists primarily for backwards compatibility
    reasons:

        define-wbtree name predicate &optional docstring

    It is equivalent to using the complex form

        (define-wbtree name 
          (:test predicate)
          (:constructor nil)
          (:spread-constructor name)
          (:documentation docstring))

    The long form has the format

        define-wbtree name clauses...

    where `name` is a symbol naming the new tree type, and each element
    of `clauses` may be one of

      - `(:test function)`

        Identifies the test function which is used to compare keys. The
        given function must be a binary function, which answers true, if
        its first argument is *strictly* less than its second argument.

      - `(:key function)`

        Provides a transformation `function`, which is applied to keys
        before they are processed further.

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

      - `(:predicate name)`

        Provides the name of the type predicate function, which answers
        true for any object, which is a wbtree of the newly defined type,
        and false for any other value. If you supply `nil` as the name,
        then no type predicate is generated. If you supply `t` (the default),
        the predicate name follows the standard rules used by `defstruct`.

      - `(:documentation string)`
    
  - function `wbtreep object` => `boolean` 

    Answers true, if `object` is a `wbtree` instance, and false otherwise

  - function `wbtree-empty-p tree` => `boolean` 

    Answers true, if wbtree instance `tree` is empty, i.e., does not 
    contain any key/value pairs.

  - function `wbtree-size tree` => `integer` 

    Answers the number of key/value pairs contained in wbtree instance 
    `tree`.

  - function `wbtree-node-value tree` => `value`

    Returns the value stored in the tree node `tree`. If `tree` is empty,
    signals a condition of type `simple-error`.

  - function `wbtree-node-key tree` => `key` 

    Returns the key stored in the tree node `tree`. If `tree` is empty,
    signals a condition of type `simple-error`.

  - function `wbtree-node-left-subtree tree` => `subtree` 

    Answers the left subtree under tree node `tree`. If `tree` is empty,
    answer the node itself (i.e., `tree`). FIXME: this is a strange idea;
    why did I do it this way?

  - function `wbtree-node-right-subtree tree` => `subtree` 

    Answers the right subtree under tree node `tree`. If `tree` is empty,
    answer the node itself (i.e., `tree`). FIXME: this is a strange idea;
    why did I do it this way?

  - function `wbtree-minimum-node tree` => `node` 

    Answers the tree node with the smallest key value (with respect to the
    tree's predicate function) in the given wbtree `tree`. If the tree is
    empty, returns `nil` instead.

  - function `wbtree-maximum-node tree` => `node` 

    Answers the tree node with the largest key value (with respect to the
    tree's predicate function) in the given wbtree `tree`. If the tree is
    empty, returns `nil` instead.

  - function `wbtree-ceiling-node key tree` => `node`

    Answers the node with the smallest key `k`, which is still equal to
    or greater than `key` in `tree`. If there is no matching key in `tree`,
    this function answer `nil`.

  - function `wbtree-floor-node key tree` => `node`

    Answers the node with the largest key `k`, which is still equal to
    or less than `key` in `tree`. If there is no matching key in `tree`,
    this function answer `nil`.

  - function `wbtree-update key value tree &optional test` => `new-tree`, `indicator` 

    Answers a wbtree, which contains an association of the given `key`,
    mapping it to `value`. The resulting tree is a copy of `tree`, potentially
    modified to hold the new or updated mapping.

    If the value of `key` in `tree` is already "equal" to `value` (as
    is determined using `test`), the resulting wbtree `new-tree` will be 
    `tree` itself.

    The secondary return value `indicator` can be used to find out,
    what changes have been applied to `tree`. Possible values are

    - `nil` No changes have been made, since the value found for the
      given `key` was already "equal" to the new `value` according to
      `test`.

    - `:added` A new key/value pair had to be added, since there was
      no previous mapping for `key` in `tree`.

    - `:replaced` The previous value of `key` in `tree` has been
      replaced by the given `value`.    

    FIXME: A more meaningful convention would be to return one of
    `nil` (no changes), `t` (node added), and "old node" (node
    replaced).

  - function `wbtree-remove key tree` => `new-tree`, `indicator` 

    Answers a copy of `tree`, which does not contain a key/value pair,
    whose key matches `key`. The secondary value `indicator` indicates,
    which changes have been applied to `tree`. Possible values are

    - `nil` There was no entry matching `key`. The `new-tree` is actually
      the `tree` itself.

    - The node, which held the old mapping of `key` (and which has been
      removed in `new-tree`)

  - function `wbtree-find key tree &optional default` => `value`, `indicator` 

    Answers the value associated with `key` in the wbtree `tree`, or `default`,
    if the key is not present in `tree`. The `indicator` returned as secondary
    value is `nil`, if no matching entry is found in `tree`, or the actual
    tree node, which represent's the association, otherwise.

  - function `wbtree-find-node key tree` => `node`

    Answers the tree node, whose key matches `key` in `tree`, or `nil`,
    if there is no entry matching `key` in `tree`.

  - function `wbtree-map function tree &key direction collectp start end` => `result` 

    Applies `function` to every node in wbtree `tree`. If `collectp`, the 
    results of each invocation are collected into a freshly allocated list,
    which is returned from `wbtree-map` after the traversal. If `collectp`
    is omitted or `nil`, the results of `function` are ignored, and the
    `result` value is `nil`.

    If `direction` is `:forward` (the default), the traversal is performed
    in the direction from "smaller" key values to "larger" key values. 
    If `start` is given, the traversal starts at the node with the smallest
    key, which is equal to or greater than value `start`, and will stop
    before reaching any node, whose key is equal to or greater than the
    given `end`. If no `end` is supplied, the traversal stops after all
    nodes have been visited.

    If `direction` is `:backward`, the traversal is performed in the direction 
    from "larger" key values to "smaller" key values. If `start` is given, 
    the traversal starts at the node with the largest key, which is equal to 
    or less than value `start`, and will stop before reaching any node, 
    whose key is equal to or less than the given `end`. If no `end` is 
    supplied, the traversal stops after all nodes have been visited.
    
  - function `wbtree-fold function tree &key direction associativity initial-value start end` => `result` 

    Generates a "summary" of `tree` by invoking `function` for all
    nodes. The arguments passed to `function` depend on the value of
    `associativity` as follows:

    - if `associativity` is `:left`, which is the default, the function 
      is called with the previous summary value as first, and the tree 
      node as second parameter.

    - if `associativity` is `:right`, the function is called with the
      tree node as first, and the previous summary value as second 
      argument.

    In either case, the value returned by the function will be used 
    as the new summary value in the next invocation or the final
    `result`, if all nodes have been processed. On the first invocation,
    the value supplied as `initial-value` is used; if the tree is
    empty, the `initial-value` will be returned as `result`. 

    If `direction` is `:forward` (the default), the traversal is performed
    in the direction from "smaller" key values to "larger" key values. 
    If `start` is given, the traversal starts at the node with the smallest
    key, which is equal to or greater than value `start`, and will stop
    before reaching any node, whose key is equal to or greater than the
    given `end`. If no `end` is supplied, the traversal stops after all
    nodes have been visited.

    If `direction` is `:backward`, the traversal is performed in the direction 
    from "larger" key values to "smaller" key values. If `start` is given, 
    the traversal starts at the node with the largest key, which is equal to 
    or less than value `start`, and will stop before reaching any node, 
    whose key is equal to or less than the given `end`. If no `end` is 
    supplied, the traversal stops after all nodes have been visited.

  - function `wbtree-difference tree1 tree2` => `new-tree` 
  - function `wbtree-union tree1 tree2 &key combiner` => `new-tree` 
  - function `wbtree-intersection tree1 tree2 &key combiner` => `new-tree` 
  - function `wbtree-iterator tree &key direction` => `iterator` 
  - function `wbtree-equal tree1 tree2 &key test` => `boolean` 

Debugging helpers and esoterica

  - function `wbtree-check-invariants tree`
  - function `wbtree-rebalance tree` => `new-tree`

Compatibility

  - function `wbtree-lower-boundary-node tree` => `node`
  - function `wbtree-upper-boundary-node tree` => `node`

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

- macro `define-hashtrie name &body clauses ...`

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

  - `(:key function)`

    Declares a transformation function, which is applied
    to all user-supplied hash key value prior to hashing. The
    function's result is what gets actually used as the hash
    key. Note, that if a key transformation is supplied, the
    original input value is not used or stored by the hash
    trie (except initially, when it is passed as argument to
    the transformation function).

    Example:

        (define-hashtrie uppercase-htrie
          (:test string=)
          (:key string-upcase))

        (setf *x* (make-uppercase-htrie (list "foo" "value-of-FOO" #\A "value-of-A" t "value-of-T")))
        (hashtrie-find #\t *x*)  ;; => "value-of-T"

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

- macro `do-hashtrie (key value trie) &body body` => `whatever`

  Enumerates the key/value pairs in the hash trie, form `trie`
  evaluates to. In each iteration, `key` and `value` are bound
  to each pair's key and value, and the forms in `body` are
  evaluated sequentially with these bindings in place.

  The whole expansion is wrapped into an anonymous `block`,
  allowing the `body` to abort the iteration by using `return`.
  This is the only way to provide a non-nil result value for
  the whole `do-hashtrie` form.