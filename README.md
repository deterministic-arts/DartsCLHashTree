
Functional Maps and Trees
==========================

This project provides a few purely functional data structures.
Right now, it provides:

  - Hash Trees: purely functional map, based on hashing. The
    implementation is derived from Phil Bagwell's paper "Ideal Hash 
    Trees"

  - Property Tree: a purely functional (sorted) map from 
    strings to arbitrary values. The implementation is derived
    from S. Adams' paper "Implementing Sets Efficiently in a Functional 
    Language"

The tree structures provided by this project are immutable, i.e.,
cannot be modified once they have been constructed. Mutation 
operations return newly constructed tree instances (which may 
actually share state with the original tree, to which the 
operation was applied).


Hash Tree
----------

A hash tree is a trie structure indexed by the hash codes of the
keys. See the paper for details on the algorithm. Hash trees are
provided by the package `DARTS.LIB.HASHTREE`.

Unlike Common Lisp's built-in hash tables, hash trees can use any
equivalence predicate and hash function you care to provide. The
default equivalence predicate is `eql`, and the default hash
function is `sxhash`.

- `make-hashtree &key test hash` => `tree` Creates and returns a new
  hash tree, which uses the given equivalence predicate `test` (defaults 
  to `eql`) hash function `hash` (defaults to `sxhash`).

- `hashtree-count tree` => `number` Returns the number of entries
  in the given hashtree `tree`

- `hashtree-hash tree` => `function` Returns the hash function used
  by the given hashtree.

- `hashtree-test tree` => `function` Returns the equivalence predicate
  used by `tree`.

- `hashtree-map function tree` Calls `function` for each entry in
  `tree`. The function must accept two arguments, the entry's key
  value as first, and the entry's associated value as second argument.
  `hashtree-map` always returns `nil`.

- `hashtree-keys tree` => `list` Returns a list containing all key
  values present in the given `tree`, in no particular order.

- `hashtree-values tree` => `list` Returns a list containing all associated
  values present in the given `tree`, in no particular order.

- `hashtree-pairs tree` => `list` Returns a list of pairs (`key` . `value`)
  containing all entries of `tree`.

- `do-hashtree (key value) tree &body body` Iterates over all entries of
  the hashtree, which is the result of evaluating `tree`. For each entry,
  binds `key` to the entry's key and `value` to its associated value,
  then evaluates the forms in `body` as `progn` would do. The result of 
  this macro is undefined.

- `hashtree-empty-p tree` => `flag` Tests, whether `tree` is empty

- `hashtreep value` => `flag` Tests, whether `value` is a hash tree.

- `hashtree-get key tree &optional default` => `value`, `indicator` 
  Searches for the value associated with `key` in `tree`. If found,
  returns it as primary result, and `t` as second result. If no 
  matching entry exists, returns `default` as primary result, and
  `nil` as the secondary.

- `hashtree-remove key tree` => `new-tree`, `indicator`
  Removes the entry for `key` from `tree`, returning the resulting new
  hash tree as primary return value. The secondary return value is a
  boolean flag indicating, whether a matching entry was found (and
  removed), or not. 

- `hashtree-update key value tree` => `new-tree`, `action`
  Inserts a key/value pair or replaces the value in an existing pair
  in `tree`. Returns the new tree as primary return value. The secondary
  value is an indicator of what was done by the call. Possible values
  are `:added` if no matching key was present prior to the call, and
  `:replaced`, if the value of an existing entry was replaced by the
  new value `value`.

- `hashtree-fold function seed tree` => `value`


Property Trees
---------------

A property tree is a weight-balanced binary search tree. Right now, the
implementation only supports string designators as keys. The algorithms
are easily generalizable, but I could not yet decide on an interface for
that, and string keys were the only ones, I needed up to now.

Since property trees are actually ordered, all iteration functions (like
`ptree-map`, `ptree-fold`, ...) iterate over the entries of a property 
tree in proper key order. Likewise, collector functions like `ptree-keys`,
`ptree-values`, ... will always yield the elements in proper key order.

- `ptreep value` => `flag`
- `empty-ptree-p tree` => `flag`
- `ptree-size tree` => `integer`
- `ptree-key tree` => `value`
- `ptree-value tree` => `value`
- `ptree-left tree` => `ptree`
- `ptree-right tree` => `ptree`
- `ptree-minimum tree` => `ptree`
- `ptree-maximum tree` => `ptree`
- `ptree-smallest key tree` => `ptree`
- `ptree-largest key tree` => `ptree`
- `ptree-get key tree &optional default` => `value`, `flag`
- `ptree-insert key value tree &optional test` => `ptree`, `flag`
- `ptree-update key tree mutator` => `ptree`, `flag`
- `ptree-remove key tree` => `ptree`, `flag`
- `ptree-map function tree &key direction collectp start end` => `value`
- `ptree-fold function seed tree &key direction start end` => `value`
- `ptree-pairs tree` => `list`
- `ptree-keys tree` => `list`
- `ptree-values tree` => `list`
- `ptree-intersection tree1 tree2` => `ptree`
- `ptree-union tree1 tree2` => `ptree`
- `ptree-difference tree1 tree2` => `ptree`
- `ptree-iterator tree` => `function`
- `ptree-equal tree1 tree2` => `boolean`
- `ptree &rest pairs` => `ptree`


Future Plans
-------------

- Generalization of the property tree code to arbitrary `<`-style
  comparison predicates

- Homogenization of the hashtree and ptree interfaces (e.g., `ptree-update`
  vs. `ptree-insert` vs. `hashtree-update`)

