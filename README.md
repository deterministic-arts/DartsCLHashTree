
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


Generic API 1 (works on any hash trie)
.......................................

The following functions are defined for any flavour of hash trie. 

- function hashtriep value => boolean
- function hashtrie-empty-p trie => boolean
- function hashtrie-count trie => integer
- function hashtrie-fold seed function trie => value
- function hashtrie-map function trie => unspecified
- function hashtrie-control trie => hash test


Generic API 2 (works on any hash trie)
.......................................

The following functions are defined for any flavour of hash trie; 
they are, however, not usually used, and will certainly be not as
efficient as using the versions implicitly defined by 
`define-hashtrie`:

- function hashtrie-get key trie &optional default => value indicator
- function hashtrie-update key value trie => new-trie old-value indicator
- function hashtrie-remove key trie => new-trie old-value indicator


Deprecated API
...............

All functions and macros with `hashtree` in their name (instead
of `hashtrie` -- note the i vs e) are deprecated. The new interface
allows you to do almost anything you could do with the old 
interface, but more efficiently. The most important omission of
the new interface is the construction of hash tries on the fly
at run-time. This is still something, you can only do with the
old API (or some `EVAL` magic, but that's a path I don't want to
walk...)



Property Trees
---------------

Warning: This package is a now deprecated precursor to the more general
darts.lib.wbtree package. The package will not suddenly disappear, but
the nowadays, I am using the more general wbtree package, and keep this
one only for the legacy code. All future development will happen in wbtree.

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

