
# InTime

## Introduction

`InTime` is as data type for informal analysis of time complexity
using dependent types.

It allows the user to perform time-complexity analysis on an
algorithm. For example, one declares that X operation runs in Y
time. E.g., that insertion sort runs in, at worst, `O(n^2)`.

The type system tracks the time complexity. It does this by annotating
the data structures upon which the algorithm acts, reflecting the
number of steps required (thus far) to create them.


## Questions

There are a number of questions about how best to implement this
library. They include:

+ How to indicate the "steps"?
+ Instance of `Monad`?
+ How to indicate Big-O (rather than step-for-step) complexity?
+ Lazy vs. eager evaluation?
+ How to compute logarithms at the type level?

only 2 things: the constant & loops


## Expressing time complexity of indeterminate # of steps

Consider the case of `insert` - inserting a value into a sorted Vec
such that the result is also sorted. The number of steps depends on
the value of the to-be-inserted value as well as the values in the
Vec. It may take only a single step, or it may take as many as there
are elements in the Vec.

How do we express this in the types? We'd like to be able to write
something like this:

```haskell
insert ∷ α → Vec n α → InTime s (Vec (n+1) α)
```

Where `s` is some Nat between 1 and `(n+1)` (or perhaps `(2*n+1)`).

In the [blog post](http://twanvl.nl/blog/agda/sorting), the author
wishes to "reason about the runtime, as measured in the number of
comparisons performed".


## How to indicate what the "steps" are?

When analyzing the time complexity of algorithms, one wishes to
consider which operations roughly constitute a unit of
time. [Here](Docs/steps.md) are some thoughts about how to indicate which
those are.


## A Monad?

We'd like our data type to be an instance of Monad, but it doesn't
work. [Here](Docs/monadish.md) are some ideas about what to do about that.
