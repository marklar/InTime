
# How to indicate what the "steps" are?

When analyzing the time complexity of algorithms, one wishes to
consider which operations roughly constitute a unit of time. How
should the implementer of the algorithm indicate what those are?


## Explicitly annotate "steps"?

One option is to annotate the algorithm's code explicitly, indicating
which operations in the algorithm constitute one "step" - i.e. cost
one unit of time.

As the algorithm (functionally) updates data structures, these "step"
annotations also update the types of these data structures.

We prioritize this solution, as it's very general, but another option
is…


## Pass in "step"-enhanced operator?

In the [blog post](http://twanvl.nl/blog/agda/sorting) upon which this
idea is based, the author works with a particular class of algorithms:
sorts. Sorts all involve a comparison operator (e.g. `(≤)`). And those
comparisons constitute the "steps" of the algorithm.

(FIXME: That's stated strangely.)

The author thus chooses to create and inject a modified version of
`(≤)` which is `InTime`-aware, incrementing the time-complexity count
by 1.

This is an interesting idea. Version 2?
