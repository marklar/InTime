
# A Monad?

We'd like our data type to be an instance of Monad. It "feels" like a
monad, and we'd like to allow use of monadic plumbing.

However, our libary's data type isn't really a Monad (or Applicative
or Functor), as the types don't check.


## Not (really) a Monad

For example, the typeclass Monad's type signature for (>>=) is as
such:

    -- class: Monad
    (>>=) ∷ m α → (α → m β) → m β

Notice: the monadic type ctor `m` remains constant throughout.

For our datatype (`InType`), we do *not* want the type of the
container to remain constant throughout. The whole point of `InTime`
is for function application to cause a "stepping" of the 'n', which
results in a changing the type. So instead of this:

    -- Acceptable signature for monadic instance
    (>>=) ∷ InTime n α → (α → InTime n β) → Intime n β

, the function signature would need to look more like this:

    -- Non-monadic signature
    (>>=) ∷ InTime m α → (a → InTime n β) → InTime (m+n) β

But that can't work as an instance of Monad.


## Monadish?

So is our `InTime` type a typeclass of anything?

Oleg discusses a Monad-like typeclass called `Monadish` (thanks,
John!):

http://okmij.org/ftp/Computation/monads.html#param-monad

Oleg writes:

```
The familiar State monad lets us represent computations with a state
that can be queried and updated. The state must have the same type
during the entire computation however. One sometimes wants to express
a computation where not only the value but also the type of the state
can be updated -- while maintaining static typing. We wish for a
parameterized `monad' that indexes each monadic type by an initial
(type)state and a final (type)state. The effect of an effectful
computation thus becomes apparent in the type of the computation, and
so can be statically reasoned about.
```

Perhaps `InTime` could be an instance of `Monadish`. There would be
limited practical benefit to making it so, but it would imply
something about the intended semantics of `InTime`.


## Do Notation

There exists a Haskell library called `codo-notation` which provides
"a notation for comonads, analogous to the do-notation for monads".

https://hackage.haskell.org/package/codo-notation

We could provide a similar library -- based on `codo-notation`,
presumably --, which provides do-like notation for `Monadish`es
generally, which we could use with `InTime`.
