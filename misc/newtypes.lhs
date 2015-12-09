Type synonyms and newtypes
--------------------------

Suppose we have a type `T` and we want to make another type which is
"the same as" `T`.  We have two options.

**Type synonyms**

The first option is to make a *type synonym*, introduced with the
`type` keyword:

~~~~ {.haskell}
type S = T
~~~~

This creates `S` as a type synonym for `T`.  `S` is simply a
*different name* for `T`, so they can be used interchangeably.

When would we want to do this?

* As abbreviation: `T` is long and frequently used, so we want to give
  it a shorter name `S` in order to save us some typing (and make type
  signatures easier to read).
* As documentation: for example, suppose a function takes three
  `String`s, where the first two represent names and the third
  represents a message.  We could write

    > type Name = String
    > type Message = String
    >
    > f :: Name -> Name -> Message -> Int

    This way it would be more obvious to someone reading the type of `f`
    what arguments it expects, but they can still pass `String` values
    to `f` as before.

**Newtypes**

The other option is to create a `newtype`, like this:

~~~~ {.haskell}
newtype N = C T
~~~~

The idea is that this creates a "new type" `N` which is *isomorphic* to
`T` but is a separate type---unlike with type synonyms, the compiler
will complain if we mix them up.  We cannot pass a value of type `T`
as an argument when an `N` is expected, or vice versa.  In order to
convert between them we need to use the constructor `C`---either
applying it (to convert from `T` to `N`) or pattern-matching on it (to
convert from `N` to `T`).

This has several common uses:

  1. To get the compiler's help distinguishing between types we do not
     want to mix up.  For example, if we have two types, one for
     representing meters and one for representing feet, we probably
     want to represent them as

    ~~~~ {.haskell}
    newtype Meters = Meters Double
    newtype Feet   = Feet Double
    ~~~~

	 This way, if we accidentally use a value in meters where we meant
	 to use one in feet, we will get a compiler error instead of
	 having our $125 million Mars probe crash.

  1. As we saw last week, if we want to give multiple type class
     instances to the same type, we can instead wrap the type in several
     different `newtype`s and give an instance for each.

So why use `newtype` instead of just

~~~~ {.haskell}
data N = C T
~~~~

? There are several ways in which `newtype` differs from `data`:

  1. `newtype`s may only have a single constructor with a single
     argument.  This may seem like an annoying restriction, but the point
     is that...

  1. `newtype`s have *no run-time cost*.  That is, at run-time, values
     of the types `N` and `T` will be represented *identically* in
     memory.  If we had instead written `data N = C T` values of type
     `N` would be paired with a "tag" to indicate the constructor `C`.
     Since `newtype`s can only have a single constructor with a single
     value inside it, there is no need to actually store the
     constructor.

  1. GHC has an extension called `GeneralizedNewtypeDeriving` which
     allows one to automatically derive type class instances for a
     `newtype` based on instances for the underlying type.  For
     example, instead of writing

    ~~~~ {.haskell}
    newtype Moo = Moo Int

    instance Num Moo where
      (Moo x) + (Moo y) = Moo (x + y)
      (Moo x) * (Moo y) = Moo (x * y)
      abs (Moo x)       = Moo (abs x)
      ...
    ~~~~

    we can just write

    ~~~~ {.haskell}
    {-# LANGUAGE GeneralizedNewtypeDeriving #-}

    newtype Moo = Moo Int
      deriving (Num)
    ~~~~
