---
title: Peeling zeroes in Idris
date: "2021-10-10T12:00:00.000Z"
description: An adventure with dependent types in Idris.
---

🎶 We're no strangers to lists, you know the rules and so do I 🎶

Anyway, here's one:

```elm
theBeatles : List String
theBeatles = ["John", "Paul", "George", "Ringo"]
```

If we want to get an element from a list at a specific index, we can
reach for a function like `index`:

```elm
index : Int -> List a -> Maybe a
```

Which we would use like this:

```elm
> index 0 theBeatles
Just "John"

> index 6 theBeatles
Nothing
```

The reason this function returns a `Maybe` is because we could pass _any_
number to it. It seems thus unescapable that we need to handle the case
when the given number is "out of bounds". It seems impossible to conjure a
function which at the same time is safe and behaves like this:

```elm
index : Int -> List a -> a
```

This seems peculiar, because unless something quite monstrous happens we
know that there will be no further changes to the Beatles lineup. So the
problem is that **we** know this but the type system doesn't, and it has
to treat that list as it would treat any other list.

Is there really no way out?

## Enter Idris

In [Idris](https://www.idris-lang.org/) there is another data structure that can describe lists called `Vect`:

```elm
theBeatles : Vect 4 String
theBeatles = ["John", "Paul", "George", "Ringo"]
```

What's going on here?

The type also contains information about the length of the list. If we made
a mistake and forgot one of the Fab Four, the compiler would let us know
promptly.

```elm
When unifying Vect 0 String and Vect 1 String.
Mismatch between: 0 and 1.

Test:6:39--6:40
 2 |
 3 | import Data.Vect
 4 |
 5 | theBeatles : Vect 4 String
 6 | theBeatles = ["John", "Paul", "George"]
                                           ^
```

This data structure allows us to write something this:

```elm
> index 0 theBeatles
"John"
```

Note that we get the raw value, no `Maybe` nonsense here. What about when
we pass a number that's out of bounds?

```idris
> index 6 theBeatles
Error: Can't find an implementation for
       IsJust (integerToFin 6 4).

(Interactive):1:12--1:14
 1 | index 6 theBeatles
```

Woah, now the compiler now understands. _Cool_. But what's the type
signature of this `index` function?

```elm
> :t index
index : Fin len -> Vect len elem -> elem
```

Uh, what that's `Fin` type? It describes a natural number strictly less
than some bound. The name comes from "finite sets", i.e. sets that have a
finite number of elements. Its definition is:

```elm
data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)
```

_Ok_.

We might need to take a little step back to unpack it.

## Natura, artis magistra

Natural numbers are integers greater or equal than zero. A neat trick to
represent them is to create a recursive type:

```elm
data Nat
  = Z
  | S Nat
```

Where `Z` represents zero, while `S Nat` represents the _successor_ of a
certain natural number.

Using this type we can construct the following values:

```plain
Z           == 0
S Z         == 1
S (S Z)     == 2
S (S (S Z)) == 3
```

You can see that Idris does a little bit of magic under the hood so that we
can write `1` instead of `S Z`. _Neat_.

A somewhat interesting fact is that we can also get the _previous_ natural
number by removing a layer of `S`:

```elm
prev : Nat -> Maybe Nat
prev Z = Nothing
prev (S x) = Just x
```

```elm
> prev 0
Nothing

> prev 1
Just 0

> prev 2
Just 1
```

Now that we know how to express natural numbers in Idris, we can take a
look at our finite subsets of natural numbers.

## The Meat and Potatoes

Let's look at that definition again:

```elm
data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)
```

This looks a tad different from the previous `data Nat = Z | S Nat`
definition, but it's actually quite close:
- We are defining a function called `Fin` that takes a natural number and returns a type.
- One possible constructor is `FZ`, which stands for "finite zero".
- The other constructor is `FS`, which takes an existing `Fin` and returns its successor.

From this explanation it's not clear _how_ we are restricting these natural numbers,
so let's play around with some finite numbers. Since a literal `0` can
represent many different numbers we can use the `the` function to tell
Idris that we want a specific type:

```elm
> the (Fin 1) 0
FZ
```

All good here, nothing to see. How about this?

```elm
> the (Fin 0) 0
```

This blows up..

```
Error: Can't find an implementation
for IsJust (integerToFin 0 0).

(Interactive):1:13--1:14
 1 | the (Fin 0) 0
```

By definition the minimum value we can create is `FZ`. The type of this
value is `Fin (S k)`, and by definition `S k` must be greater than zero. So
this means that the smallest set we can create is `Fin 1`, which the set
that only contains zero.

Let's look at `Fin 2`, the set that contains `0` and `1`.

```elm
> the (Fin 2) 0
FZ

> the (Fin 2) 1
FS FZ
```

As expected, the zeroes in `Fin 1` and `Fin 2` are different:

```elm
> the (Fin 1) 0 == the (Fin 2) 0
Error: When unifying Fin 1 and Fin 2.
Mismatch between: 0 and 1.

(Interactive):1:1--1:14
 1 | the (Fin 1) 0 == the (Fin 2) 0
     ^^^^^^^^^^^^^
```

Ok, time for a little surprise. Look at these values again:

```elm
> the (Fin 2) 0
FZ

> the (Fin 2) 1
FS FZ
```

Those `FZ` values are not the same! The first one is of type `Fin 2`, while
the second one is of type `Fin 1`. This makes sense when we look at the
type of the `FS` function:

```elm
FS : Fin k -> Fin (S k)
```

Or making it more concrete:

```elm
FS : Fin 1 -> Fin 2
```

- we get a `0` of type `Fin 1`
- we apply `FS`
- we get a `1` of type `Fin 2`

How would we get a `2` of type `Fin 2`? We would need to apply the function
twice, going from `Fin 0` to `Fin 1` to `Fin 2`, right?

The problem is, such value simply cannot be created.

The smallest valid finite set is `Fin 1`, which means we just _cannot_
apply that function twice to create that value. You can only apply it once,
giving us `1`.

If we look at `Fin 3`, things start making more sense:

```elm
> the (Fin 3) 0
FZ

> the (Fin 3) 1
FS FZ

> the (Fin 3) 2
FS (FS FZ)

> the (Fin 3) 3
Error: Can't find an implementation
for IsJust (integerToFin 3 3).

(Interactive):1:13--1:14
 1 | the (Fin 3) 3
```

It's like we're starting with this beefy zero, and we need to peel a layer
to be able to create the next number. Then we keep peeling and peeling, up
until we just can't peel anything anymore. `Fin 1` is the end of all hopes,
the last laugh, the final curtain.

I hope you enjoyed this adventure in the land of zeroes and dependent types.

Fin 1.
