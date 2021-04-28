---
title: Complicated Haskell Words - Isomorphism
date: "2020-11-29T12:00:00.000Z"
description: isos "equal", morphe "shape".
---

As someone who has been learning Haskell on and off for the last year, I
know the struggles of just how complicated Haskell can be for beginners.
You are instantly thrown off in a new world full of new terms, each one
more obscure than the other. Being familiar with a statically typed
language like [Elm](https://elm-lang.org) certainly helped, but I felt I
struggled the most with the seemingly endless list of new words I would
bump into every day... _Isomorphism_, _functor_, _monoid_, _covariant_,
and so on and so forth.

So my goal here is to explain these terms using the simplest analogies I've
come across. Some of them might not be the most complete or the most
faithful, but my hope is that they will be useful lifeboats when you find
yourself lost at sea. I will try to keep this series updated over time by
adding new terms and simpler explanations. One word of warning: I won't be
talking about monads since there are plenty of
[tutorials](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)
and [burritos](https://blog.plover.com/prog/burritos.html) out there.

## Isomorphism

Let's start with something that's used all the time. If two things are
isomorphic, it means that _for all intents and purposes they are the same_.
Anytime you read _isomorphic_, just read _more or less the same for what we
care about_. 

This should be enough information for you to get on with your day. You can
close the tab now. Thanks for reading!

The rest of this post is about building an intuitive understanding of why
isomorphisms are just _so damn cool_.

Let's say we want to describe if we are happy or sad.

```haskell
data Mood = Happy | Sad
```

Now we could have a function that tells us if we are happy:

```haskell
areWeHappy :: Mood -> Bool
areWeHappy Happy = True
areWeHappy Sad = False
```

We could come up with a way to derive a `Mood` from a `Bool`:

```haskell
moodFromBool :: Bool -> Mood
moodFromBool True = Happy
moodFromBool False = Sad
```

If we take a look at this pair of functions we can see that we can chain
them together.

```haskell
mysteriousFunction =
  -- first run moodFromBool, then areWeHappy
  areWeHappy . moodFromBool
```

Let's try plugging some values to this mysterious function:

- if we pass `True`, it gets converted to `Happy`, then back to `True`
- if we pass `False`, it gets converted to `Sad`, then back to `False`

So we basically implemented an identity function:

```haskell
id :: a -> a
id x = x
```

This doesn't sound too interesting, I know. What's also not too interesting
is that there is another way to combine those two functions:

```haskell
otherMysteriousFunction =
  -- first run areWeHappy, then moodFromBool
  moodFromBool . areWeHappy
```

- if we pass `Happy`, it gets converted to `True`, then back to `Happy`
- if we pass `Sad`, it gets converted to `False`, then back to `Sad`

Another day, another identity function. What's new?

### Intermezzo, or why is zero cool

The ancient Greeks were thoroughly confused by the number zero and used to wonder:

> How can nothing be something?

Adding zero to an existing number is a no-op, it doesn't do anything at
all. It's a complete waste of effort!

The great thing about zero is that it helps us to define a relationship
between numbers. The number `3` has now this beautiful property with
itself:

```none
3 - 3 = 0
```

Not only that, without zero we wouldn't even have negative numbers! Zero
stands as the legendary gatekeeper between positive and negative numbers.
Now each positive number `a` has a friend in the nether world `a'`, which relation
is defined as:

```haskell
a + a' = 0
```

### Back to identity functions

That's the same reason why identity functions are cool. They are a way to
establish _interesting relationships_ between other functions.

Let's do a quick refresher on the functions we had:

```haskell
areWeHappy :: Mood -> Bool
areWeHappy Happy = True
areWeHappy Sad = False

moodFromBool :: Bool -> Mood
moodFromBool True = Happy
moodFromBool False = Sad

mysteriousFunction :: Bool -> Bool
mysteriousFunction =
  areWeHappy . moodFromBool
```

If our mysterious function is an identity function, it means we have
_successfully undone_ what `moodFromBool` did in the first place. We were
able to implement Control-Z for a function! We went from `Bool` to
`Mood` and then back to `Mood` without losing any information.

```haskell
otherMysteriousFunction :: Mood -> Mood
otherMysteriousFunction =
  moodFromBool . areWeHappy
```

In the other mysterious function, we were able to go from `Mood` to `Bool`
and then back to `Mood` without losing information.

### The definition™

Two types `A` and `B` are isomorphic if two conversion functions exist such
that:

```haskell
f :: A -> B
g :: B -> A
```

And these two properties are valid:

```haskell
f . g = id
g . f = id
```

Having this pair of functions means that we can losslessly convert between
values of type `A` and values of type `B`.

So this is what we meant at the beginning when we said that _for all
intents and purposes_ `A` and `B` are the same!

### Many isomorphisms

Note that the multiple isomorphisms can exist between two types. For
example we could have this:

```haskell
areWeSad :: Mood -> Bool
areWeSad Sad = True
areWeSad Happy = False

moodFromBoolSad :: Bool -> Mood
moodFromBoolSad True = Sad
moodFromBoolSad False = Happy
```

What's the lesson here? All isomorphisms are born equal, and no isomorphism
is more equal than others.

### Some examples of isomorphisms

We can easily prove that `(a, b)` and `(b, a)` are isomorphic by importing
the `Data.Tuple` module:

```haskell
f :: (a, b) -> (b, a)
f = swap

g :: (b, a) -> (a, b)
g = swap
```

In the same module, there's another couple of functions:

```haskell
curry :: ((a, b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> (a, b) -> c
```

Those type signatures look quite funky, right? `curry` lets us apply a
function, originally intended to be used on a tuple, on two separate
arguments:

```haskell
Prelude> :t fst
fst :: (a, b) -> a

Prelude> fst (1, 2)
1

Prelude> curry fst 1 2
1
```

While `uncurry` lets us apply a function, originally intended to be used on
two arguments, on a tuple:

```haskell
Prelude> (+) 1 2
3

Prelude> uncurry (+) (1, 2)
3
```

Let's prove that these functions create an isomorphism:

```haskell
Prelude> uncurry (curry fst) (1, 2)
1

Prelude> curry (uncurry (+)) 1 2
3
```

After looking at the type signatures again we can say that these following
functions are isomorphic:

```haskell
((a, b) -> c) -> a -> b -> c
(a -> b -> c) -> (a, b) -> c
```

Pretty neat!

### Unit

In Haskell you would sometimes see this type:

```haskell
foo :: ()
```

It's an empty tuple, which is referred to as the _unit_. It can contain a
single value, which can only be:

```haskell
()
```

It turns out to be quite useful when you need a type that contains a single
value.

For example, we could prove that `Bool` and `Either () ()` are isomorphic:

```haskell
f :: Bool -> Either () ()
f True = Right ()
f False = Left ()

g :: Either () () -> Bool
g (Right ()) = True
g (Left ()) = False
```

Or that `Maybe a` is isomorphic to `Either () a`

```haskell
f :: Maybe a -> Either () a
f (Just x) = Right a
f Nothing = Left ()

g :: Either () a -> Maybe a
g (Right a) = Just a
g (Left ()) = Nothing
```

### One more thing...

[Peano numbers](https://en.wikipedia.org/wiki/Peano_axioms) are a simple way of representing natural numbers using a zero value and a successor function:

```haskell
data PeanoNum
  = Zero
  | Succ PeanoNum
  deriving (Show)
```

We can construct an isomorphism between this representation and `Int`
(let's just pretend for a moment that negative numbers aren't a thing):

```haskell
toInt :: PeanoNum -> Int
toInt Zero = 0
toInt (Succ x) = toInt x + 1

fromInt :: Int -> PeanoNum
fromInt 0 = Zero
fromInt x = Succ (fromInt (x - 1))
```

Let's take it for a spin:

```haskell
Prelude> toInt Zero
0

Prelude> toInt (Succ (Succ Zero))
2

Prelude> fromInt 3
Succ (Succ (Succ Zero))

Prelude> toInt (fromInt 3)
3
```

🎉

Thanks for reading, I hope you have emerged from this with a newfound sense
of awe for isomorphisms and identity functions!
