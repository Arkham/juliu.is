---
title: Performant Elm, Part III, Html.Lazy
date: "2019-10-21T12:00:00.000Z"
description: Being successful at being lazy.
---

Greetings traveler! This is the third instalment of a series on how to make
your Elm applications more performant. If you are into this kind of stuff,
you can read about [how to tell what your browser is really
doing](../performant-elm) and [how Html.Keyed can help you help Elm to be
faster](../performant-elm-html-keyed).

Today we are talk about being lazy. As an Italian, I know a thing or two
about being lazy. I'm not talking "Oh I spent Sunday binge-watching Derry
Girls" lazy, but more something like ["I got caught with 400kg of undelivered
mail in my flat"](https://www.theguardian.com/world/2018/apr/06/former-postman-found-with-400kg-of-undelivered-mail-in-italy) sort of lazy.

So [the Elm Guide](https://guide.elm-lang.org/optimization/lazy.html) does a fantastic job at explaining what `Html.Lazy` does. I really recommend going there and reading about it. Anyway, the main idea of it is this function:

```elm
lazy : (a -> Html msg) -> a -> Html msg
```

As you can see, we pass two arguments:

- a **function that takes a thing**
- a **thing**

When you're learning about functional programming, it's common to bump into
something called [referential
transparency](https://en.wikipedia.org/wiki/Referential_transparency)
(don't open that link, that's five minutes of your life you're never
getting back). In Elm it's simply called:

> Same input, same output

You're always guaranteed that any function in Elm will return the same
output, **if** you pass the same input. So in the context of web
applications, by using `lazy` you are telling Elm that you know for sure
that if the data ain't changing, then the view shouldn't change either.

## Intermezzo, Sieve of Eratosthenes

The [Sieve of
Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes) is a
very ancient algorithm devised for finding all prime numbers up to a given
limit. It works by marking each multiple of a prime number as non-prime, so
that when we're done we're left with all the prime numbers. Its beauty can
be admired in the following GIF:

![Sieve of Erasthothenes](./sieve.gif)

A naive implementation of the algorithm in Elm would look like this:

```elm
sieve : Int -> List Int
sieve limit =
    let
        numbers =
            List.range 2 limit

        last =
            limit
                |> toFloat
                |> sqrt
                |> round

        isMultiple n m =
            n /= m && modBy m n == 0
    in
    List.range 2 last
        |> List.foldl
            (\current result ->
                List.filter
                    (\elem -> not (isMultiple elem current))
                    result
            )
            numbers
```

Now we can use this function to make our Elm code as slow as we want 🎉

## Our testbed example

I have built a small Ellie app that calculates the number of prime numbers
up to a certain limit, which you can find
[here](https://ellie-app.com/bc2LYDftdQKa1). Here's a GIF of the app in
action:

![Prime before](./before.gif)

We are going to focus on the `view` function, which looks like this:

```elm
view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Toggle ] [ text "Toggle color" ]
        , div
            [ -- style stuff
            ]
            [ viewPrime 200000 ]
        ]

viewPrime : Int -> Html Msg
viewPrime limit =
    text
        ("There are "
            ++ String.fromInt (sieve limit |> List.length)
            ++ " prime numbers between between 2 and "
            ++ String.fromInt limit
        )
```

You can notice there is a noticeable lag in the UI after pressing the
"Toggle Color" button: this happens because after receiving the `Toggle`
event, the Elm runtime needs to rerun the `view` function, which triggers
again the sieve computation.

## Html.Lazy to the rescue

We can change this line from:

```elm
[ viewPrime 200000 ]
```

to this:

```elm
[ Html.Lazy.lazy viewPrime 200000 ]
```

and we get [this result](https://ellie-app.com/bc2KLQMsYkYa1):

![Prime after](./after.gif)

We can now see that after the initial call, we never really get inside the
`viewPrime` function anymore, thus making our application blazing fast. 🤟

## Happily ever after?

Unfortunately, as almost everything in else in life, nothing is as simple
as it seems. If you have ever tried to use `Html.Lazy` and failed to see
any improvements, you know how frustrating it can be. So I'm going to list
some common reasons on why it might not be working.

### Using anonymous functions

A very simple way to break `Html.Lazy` is to replace this line:

```elm
[ Html.Lazy.lazy viewPrime 200000 ]
```

with this:

```elm
[ Html.Lazy.lazy (\n -> viewPrime n) 200000 ]
```

You can check it out [here](https://ellie-app.com/bc2MVVJTLJwa1). Why is
that? Because `Html.Lazy` needs to associate the cached value with a
precise function, but specifying an anonymous function forces the runtime
to recreate that function every time the `view` is invoked.

### Constructing records on the fly

A similar problem happens when we construct a record on the fly and pass it
to `Html.Lazy`. Imagine our `viewPrime` looked like this:

```elm
viewPrime : { limit : Int } -> Html Msg
viewPrime { limit } =
    text
        ("There are "
            ++ String.fromInt (sieve limit |> List.length)
            ++ " prime numbers between between 2 and "
            ++ String.fromInt limit
        )
```

And now we called it by writing:

```elm
[ Html.Lazy.lazy viewPrime { limit = 200000 } ]
```

You will see the app going back to [being slow
again](https://ellie-app.com/bc2Q8mxQF8Ma1). This happens because when we
build a new record we invalidate the caching mechanism of `Html.Lazy`.

### Reference equality

The Elm Guide has this very interesting section:

<hr />

**Note**: When are two values “the same” though? To optimize for performance, we use JavaScript’s `===` operator behind the scenes:

- Structural equality is used for `Int`, `Float`, `String`, `Char`, and `Bool`.
- Reference equality is used for records, lists, custom types, dictionaries, etc.

Structural equality means that `4` is the same as `4` no matter how you produced those values. Reference equality means the actual pointer in memory has to be the same. Using reference equality is always cheap `O (1)`, even when the data structure has thousands or millions of entries. So this is mostly about making sure that using `lazy` will never slow your code down a bunch by accident. All the checks are super cheap!

<hr />

Let's try to reproduce this error case by changing the `viewPrime` function
to take a list of integers:

```elm
viewPrime : List Int -> Html Msg
viewPrime limits =
    let
        line limit =
            div []
                [ text
                    ("There are "
                        ++ String.fromInt (sieve limit |> List.length)
                        ++ " prime numbers between between 2 and "
                        ++ String.fromInt limit
                    )
                ]
    in
    div [] (List.map line limits)
```

When you click on the toggle [now](https://ellie-app.com/bc2Rn9t8HKxa1),
you'll see that we're back to being slow.

So what can we do when we need to use a record, a list, a dictionary or a
custom type as a **thing** to pass to `lazy`?

The easiest solution is to store it inside our `Model`:

```elm
type alias Model =
    { color: Color
    , limits: List Int
    }
```

Now the reference won't change as long as we don't change our model. Check
out the updated version [here](https://ellie-app.com/bc2RXrWn6fKa1).

## The End

Remember to always benchmark your application before and after `Html.Lazy`,
so you can prove to your team lead that you have actually made the
application faster 🤡

This article concludes the series on writing performant Elm, thanks for
reading!
