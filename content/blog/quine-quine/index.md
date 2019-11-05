---
title: Writing a program that writes itself
date: "2019-11-04T12:00:00.000Z"
description: What is a Quine and how to write one.
---

Recursion is one of those concepts in programming which have always been
incredibly fascinating to me. _What do you mean that I can call the
function from the function itself?_

Well, there is a class of programs that bring the whole idea of recursion
to a completely different level, which is programs that are able to
replicate themselves. These programs, named **Quines**, produce a copy of
their own source code as their only output.

![Drawing Hands, M.C.Escher](./escher.jpg)

The name "quine" was introduced by Douglas Hofstadter in _Gödel, Escher,
Bach: An Eternal Golden Braid_ in honor of [Willard Van Orman
Quine](https://en.wikipedia.org/wiki/Willard_Van_Orman_Quine), an American
analytic philosopher who made extensive studies in the field of
self-reference. There is a paradox named after him that goes:

> "Yields falsehood when preceded by its quotation" yields falsehood when preceded by its quotation.

This is a more elaborate version of the [Liar
paradox](https://en.wikipedia.org/wiki/Liar_paradox), where the sentence
`Yields falsehood when preceded by its quotation`, when preceded by itself
in quotation marks, gives a new expression. If this new expression is true,
then it says of itself that it must be false. But if it's false, then what
it says of itself must be true.

Pretty puzzling, huh?

## Some rules

Before we start and try to write a Quine, we have to define some ground
rules:

- The program must produce its own source code when executed
- It must not use I/O to read its own source code
- It must not be an empty program

## A first attempt

If we had this simple Ruby program:

```ruby
puts "hello"
```

And we executed it, we would get:

```bash
$ ruby quine.rb
hello
```

So we could make our program to look like this:

```ruby
puts "puts \"hello\""
```

But running that would only yield:

```bash
$ ruby quine.rb
puts "hello"
```

As we can see, this approach won't get us closer to the solution since
we're always going to miss the outer `puts`.

## A bit of self reflection

Luckily in Ruby there is a simple way to escape this using
`Object#inspect`:

```ruby
src = "puts src"
puts "src = " + src.inspect
```

```bash
$ ruby quine.rb
src = "puts src"
```

But we're still missing the last line. What if we added the `src` again
to the end of our `puts` statement, but this time as a normal string?

```ruby
src = "puts src"
puts "src = " + src.inspect + src
```

```bash
$ ruby quine.rb
src = "puts src"puts src
```

Uh! Since the contents of the `src` string don't really matter to the
behaviour of our program, we can get quite creative with it!

```ruby
src = "\nputs \"src = \" + src.inspect + src"
puts "src = " + src.inspect + src
```

```bash
$ ruby quine.rb
src = "\nputs \"src = \" + src.inspect + src"
puts "src = " + src.inspect + src
```

Tada! 🎉

## How to write your own Quine

This simple example showed us some of the core ideas for writing a Quine:

- We build the program in two parts, one that we call the _code_ and one
  that we call the _data_. The data part of the program is derived from the
  code part, usually by some means of escaping.
- When executed, the code first uses the data to print the data.
- Then the code uses the data to print the code. This is generally easy
  because we intentionally derived the data from the code.

A great analogy of a Quine comes from cellular biology: if we think of the
cell as the **code** and the cell's DNA as the **data**, then we'll see that
the cell is able to replicate itself using the DNA, and this process
involves replicating the DNA itself. On the other hand the DNA contains
all the information for replicating the cell, but without the cell itself
it would just be an inert piece of data.

## Quine Quine

Douglas Hofstadter also invented the verb _to quine_, which means "to write
something a first time, and then to write it a second time with quotation
marks around it". For example, if we quine "say", we would get "say 'say'".
If we were to quine "quine", we would get "quine 'quine'", which itself is
a quine...

## Testing, testing, testing

When you're trying to write your own Quine, you'll get to a point when you
think you've reached a solution, but end up having to compare two printouts
which are not exactly human friendly. So you can use this bit of Bash
instead:

```bash
$ diff <(cat quine.rb) <(ruby quine.rb) && echo "IS QUINE"
IS QUINE
```

If you had make a mistake, it would print the diff like this:

```bash
$ diff <(cat wrong.rb) <(ruby wrong.rb) && echo "IS QUINE"
2c2
< puts "src = " + src.inspect + src
---
> puts src = " + src.inspect + src
```

The weird looking `<()` is a Bash feature called [process
substitution](http://tldp.org/LDP/abs/html/process-sub.html) that allows
you to pass one command's output to another program as if it were a file
name.

## A Quine in Haskell

Here is a very elegant solution in Haskell:

```haskell
main = putStr src >> print src
  where src = "main = putStr src >> print src\n  where src = "
```

This works thanks to the `where` notation, where you can use a variable in
an expression before defining it. We first print the code using the normal
`putStr` function, then we print the data using `print`, which maintains
the double quotes around the string. The beauty of this solution is that
the data is barely escaped!

Here it is in action:

```bash
$ ghc quine.hs
[1 of 1] Compiling Main             ( quine.hs, quine.o )
Linking quine ...

$ diff <(cat quine.hs) <(./quine) && echo "IS QUINE"
IS QUINE
```

## A Quine in Elm?

We can almost directly port our Ruby code to Elm. The only challenge is
that browsers don't usually recognize newlines literally, so that makes
escaping code quite daunting. But fear not, thanks to the amazing powers of
CSS and the
[white-space](https://developer.mozilla.org/en-US/docs/Web/CSS/white-space)
property, we can make the browser behave:

```css
span {
  white-space: pre;
  font-family: monospace;
}
```

The other challenge is to be able to inspect the data in a literal way:
this can be achieved in Elm using the `Debug#toString` function.

So I challenge you to write your own implementation of a Quine in Elm. If
you get stuck at any point, you can check this [working example on
Ellie](https://ellie-app.com/76YftNWMdDTa1).

Happy Quining!

### References

- [David Madore's Quines
  page](http://www.madore.org/~david/computers/quine.html)
- [Florian Harmann's Quines blog post](https://florian.github.io/quines)
