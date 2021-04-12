---
title: Permutate parsers, don't validate
date: "2021-01-13T12:00:00.000Z"
description: A practical example of “Parse, don't validate” in Haskell.
---

["Parse, don't
validate"](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
has been one of my favourite programming articles for some time. The main
gist of the article is that, when writing in a type-driven fashion, your
snappy slogan should be:

> Parse, don't validate.

The core difference between parsing and validating can be explained by
looking at two very similar functions:

```haskell
parseInt :: String -> Maybe Int
parseInt str = Text.Read.readMaybe str

validateInt :: String -> Bool
validateInt str = Text.Read.readMaybe str /= Nothing
```

As you can see, they look very similar. The main difference is that
`parseInt` returns a useful value, the `Int` that we wanted to parse, while
`validateInt` takes that useful value and throws it away. This is also
mentioned in the wonderful [Haskell Mini-Patterns
Handbook](https://kowainik.github.io/posts/haskell-mini-patterns#evidence)
as the "Evidence" pattern.

> The key issue here is that by calling a function that returns Bool you
> lose the information about earlier performed validation. Instead, you can
> keep this information by explicitly pattern-matching on the validation or
> result.

In this post, I would like to go through a practical example that shows the
power of bringing this concept to its limits. Which brings us to...

## Advent of Code 2020, Day 4

We [are tasked](https://adventofcode.com/2020/day/4) with parsing a batch
of passports composed of these fields:

```none
- byr (Birth Year)
- iyr (Issue Year)
- eyr (Expiration Year)
- hgt (Height)
- hcl (Hair Color)
- ecl (Eye Color)
- pid (Passport ID)
- cid (Country ID)
```

All the fields are required except for the `cid` field, which is optional.
Note that the fields can be written in any order, this will be important
later. Our batch is composed of multiple passports separated by empty lines
(the `input.txt`):

```none
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
```

- The first passport is _valid_ - all required fields are present.
- The second passport is _invalid_ - it is missing `hgt`.
- The third passport is interesting: the only missing field is the optional `cid`, which makes it _valid_.
- The fourth passport is missing two fields, `cid` and `byr`. Missing `cid` is fine, but missing `byr` is not, so this passport is _invalid_.

The challenge is to count **how many passports** are **valid** in the given
batch.

Let's write some code to open the file and parse each group of passport
fields:

```haskell
module Main where

import qualified Data.List.Split as S

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let entries = map parseEntry (S.splitOn "\n\n" contents)
  print entries

data PassportEntry = PassportEntry
  deriving (Show)

parseEntry :: String -> PassportEntry
parseEntry text = undefined
```

Nothing too fancy here, we're using `Data.List.Split` from the `split`
package to do the heavy lifting. And the implementation of `parseEntry` has
been conveniently stubbed so that the code compiles.

Now how should our `PassportEntry` data structure look like? I'd love to
eventually represent passports as:

```haskell
data Passport = Passport
  { birthYear :: Int,
    issueYear :: Int,
    expirationYear :: Int,
    height :: String,
    hairColor :: String,
    eyeColor :: String,
    passportId :: String,
    countryId :: Maybe Int
  }
```

If we imagine parsing each field sequentially, we can see that we won't be
able to construct this data structure in a single operation. We'll have to
accumulate the data up until we're ready to create a proper `Passport`.

One way to store the fields is to insert them into a hash. First of all,
we're going to use a custom data type to represent the keys of the hash.
Why is that? We really don't want to be making typos later when comparing
raw strings like `"ecl"` and `"elc"`. We'll use a `HashMap` from the
`Data.HashMap.Strict` module:

```haskell
import qualified Data.HashMap.Strict as HM

data PassportField
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportId
  | CountryId
  deriving (Eq, Show)

type PassportEntry = HM.HashMap PassportField String
```

Of course things can't be that easy. We also need to make our type
implement the `Hashable` typeclass:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.HashMap.Strict as HM
import Data.Hashable
import GHC.Generics (Generic)

data PassportField
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportId
  | CountryId
  deriving (Eq, Show, Generic)

instance Hashable PassportField

type PassportEntry = HM.HashMap PassportField String
```

Don't worry about what we've added. Just take them as God-given truths. 👼

Okay, now we can implement our `parseEntry` function:

```haskell
import Data.Maybe (mapMaybe)
import qualified Data.Char as Char

parseEntry :: String -> PassportEntry
parseEntry line =
  HM.fromList $
    mapMaybe parseTag $
      S.splitWhen Char.isSpace line

parseTag :: String -> Maybe (PassportField, String)
parseTag value =
  case S.splitOn ":" value of
    ["byr", byr] ->
      Just (BirthYear, byr)
    ["iyr", iyr] ->
      Just (IssueYear, iyr)
    ["eyr", eyr] ->
      Just (ExpirationYear, eyr)
    ["hgt", height] ->
      Just (Height, height)
    ["hcl", color] ->
      Just (HairColor, color)
    ["ecl", color] ->
      Just (EyeColor, color)
    ["pid", pid] ->
      Just (PassportId, pid)
    ["cid", cid] ->
      Just (CountryId, cid)
    _ ->
      Nothing
```

We try to parse each field (such as `byr:2002`) into a `PassportField`
type, then end up building a hash using `HM.fromList`. We can take this for
a spin:

```haskell
Prelude> :l Main.hs

*Main> main
[ fromList
    [ (CountryId, "147"),
      (BirthYear, "1937"),
      (IssueYear, "2017"),
      (HairColor, "#fffffd"),
      (ExpirationYear, "2020"),
      (EyeColor, "gry"),
      (Height, "183cm"),
      (PassportId, "860033327")
    ],
  fromList
    [ (CountryId, "350"),
      (BirthYear, "1929"),
      (IssueYear, "2013"),
      (HairColor, "#cfa07d"),
      (ExpirationYear, "2023"),
      (EyeColor, "amb"),
      (PassportId, "028048884")
    ],
  fromList
    [ (BirthYear, "1931"),
      (IssueYear, "2013"),
      (HairColor, "#ae17e1"),
      (ExpirationYear, "2024"),
      (EyeColor, "brn"),
      (Height, "179cm"),
      (PassportId, "760753108")
    ],
  fromList
    [ (IssueYear, "2011"),
      (HairColor, "#cfa07d"),
      (ExpirationYear, "2025"),
      (EyeColor, "brn"),
      (Height, "59in"),
      (PassportId, "166559648")
    ]
]
```

Nice and tidy! 🦾

Now our goal is to verify which one of these groups is valid. First of all,
we should define a list of required fields:

```haskell
requiredFields :: [PassportField]
requiredFields =
  [ BirthYear,
    IssueYear,
    ExpirationYear,
    Height,
    HairColor,
    EyeColor,
    PassportId
  ]
```

We can then define a validation function:

```haskell
isEntryValid :: PassportEntry -> Bool
isEntryValid entry =
  all (`HM.member` entry) requiredFields
```

And change our main function to use that:

```haskell
main :: IO ()
main = do
  contents <- readFile "input.txt"
  let entries = map parseEntry (S.splitOn "\n\n" contents)
  print $ length $ filter isEntryValid entries
```

Running this yields `2`, which is the correct answer!
[Here](https://gist.github.com/Arkham/b749043e1622bf01c96eefe303df0b9b) is
all the code we have written so far, if you're feeling like you need a
refresher.

## Advent of Code 2020, Day 4, Part II

In the second part of the challenge, these new rules are added:

```none
- byr (Birth Year) - four digits; between 1920 and 2002.
- iyr (Issue Year) - four digits; between 2010 and 2020.
- eyr (Expiration Year) - four digits; between 2020 and 2030.
- hgt (Height) - a number followed by either cm or in:
  - If cm, the number must be between 150 and 193.
  - If in, the number must be between 59 and 76.
- hcl (Hair Color) - a '#' followed by six chars 0-9 or a-f.
- ecl (Eye Color) - one of: amb blu brn gry grn hzl oth.
- pid (Passport ID) - a nine-digit number.
- cid (Country ID) - ignored, missing or not.
```

Here is a new batch for us to peruse:

```none
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
```

- the first passport is _valid_
- the second passport is _invalid_ (look at the `eyr` field)

These new requirements are a bit annoying. Our simple approach of checking
if all required fields are present won't work any longer. We can instead
implement a `isFieldValid` function to check if all fields are valid.

```haskell
isFieldValid :: (PassportField, String) -> Bool
isFieldValid (field, value) =
  case field of
    BirthYear ->
      let v = toInt value
       in length value == 4 && v >= 1920 && v <= 2002
    IssueYear ->
      let v = toInt value
       in length value == 4 && v >= 2010 && v <= 2020
    ExpirationYear ->
      let v = toInt value
       in length value == 4 && v >= 2020 && v <= 2030
    Height ->
      case span Char.isDigit value of
        (num, "cm") ->
          let n = toInt num
           in n >= 150 && n <= 193
        (num, "in") ->
          let n = toInt num
           in n >= 59 && n <= 76
        _ ->
          False
    HairColor ->
      case (length value, value) of
        (7, '#' : rest) ->
          all (`elem` allowedHexChars) rest
        _ ->
          False
    EyeColor ->
      value `elem` validEyeColors
    PassportId ->
      length value == 9 && all Char.isDigit value
    CountryId ->
      all Char.isDigit value

toInt :: String -> Int
toInt = read

validEyeColors :: [String]
validEyeColors =
  ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

allowedHexChars :: [Char]
allowedHexChars =
  ['0' .. '9'] <> ['a' .. 'f']
```

Then we can change our `isEntryValid` to use this function:

```haskell
isEntryValid :: PassportEntry -> Bool
isEntryValid entry =
  requiredFieldsPresent && allFieldsValid
  where
    requiredFieldsPresent =
      all (`HM.member` entry) requiredFields

    allFieldsValid =
      all isFieldValid (HM.toList entry)
```

Running this program on our second data sample yields `1`, and it will be
good enough to solve the Advent of Code challenge and get us those sweet
sweet stars.

🎉 🎉 🎉

## A moment of reflection

If we look back at the [current
state](https://gist.github.com/Arkham/923df6e872d855101bca4261f7d71e65) of
our code, we can see that we are doing a lot of validations.

We do a lot of work to verify if something is valid, then throw it all out
of the window to return a meagre `Bool`. German folks from the [sixteenth
century](https://en.wikipedia.org/wiki/Don%27t_throw_the_baby_out_with_the_bathwater)
would have told us snarkily:

> das Kind mit dem Bade ausschütten

Yes, literally _throwing the baby out with the bathwater_.

This would be even more true if we were given a new challenge:

> Now find the unique set of eye colors in all valid passports

With our current code, we know which passport is valid, but we have no way
of extracting the eye color of a valid passport. This is why earlier we
were mentioning this sort of `Passport` representation:

```haskell
data Passport = Passport
  { birthYear :: Int,
    issueYear :: Int,
    expirationYear :: Int,
    height :: String,
    hairColor :: String,
    eyeColor :: String,
    passportId :: String,
    countryId :: Maybe Int
  }
```

If we had a function like `parsePassport` that went from `String` to `Maybe
Passport` we could then write some code like this:

```haskell
Set.fromList $
  map eyeColor $
  mapMaybe parsePassport (S.splitOn "\n\n" contents)
```

But let's not get too ahead of ourselves. Let's try to refactor our current
code to do something similar. First we can try to write a function like
this one:

```haskell
entryToPassport :: PassportEntry -> Maybe Passport
```

This function takes the intermediate representation of a collection of
passport fields and returns a 'validated' passport. We can also reuse our
`isFieldValid` function by using this trick:

```haskell
parseField ::
  (PassportField, String) -> Maybe (PassportField, String)
parseField tuple =
  if isFieldValid tuple
    then Just tuple
    else Nothing
```

We are still reusing the validating logic, but we end up returning
something useful instead. Remember, we are slowly migrating our code from
validating data to parsing data.

Using our new helper we can finally implement the `entryToPassport`
function. We'll do that in two separate steps. First we'll get all the
values of the required fields:

```haskell
getAllRequiredFields :: PassportEntry -> Maybe [String]
getAllRequiredFields e =
  traverse
    ( \field -> do
        v <- HM.lookup field e
        (_field, text) <- parseField (field, v)
        return text
    )
    requiredFields
```

The `traverse` magic ensures that we either get all the values we're
looking for wrapped in a `Just`, or `Nothing` if any of those fields were
invalid. Ok, we're ready to roll now!

```haskell
entryToPassport :: PassportEntry -> Maybe Passport
entryToPassport entry = do
  case getAllRequiredFields entry of
    Just [byr, iyr, eyr, hgt, hcl, ecl, pid] ->
      Just $
        Passport
          { birthYear = toInt byr,
            issueYear = toInt iyr,
            expirationYear = toInt eyr,
            height = hgt,
            hairColor = hcl,
            eyeColor = ecl,
            passportId = pid,
            countryId = toInt <$> HM.lookup CountryId entry
          }
    _ ->
      Nothing
```

We end up having to pass `String` values around, which need to be parsed
again into the exact types that we desire. Also we need to pass these
values into a list and hope not to mess up the ordering of the fields. So
it's far from perfect, but we're getting somewhere.

In order to use this function in our main, we replace the last line of the
main fuction from:

```haskell
print $ length $ filter isEntryValid entries
```

to:

```haskell
print $ length $ mapMaybe entryToPassport entries
```

Running
[this](https://gist.github.com/Arkham/22cb983c61f5a6aaf54227c94d0c7a6b) on
our test batch still returns `1`, which is a good sign we haven't broken
anything.

Still, there is one thing that I'm particularly unhappy about in this code:
we use an intermediate representation of the passport that has no real
domain value. Nobody cares about `PassportField` and `PassportEntry`, but
we need to have these types in order to build our `Passport`.

Not only that, but having these intermediate types means that there are
bugs waiting to happen when we transform them to our desired data type:

- getting the order of the fields wrong
- parsing strings in inconsistent ways
- forgetting to validate the presence of required fields

This is also known as _shotgun parsing_:

> Shotgun parsing is a programming antipattern whereby parsing and input-validating code is mixed with and spread across processing code—throwing a cloud of checks at the input, and hoping, without any systematic justification, that one or another would catch all the “bad” cases.

In "Parse, don't validate", Alexis King goes on to describe how this is
specifically related to parsing and validating:

> It may not be immediately apparent what shotgun parsing has to do with validation—after all, if you do all your validation up front, you mitigate the risk of shotgun parsing. The problem is that validation-based approaches make it extremely difficult or impossible to determine if everything was actually validated up front or if some of those so-called “impossible” cases might actually happen. The entire program must assume that raising an exception anywhere is not only possible, it’s regularly necessary.

How can we avoid doing this? Let's try something new!

## A new perspective

We're going to write the same program using
[parsec](https://hackage.haskell.org/package/parsec), a monadic parser
combinator library. I've recently bumped
into this excellent [walkthrough to parser
combinators](https://hasura.io/blog/parser-combinators-walkthrough/), which
I thouroughly recommend reading.

In short a parser is a type like this one:

```haskell
type Parser a = Parser {
  runParser :: String -> (String, Either ParseError a)
}
```

It works by consuming input characters from the input string and returning
a tuple with two values:

1. The first value is what's left of the input string, so that other
   parsers can keep parsing the rest of the input.
2. The second value contains either a parse error or a properly parsed value of type `a`.


We'll need to install `parsec` and add some imports:

```haskell
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Control.Monad (guard)
```

Now let's implement parsers for some fields:

```haskell
-- byr (Birth Year) - four digits; between 1920 and 2002.
byrParser :: Parser Int
byrParser = do
  P.string "byr"
  P.char ':'
  value <- P.count 4 P.digit
  P.spaces
  let int = read value
  guard (int >= 1920 && int <= 2002)
  return int

-- iyr (Issue Year) - four digits; between 2010 and 2020.
iyrParser :: Parser Int
iyrParser = do
  P.string "iyr"
  P.char ':'
  value <- P.count 4 P.digit
  P.spaces
  let int = read value
  guard (int >= 2010 && int <= 2020)
  return int

-- eyr (Expiration Year) - four digits; between 2020 and 2030.
eyrParser :: Parser Int
eyrParser = do
  P.string "eyr"
  P.char ':'
  value <- P.count 4 P.digit
  P.spaces
  let int = read value
  guard (int >= 2020 && int <= 2030)
  return int
```

Here we use the `guard` function to introduce an assertion that will make
the parser fail when the condition is not met. In general I feel that this
code is quite readable, but we might want to extract a reusable helper to
parse years:

```haskell
yearParser :: String -> (Int, Int) -> Parser Int
yearParser value (rangeStart, rangeEnd) = do
  P.string value
  P.char ':'
  value <- P.count 4 P.digit
  P.spaces
  let int = read value
  guard (int >= rangeStart && int <= rangeEnd)
  return int

byrParser :: Parser Int
byrParser = do
  yearParser "byr" (1920, 2002)

iyrParser :: Parser Int
iyrParser = do
  yearParser "iyr" (2010, 2020)

eyrParser :: Parser Int
eyrParser = do
  yearParser "eyr" (2020, 2030)
```

🎉

This is the beauty of writing parser combinators. They are **extremely** easy to
reuse and combine.

Now we want to write a parser for the height field. It might be nice to use
a more specialized data type to represent that:

```haskell
data Height
  = InCms Int
  | InInches Int
  deriving (Eq, Show)
```

Here we go!

```haskell
-- hgt (Height) - a number followed by either cm or in:
-- If cm, the number must be between 150 and 193.
-- If in, the number must be between 59 and 76.
heightParser :: Parser Height
heightParser = do
  P.string "hgt"
  P.char ':'
  digits <- P.many1 P.digit
  let value = read digits
  result <- unitParser value
  case result of
    InCms _ ->
      guard (value >= 150 && value <= 193)
    InInches _ ->
      guard (value >= 59 && value <= 76)
  P.spaces
  return result
```

What's that `unitParser` you ask?

```haskell
unitParser :: Int -> Parser Height
unitParser value =
  let cmParser = do
        P.string "cm"
        return (InCms value)

      inParser = do
        P.string "in"
        return (InInches value)
   in P.choice [cmParser, inParser]
```

The rest of the parsers are basically a step by step translation of the
requirements in English:

```haskell
-- hcl (Hair Color) - a '#' followed by six chars 0-9 or a-f.
hairColorParser :: Parser String
hairColorParser = do
  P.string "hcl"
  P.char ':'
  P.char '#'
  v <- P.count 6 (P.oneOf "0123456789abcdef")
  P.spaces
  return v
```

```haskell
-- pid (Passport ID) - a nine-digit number.
passportIdParser :: Parser String
passportIdParser = do
  P.string "pid"
  P.char ':'
  v <- P.count 9 P.digit
  P.spaces
  return v
```

```haskell
-- cid (Country ID) - ignored, missing or not.
countryIdParser :: Parser Int
countryIdParser = do
  P.string "cid"
  P.char ':'
  value <- P.many1 P.digit
  P.spaces
  return $ read value
```

```haskell
-- ecl (Eye Color) - one of: amb blu brn gry grn hzl oth.
eyeColorParser :: Parser String
eyeColorParser = do
  P.string "ecl"
  P.char ':'
  v <-
    P.choice $
      map
        (P.try . P.string)
        [ "amb",
          "blu",
          "brn",
          "gry",
          "grn",
          "hzl",
          "oth"
        ]
  P.spaces
  return v
```

You will notice we had to use this mysterious `P.try` function in the last
snippet. This is very useful when we need to look ahead in the input
string. Consider the example of `blu` and `brn`: after consuming an initial
`b` character we land in the `blu` branch. If at that point we encounter a
`r` character, we realize we need to go back and choose the `brn` branch
instead. But by default the parsing would stop because we have already
consumed the first character. `P.try` will make it so our parser pretends
it hasn't consumed any input so that we can keep trying other
alternatives.

We have now written parsers for each individual field. So now it's time to
combine them all together...

## Another problem?!

Remember that the fields can be written in any order? Uh oh.

Since our parser tries to consume input one character at a time, how the
heck can we write one that has to deal with randomly ordered input?

It is impossible, right?

No, it's **possible**!

The `parsec` library includes a wonderful `Text.Parsec.Perm` module:

> This module implements permutation parsers. A permutation phrase is a
> sequence of elements (possibly of different types) in which each element
> occurs exactly once and the order is irrelevant. Some of the permutable
> elements may be optional.

Let's import it:

```haskell
import Text.Parsec.Perm (permute, (<$$>), (<|?>), (<||>))
```

Woah, calm down, that's lots of operators there mate.

- `permute` is the last call that will wrap everything up and return a
    parser of something.
- `<$$>` is used to assign all the fields that we parsed to something. In
    our case it will be a `Passport`.
- `<||>` is used to describe a required field
- `<|?>` is used to describe an optional field

Ready for the big reveal? 🥁🥁🥁

```haskell
passportParser :: Parser Passport
passportParser =
  permute $
    Passport <$$> byrParser
      <||> iyrParser
      <||> P.try eyrParser
      <||> P.try heightParser
      <||> P.try hairColorParser
      <||> P.try eyeColorParser
      <||> passportIdParser
      <|?> (Nothing, Just <$> countryIdParser)
```

This parser can parse passports which fields are written in any
random order, as long as each required field is present once. Pretty slick,
eh?

We just need to wire it up in our main:

```haskell
import Data.Either (rights)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let passports =
        rights $
          map
            (P.parse passportParser "")
            (S.splitOn "\n\n" contents)
  print $ length passports
```

That's all we need! We can now get rid of the intermediate `PassportField`
and `PassportEntry` types and all that validation code. Welcome to the
wonderful world of parsing!

If you glance over the [final
listing](https://gist.github.com/Arkham/f3b76516b8a9c07cf2b0038871c60657)
you'd be surprised to see how tidy it is. We have a description of the
input that we'd like to parse and nothing more. No validations, no
transformations, no other massaging of the types. That's the beauty of
parsing and expressing real world problems in terms of parsing.

I hope you enjoyed this deep dive into parsing, thanks for reading!
