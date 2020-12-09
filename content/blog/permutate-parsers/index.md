---
title: Permutate parsers, don't validate
date: "2020-12-10T12:00:00.000Z"
description: A practical example of “Parse, don't validate” in Haskell.
---

["Parse, don't
validate"](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
has been one of my favourite programming articles for some time. The main
gist is that, when writing in a type-driven fashion, your snappy slogan should be:

> Parse, don't validate.

The core difference between parsing and validating can be explained by
looking at two very similar functions:

```haskell
parseInt :: String -> Maybe Int
parseInt str = Text.Read.readMaybe str

validateInt :: String -> Bool
validateInt str = parseInt str /= Nothing
```

As you can see, they look very similar. The main difference is that
`parseInt` returns a useful value, the `Int` that we wanted to parse, while
`validateInt` takes that useful value and throws it away. This is also
mentioned in the wonderful [Haskell Mini-Patterns
Handbook](https://kowainik.github.io/posts/haskell-mini-patterns#evidence)
as the "Evidence" pattern.

> The key issue here is that by calling a function that returns Bool you lose the information about earlier performed validation. Instead, you can keep this information by explicitly pattern-matching on the validation or result.

In this post, I would like to go through a practical example that shows the power
of bringing this concept to its limits. Which brings us to...

## Advent of Code 2020, Day 4

We [are tasked](https://adventofcode.com/2020/day/4) with parsing a batch of passports composed of these fields:

```plain
- byr (Birth Year)
- iyr (Issue Year)
- eyr (Expiration Year)
- hgt (Height)
- hcl (Hair Color)
- ecl (Eye Color)
- pid (Passport ID)
- cid (Country ID)
```

All the fields are required except for the `cid` field, which is optional. Our batch is composed of multiple passports separated by empty lines:

```plain
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
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let entries = map parseEntry (T.splitOn "\n\n" contents)
  print entries

data PassportEntry = PassportEntry
  deriving (Show)

parseEntry :: T.Text -> PassportEntry
parseEntry text = undefined
```

Here I've decided to use `Data.Text` to read the contents of the file and
stubbed the implementation of `parseEntry` just to get the file to compile.
I'm also using the `OverloadedStrings` language extension to be able to
write `"hello"` and have Haskell infer that it's a `Text`.

Now how should our `PassportEntry` data structure look like? I'd love to
eventually represent passports as something like:

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

The problem is that the fields are ordered arbitrarily, so we can't really
know in which order they will come. So we might want to store fields in
another data structure, then later convert them to our beautiful `Passport`
representation.

One way to store the fields is to insert them into a hash. First of all, I
want to use a data type to represent the keys of the hash. Why is that? I
really don't want to be making typos later when comparing `"ecl"` with `"elc"`.
I'll use a `HashMap` from the `Data.HashMap.Strict` module:

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

type PassportEntry = HM.HashMap PassportField T.Text
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

type PassportEntry = HM.HashMap PassportField T.Text
```

Don't worry about any of that means. Just take it as a God-given truth.

Okay, now we can implement our `parseEntry` function:

```haskell
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)

parseEntry :: T.Text -> PassportEntry
parseEntry line =
  HM.fromList $
    mapMaybe parseField $
      T.split isSpace line

parseField :: T.Text -> Maybe (PassportField, T.Text)
parseField value =
  case T.splitOn ":" value of
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

We try to parse each `byr:2002` field into our `PassportField` type
and end up building a hash using `HM.fromList`. We can take this for a
spin in `ghci`:

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

Then we can define a validation function:

```haskell
isEntryValid :: PassportEntry -> Bool
isEntryValid entry =
  all
    (\field -> HM.member field entry)
    requiredFields
```

And change our main function to use that:

```haskell
main :: IO ()
main = do
  contents <- TI.readFile "short-input.txt"
  let entries = map parseEntry (T.splitOn "\n\n" contents)
  print $ length $ filter isEntryValid entries
```

Running this yields `2`, which is the correct answer!
[Here](https://gist.github.com/Arkham/4501df31f2a4f5eedef1c50fe01daeda) is all the code we have written so far, if you're feeling like you need a refresher.

## Advent of Code 2020, Day 4, Part II

In the second part of the challenge, these new rules are added:

```plain
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

Here is a valid passport, followed by an invalid passport (look at `eyr`):

```plain
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
```

That's annoying. Our simple approach of checking if all required fields are
present won't work any longer. I guess we can implement a `isFieldValid`
function to check if all fields are valid.

```haskell
isFieldValid :: (PassportField, T.Text) -> Bool
isFieldValid (field, value) =
  case field of
    BirthYear ->
      let v = toInt value
       in T.length value == 4 && v >= 1920 && v <= 2002

    IssueYear ->
      let v = toInt value
       in T.length value == 4 && v >= 2010 && v <= 2020

    ExpirationYear ->
      let v = toInt value
       in T.length value == 4 && v >= 2020 && v <= 2030

    Height ->
      case T.span isDigit value of
        (num, "cm") ->
          let n = toInt num
           in n >= 150 && n <= 193
        (num, "in") ->
          let n = toInt num
           in n >= 59 && n <= 76
        _ ->
          False

    HairColor ->
      case (T.length value, T.unpack value) of
        (7, '#' : rest) ->
          all (`elem` allowedHexChars) rest
        _ ->
          False

    EyeColor ->
      value
        `elem` ["amb","blu","brn","gry","grn","hzl","oth"]

    PassportId ->
      T.length value == 9 && T.all isDigit value

    CountryId ->
      T.all isDigit value
  where
    toInt :: T.Text -> Int
    toInt = read . T.unpack

    allowedHexChars :: [Char]
    allowedHexChars = ['0' .. '9'] <> ['a' .. 'f']
```

We can now change our `isEntryValid` to use this function:

```haskell
isEntryValid :: PassportEntry -> Bool
isEntryValid entry =
  requiredFieldsPresent && allFieldsValid
  where
    requiredFieldsPresent =
      all
        (\field -> HM.member field entry)
        requiredFields

    allFieldsValid =
      all
        isFieldValid
        (HM.toList entry)
```

Running this program yields `1`, and it will be good enough to solve the
Advent of Code challenge and get you those sweet sweet stars.

🎉 🎉 🎉

## A moment of reflection

If we look back at the [current
state](https://gist.github.com/Arkham/ba80dc39f5e1b7fe700287ee7bed1f2d) of our code, we can see that we are validating, validating, and validating.

We do a lot of work to verify if something is valid, then throw it all out
of the window to return a meagre `Bool`. German folks from the [sixteenth
century](https://en.wikipedia.org/wiki/Don%27t_throw_the_baby_out_with_the_bathwater) would have told us:

> das Kind mit dem Bade ausschütten

Yes, we're literally _throwing the baby out with the bathwater_.

We realize this is even more true if we tried to take the data and fill
that ideal `Passport` representation we were mentioning earlier:

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

We'd like to write a `entryToPassport` function with this shape:

```haskell
PassportEntry -> Maybe Passport
```
