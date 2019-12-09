# diffcheck

`diffcheck` is a tool to add reminders to certain portions of code.

## Installation

```
stack install
```

## Usage

First add a check to some function in your code. E.g.:

``` haskell
-- | A function that does stuff.
--
-- CHECK: 'f' is still compatible with 'g'
-- Whenever 'f' (this function) changes it is important to update 'g' accordingly.
-- This is another description line.
-- STAMP: James Henri Haydon CHECKED 'f' is still compatible with 'g' (AMJA4QEe)
f :: Int -> Int -> Int
f x y = x + y
```

Then you can call `diffcheck`:

```
diffcheck
```

The tool will warn you if there are unchecked changes against `master`.

Notes:
- For the moment all config is baked in and the options are limited, but this
  will change hopefully.
- It only checks for changes against `master`.
- The region a _check_ applies to is just everything till the next empty line.

## TODO

- Allow specifying another branch from `master`.
- Allow more flexible regions.
- Allow configuring different markers from `CHECK:` and `STAMP:` if these are
  used in your code for some reason.
