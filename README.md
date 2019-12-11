# diffcheck

`diffcheck` is a tool to add reminders to check things if certain portions of
code get updated.

## Installation

Install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) and then:

```
stack install
```

## Usage

First add a CHECK to some function in your code. E.g.:

``` haskell
-- CHECK: short description
-- Longer description follows,
--   - and can be,
--   - over several lines.
-- STAMP: James Henri Haydon CHECKED short description (AMJA4QEe)
f :: Int -> Int -> Int
f x y = x + y
```

If a region that is protected by a `CHECK` is changed and `diffcheck` is
invoked, one gets output like this (but with more colours):

```
$ diffcheck

path/to/file.ext
CHECK: short description

  Longer description here.
  Possibly over several lines.

The region of this check is affected by the following hunks:

  L66
  -        foo :: Foo,
  +        bar :: Bar,

To mark this as checked, use the stamp:
STAMP: James Henri Haydon CHECKED short description (MovKghFl)
```

To mark the check as done the stamp must be updated. This can be done by using
the interactive mode:

```
diffcheck -i
```

To perform the check against a branch other than master, use:

```
diffcheck --diff-against origin/develop
```

If `CHECK:` and `STAMP:` are used for other reasons in your source-code, you can
use different markers:

```
diffcheck --check-marker CAREFUL: --stamp-marker CHECKPOINT:
```

Currently the region a CHECK applies to is till the next empty line and is
non-configurable.

## FAQ

- _Shouldn't these sorts of things be enforced by the type system?_ Absolutely,
  if the type system of your language is expressive enough to enfoce the
  property you want to check, you should use that. However, I have found that
  even with Haskell, a language with a type-system more expressive that most,
  some invariants are either not expressible or they involve using extension X
  of the type system which doesn't play well with some other advanced
  type-hackery you are using.
- _Shouldn't these sorts of things be checked with tests?_ Again if you can
  enfoce something with a test that is the way to go. However sometimes you can
  forget to update the tests! For example you might want to test that each field
  of an API type is independently patchable. If you add a field to the type,
  what will remind you to add a new test?

## TODO

- Allow more flexible regions.
