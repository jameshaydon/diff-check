# diffcheck

`diffcheck` is a tool to add reminders to check things if certain portions of
code get updated.

## Installation

Install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
and then:

```
stack install
```

## Usage

Changing one part of a codebase might often require corresponding changes to
other parts. To be reminded of this you add a `CHECK` to the part of the code
that triggers other required changes.

E.g. if you want changes to `f` to trigger a reminder:
``` haskell
f :: Int -> Int -> Int
f x y = x + y
```

You add a check like this:
``` haskell
-- CHECK: short description
-- Longer description follows,
--   - and can be,
--   - over several lines.
f :: Int -> Int -> Int
f x y = x + y
```

If a region that is protected by a `CHECK` is changed and `diffcheck` is
invoked, one gets output like this (but with more colours):

```
$ diffcheck

path/to/file.ext
CHECK: short description (A8MFn4Wp ➜ 00pxTFJG)
  Longer description here.
  Possibly over several lines.

The region of this check is affected by the following hunks:

  L66
  -        foo :: Foo,
  +        bar :: Bar,
```

To mark a check as done, use the interactive mode which displays each check in
turn and offers to add/update the stamp:

```
diffcheck -i
```

The check will now have a stamp, like this:

``` haskell
-- CHECK: short description
-- Longer description follows,
--   - and can be,
--   - over several lines.
-- STAMP: James Henri Haydon CHECKED short description (AMJA4QEe)
f :: Int -> Int -> Int
f x y = x + y
```

To perform the diff against a branch other than the default (`origin/master`),
use:

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
  what will remind you to add a new test? Some things are also not checkable by
  code, e.g. checking things with other teams, non-coders, etc.

## TODO

- Allow more flexible regions.
