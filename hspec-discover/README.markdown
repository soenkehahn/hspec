# Automatically discover and run Hspec tests

> What follows is a short summary of the underlying problem.  If you are only
> interested in the solution, jump right into [getting started]
> (https://github.com/hspec/hspec/tree/master/hspec-discover#getting-started).

It is a common convention to have one spec file for each source file.  To run
all specs for a given project together, it is necessary to combine all the
specs from all spec files into a single spec.  For example, if we have three
modules, `Foo`, `Foo.Bar` and `Baz`, and corresponding specs in `FooSpec`,
`Foo.BarSpec` and `BazSpec`, we are required to write the following
boilerplate:

```haskell
module Main where

import Test.Hspec

import qualified FooSpec
import qualified Foo.BarSpec
import qualified BazSpec

main :: IO ()
main = hspec $ do
  describe "Foo"     FooSpec.spec
  describe "Foo.Bar" Foo.BarSpec.spec
  describe "Baz"     BazSpec.spec
```

This is error prone, and neither challenging nor interesting.  So it should be
automated.  `hspec-discover` comes to the rescue.

## Getting started

> For the purpose of this example, all source files are put into the `src/`
> directory, and specs are put into the `test/` directory.  The main module of
> the test suite is defined in `test/Spec.hs`.
>
> This are useful conventions, but you can
> use anything you like.

Create some modules.

```
src/Foo.hs
src/Foo/Bar.hs
src/Baz.hs
```

And write some specs.

```
test/FooSpec.hs
test/Foo/BarSpec.hs
test/BazSpec.hs
```
Create a main module for your test suite, containing one single line.

```haskell
-- file test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

Run it (e.g. with `ghci`).

```
$ echo ':set -isrc -itest' > .ghci
$ ghci test/Spec.hs
*Main> main
```

Or turn it into a [Cabal test suite]
(http://www.haskell.org/cabal/users-guide/developing-packages.html#test-suites).

```
test-suite spec
  type:           exitcode-stdio-1.0
  main-is:        Spec.hs
  hs-source-dirs: src, test
  build-depends:  base, hspec
```

## How it works

`-F -pgmF hspec-discover` tells GHC to preprocess a module with
`hspec-discover`.  `hspec-discover` will then generate a suitable main module
for your test suite.

For the above example something like this is generated.

```haskell
module Main where

import Test.Hspec

import qualified FooSpec
import qualified Foo.BarSpec
import qualified BazSpec

main :: IO ()
main = hspec $ do
  describe "Foo"     FooSpec.spec
  describe "Foo.Bar" Foo.BarSpec.spec
  describe "Baz"     BazSpec.spec
```

`hspec-discover` searches for specs in the same directory where the file with
the preprocessor directive is located, and in all subdirectories.  All files
with a name that ends in `Spec.hs` are include in the generated test suite.
And it is assumed, that they export a `spec` of type `Spec`.

## Customizing things

If you rather want to get a nested spec for hierarchical modules, pass
`--nested` to `hspec-discover`.

```haskell
-- file Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --nested #-}
```

For the above example this will result in:

```haskell
module Main where

import Test.Hspec

import qualified FooSpec
import qualified Foo.BarSpec
import qualified BazSpec

main :: IO ()
main = hspec $ do
  describe "Foo" $ do
    FooSpec.spec
    describe "Bar"
      Foo.BarSpec.spec
  describe "Baz"
    BazSpec.spec
```

Other aspects of `hspec-discover` are not yet configurable.  If you need
anything else, write me an email, [open an issue on GitHub]
(https://github.com/hspec/hspec/issues), or discuss your ideas at
`#hspec` on freenode.

## Limitations

At the moment only monadic specs are supported.  It would be possible to
support non-monadic specs, or even your custom specs through type classes.
Again, if you need any of those, write me an email, [open an issue on GitHub]
(https://github.com/hspec/hspec/issues), or discuss your ideas at `#hspec` on
freenode.
