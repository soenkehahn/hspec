{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}
module Test.Hspec.HUnit (
-- * Interoperability with HUnit
  fromHUnitTest
) where

import           Data.List (intercalate)
import qualified Test.HUnit as HU
import           Test.HUnit (Test (..))

import           Test.Hspec.Core.Type

-- | This instance is deprecated, use `Test.Hspec.HUnit.fromHUnitTest` instead!
instance Example r Test where
  evaluateExample test _ _ = do
    (counts, fails) <- HU.runTestText HU.putTextToShowS test
    let r = if HU.errors counts + HU.failures counts == 0
             then Success
             else Fail (details $ fails "")
    return r
    where
      details :: String -> String
      details = intercalate "\n" . tail . init . lines

-- |
-- Convert a HUnit test suite to a spec.  This can be used to run existing
-- HUnit tests with Hspec.
fromHUnitTest :: Test -> Spec
fromHUnitTest t = fromSpecList $ case t of
  TestList xs -> map go xs
  x           -> [go x]
  where
    go :: Test -> SpecTree ()
    go t_ = case t_ of
      TestLabel s (TestCase e)  -> it s e
      TestLabel s (TestList xs) -> describe s (map go xs)
      TestLabel s x             -> describe s [go x]
      TestList xs               -> describe "<unlabeled>" (map go xs)
      TestCase e                -> it  "<unlabeled>" e
