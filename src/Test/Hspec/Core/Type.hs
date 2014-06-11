{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables, MultiParamTypeClasses #-}
module Test.Hspec.Core.Type (
  Spec
, SpecM (..)
, runSpecM
, fromSpecList
, SpecTree (..)
, Item (..)
, mapSpecItem
, Example (..)
, Result (..)
, Params (..)
, Progress

, describe
, it
, forceResult

, pending
, pendingWith
) where

import qualified Control.Exception as E
import           Control.Applicative
import           Control.Monad (when)
import           Control.Monad.Trans.Writer (Writer, execWriter, tell)
import           Data.Typeable (Typeable)
import           Data.List (isPrefixOf)
import           Data.Maybe (fromMaybe)

import           Test.Hspec.Compat
import           Test.Hspec.Util
import           Test.Hspec.Expectations
import           Test.HUnit.Lang (HUnitFailure(..))
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.State as QC
import qualified Test.QuickCheck.Property as QCP
import qualified Test.QuickCheck.IO ()

import           Test.Hspec.Core.QuickCheckUtil
import           Control.DeepSeq (deepseq)

type Spec = SpecM () ()

-- | A writer monad for `SpecTree` forests.
newtype SpecM resource a = SpecM (Writer [SpecTree resource] a)
  deriving (Functor, Applicative, Monad)

-- | Convert a `Spec` to a forest of `SpecTree`s.
runSpecM :: SpecM r () -> [SpecTree r]
runSpecM (SpecM specs) = execWriter specs

-- | Create a `Spec` from a forest of `SpecTree`s.
fromSpecList :: [SpecTree r] -> SpecM r ()
fromSpecList = SpecM . tell

-- | The result of running an example.
data Result = Success | Pending (Maybe String) | Fail String
  deriving (Eq, Show, Read, Typeable)

forceResult :: Result -> Result
forceResult r = case r of
  Success   -> r
  Pending m -> m `deepseq` r
  Fail    m -> m `deepseq` r

instance E.Exception Result

type Progress = (Int, Int)

data Params = Params {
  paramsQuickCheckArgs  :: QC.Args
, paramsSmallCheckDepth :: Int
, paramsReportProgress  :: Progress -> IO ()
}

-- | Internal representation of a spec.
data SpecTree r =
    SpecGroup String [SpecTree r]
  | SpecItem (Item r)

data Item r = Item {
  itemIsParallelizable :: Bool
, itemRequirement :: String
, itemExample :: Params -> ((r -> IO ()) -> IO ()) -> IO Result
}

mapSpecItem :: forall r . (Item r -> Item r) -> SpecM r () -> SpecM r ()
mapSpecItem f = fromSpecList . map go . runSpecM
  where
    go :: SpecTree r -> SpecTree r
    go spec = case spec of
      SpecItem item -> SpecItem (f item)
      SpecGroup d es -> SpecGroup d (map go es)

-- | The @describe@ function combines a list of specs into a larger spec.
describe :: String -> [SpecTree r] -> SpecTree r
describe s = SpecGroup msg
  where
    msg
      | null s = "(no description given)"
      | otherwise = s

-- | Create a spec item.
it :: Example r a => String -> a -> SpecTree r
it s e = SpecItem $ Item False msg (evaluateExample e)
  where
    msg
      | null s = "(unspecified behavior)"
      | otherwise = s

-- | A type class for examples.
class Example r a where
  evaluateExample :: a -> Params -> ((r -> IO ()) -> IO ()) -> IO Result

instance Example r a => Example r (r -> a) where
  evaluateExample example params wrapper = do
    ref <- newIORef (error "new io ref")
    wrapper $ \ resource -> do
        result <- evaluateExample (example resource) params (\ spec -> spec resource)
        writeIORef ref result
    readIORef ref

instance Example r Bool where
  evaluateExample b _ _ = if b then return Success else return (Fail "")

instance Example r Expectation where
  evaluateExample e _ action = (action (const e) >> return Success) `E.catches` [
      E.Handler (\(HUnitFailure err) -> return (Fail err))
    , E.Handler (return :: Result -> IO Result)
    ]

instance Example r Result where
  evaluateExample r _ _ = return r

instance Example r QC.Property where
  evaluateExample p c action = do
    r <- QC.quickCheckWithResult (paramsQuickCheckArgs c) {QC.chatty = False}
        (QCP.callback progressCallback $ aroundProperty (\ spec -> action (const spec)) p)
    when (isUserInterrupt r) $ do
      E.throwIO E.UserInterrupt

    return $
      case r of
        QC.Success {}               -> Success
        QC.Failure {QC.output = m}  -> fromMaybe (Fail $ sanitizeFailureMessage r) (parsePending m)
        QC.GaveUp {QC.numTests = n} -> Fail ("Gave up after " ++ pluralize n "test" )
        QC.NoExpectedFailure {}     -> Fail ("No expected failure")
    where
      progressCallback = QCP.PostTest QCP.NotCounterexample $
        \st _ -> paramsReportProgress c (QC.numSuccessTests st, QC.maxSuccessTests st)

      sanitizeFailureMessage :: QC.Result -> String
      sanitizeFailureMessage r = let m = QC.output r in strip $
#if MIN_VERSION_QuickCheck(2,7,0)
        case QC.theException r of
          Just e -> let numbers = formatNumbers r in
            "uncaught exception: " ++ formatException e ++ " " ++ numbers ++ "\n" ++ case lines m of
              x:xs | x == (exceptionPrefix ++ show e ++ "' " ++ numbers ++ ": ") -> unlines xs
              _ -> m
          Nothing ->
#endif
            (addFalsifiable . stripFailed) m

      addFalsifiable :: String -> String
      addFalsifiable m
        | "(after " `isPrefixOf` m = "Falsifiable " ++ m
        | otherwise = m

      stripFailed :: String -> String
      stripFailed m
        | prefix `isPrefixOf` m = drop n m
        | otherwise = m
        where
          prefix = "*** Failed! "
          n = length prefix

      parsePending :: String -> Maybe Result
      parsePending m
        | exceptionPrefix `isPrefixOf` m = (readMaybe . takeWhile (/= '\'') . drop n) m
        | otherwise = Nothing
        where
          n = length exceptionPrefix

      exceptionPrefix = "*** Failed! Exception: '"

-- | Specifies a pending example.
--
-- If you want to textually specify a behavior but do not have an example yet,
-- use this:
--
-- > describe "fancyFormatter" $ do
-- >   it "can format text in a way that everyone likes" $
-- >     pending
pending :: Expectation
pending = E.throwIO (Pending Nothing)

-- | Specifies a pending example with a reason for why it's pending.
--
-- > describe "fancyFormatter" $ do
-- >   it "can format text in a way that everyone likes" $
-- >     pendingWith "waiting for clarification from the designers"
pendingWith :: String -> Expectation
pendingWith = E.throwIO . Pending . Just
