{- | Description: Hedgehog meta-testing

/"Hedgehog" meta-testing with a focus on reproducibility./

The property-running functions of this module:

- run the inner property using a t'Seed' that is either the module-hardcoded 'defaultSeed' or an explicitly passed t'Seed'

    - i.e.: the t'Seed' that is used when runing user tests never affects the running of inner properties

- do not produce terminal output in the form of run progress, failure reports/diffs and summaries

    - they do not however redirect stdout/stderr, meaning that e.g. @'liftIO' . 'putStr' . ...@ still writes to stdout

- will report inner properties that throw uncaught exceptions as __failing__

    - since "Hedgehog"s internal test runner catches exceptions, considering such properties as test failures

- are similar in spirit to "Hedgehog"'s 'check', however:

    - 'check' has the side effect of producing terminal output, in a way that can't be easily silenced
    - 'check' returns an @'IO' 'Bool'@ which does not distinguish between a test failure and giving up trying to generate input values
-}

{-# LANGUAGE OverloadedStrings #-}

module Hedgehog.Utils.MetaTesting
(
  -- * Running properties
  evalCheck
, evalCheckWith
, evalCheckProperty
, evalCheckPropertyWith

  -- * Asserting on the result of running properties
, MetaAssertion
, assertPassing
, assertGivingUp
, assertFailing
, assertFailingReport
, assertFailsWithReport
, checkSilently

  -- * Convenience functions
, isFailed
, Outcome(..)
, outcomeOf
, outcomeOf'

  -- * Utilities
, randomOutcomeProp
, TestException
, genSeed
, forAllProps
, forAllPropsWith

  -- * Default values reference
, defaultMetaConfig
, Size0
, defaultSize0
, defaultSeed

) where

import Hedgehog.Utils.Internal

import Hedgehog
import Hedgehog.Gen               qualified as Gen
import Hedgehog.Range             qualified as Range

import Hedgehog.Internal.Runner   qualified as Runner
import Hedgehog.Internal.Report   qualified as Report
import Hedgehog.Internal.Config   qualified as Cfg
import Hedgehog.Internal.Property qualified as Prop
import Hedgehog.Internal.Show     qualified as Shw
import Hedgehog.Internal.Seed     qualified as Seed

import Hedgehog.Internal.Report (Result, Report, FailureReport)

import Control.Monad.IO.Class
import Control.Exception
import GHC.Stack
import Data.Word
import Control.Monad


{- $setup
>>> import Hedgehog
>>> import Hedgehog.Gen qualified as Gen
>>> import Control.Monad.IO.Class
-}


{- | The action of running a property test on the given @'PropertyT' 'IO' ()@.

'defaultSeed' is used as t'Seed'.

'defaultSize0' is used as the initial t'Size'.

=== Example

>>> :{
  innerProp = do
    char <- forAll Gen.lower
    char /== 'b'               -- SOMETIMES FAILS!
:}

>>> :{
  myTest :: (MonadTest m, MonadIO m) => m ()
  myTest = do
    res <- evalCheck innerProp
    outcomeOf res === Failing
:}

Here we use "Hedgehog"s 'check' to run the test. 'check' prints some output to stdout, and returns a 'True' IO action:

>>> check (property myTest) :: IO Bool
...
True

The omitted stdout output would be something like:

> ✓ <interactive> passed 100 tests.

Our own test (outer property) passes, because it asserts that the inner property should fail when it is run - which it does.
-}
evalCheck
  :: (MonadIO m, HasCallStack)

  {- | An __inner property__ to run as a property test.
  -}
  => PropertyT IO ()

  {- | The result of running a property test on the inner property.

  In our test code, we typically put this value in some 'MonadTest' context which allows making assertions on the 'Result'. We then refer to this @m 'Result'@ as the __outer property__.
  -}
  -> m Result

evalCheck prop =
  Report.reportStatus <$>
    ( withFrozenCallStack
      evalCheckWith
        defaultMetaConfig
        defaultSize0
        defaultSeed
        prop
    )

_evalCheck_doctest :: a
_evalCheck_doctest = undefined
  where

    innerProp = do
      char <- forAll Gen.lower
      char /== 'b'

    myTest :: (MonadTest m, MonadIO m) => m ()
    myTest = do
      res <- evalCheck innerProp
      outcomeOf res === Failing

    _res = check (property myTest) :: IO Bool


{- | The action of running a property test on the given @'PropertyT' 'IO' ()@.

Compared to 'evalCheck', this function takes explicit run parameters and has a richer return type.

This function plays a role similar to that of @checkReport@ in "Hedgehog.Internal.Runner".
-}
evalCheckWith
  ::  ∀ m. (MonadIO m, HasCallStack)
  => Prop.PropertyConfig -- ^ limits etc. to use when running the property test
  -> Size0
  -> Seed
  -> PropertyT IO () -- ^ the inner property to run
  -> m (Report Result)
evalCheckWith propCfg size0 seed prop =
  withFrozenCallStack $
  liftIO (runInner prop)
  where
    runInner :: PropertyT IO () -> IO (Report Result)
    runInner prop' = Runner.checkReport propCfg size0 seed prop' silentReporter

    silentReporter :: (Report Report.Progress -> IO ())
    silentReporter _ = pure ()


{- | The action of running a property test on the given 'Property'.
-}
evalCheckProperty
  ::  ∀ m. (MonadIO m, HasCallStack)
  => Property  -- ^ the inner 'Property' to run
  -> m Result
evalCheckProperty propty =
  Report.reportStatus <$>
    ( withFrozenCallStack $
      evalCheckPropertyWith
        defaultSize0
        defaultSeed
        propty
    )


evalCheckPropertyWith
  ::  ∀ m. (MonadIO m, HasCallStack)
  => Size0
  -> Seed
  -> Property  -- ^ the inner 'Property' to run
  -> m (Report Result)
evalCheckPropertyWith size0 seed (Prop.Property propCfg prop) =
  withFrozenCallStack $
  evalCheckWith propCfg size0 seed prop


-- --- assertions ---

type MetaAssertion m
  =  (MonadTest m, MonadIO m, HasCallStack)
  => PropertyT IO () -> m ()

assertPassing  :: MetaAssertion m
assertFailing  :: MetaAssertion m
assertGivingUp :: MetaAssertion m

assertPassing  = withFrozenCallStack $ asrt_impl "succeed" (==Report.OK)
assertFailing  = withFrozenCallStack $ asrt_impl "fail"    isFailed
assertGivingUp = withFrozenCallStack $ asrt_impl "give up" (==Report.GaveUp)

asrt_impl
  :: (MonadIO m, MonadTest m, HasCallStack)
  => String
  -> (Result->Bool)
  -> PropertyT IO ()
  -> m ()
asrt_impl didNot p prop =
  withFrozenCallStack $ do
  r <- evalCheck prop
  when (not (p r)) $ fail_with_didNotMsg r didNot

assertFailingReport
  :: (MonadIO m, MonadTest m, HasCallStack)
  => (FailureReport -> Bool) -- ^ assertion on the failure report
  -> PropertyT IO ()
  -> m ()
assertFailingReport f p =
  withFrozenCallStack $ do
  res <- evalCheck p
  case res of
    Report.OK             -> fail_with_didNotMsg res "fail"
    Report.GaveUp         -> fail_with_didNotMsg res "fail"
    Report.Failed fReport
      |f fReport -> success
      |otherwise ->
         fail_with
           "Property test failed, but not with the expected FailureReport."
           [Shw.showPretty res]

{- | @'assertFailsWithReport'  ==  'flip' 'assertFailingReport'@
-}
assertFailsWithReport
  :: (MonadIO m, MonadTest m, HasCallStack)
  => PropertyT IO ()
  -> (FailureReport -> Bool) -- ^ assertion on the failure report
  -> m ()
assertFailsWithReport =
  withFrozenCallStack $
  flip assertFailingReport


fail_with_didNotMsg :: (MonadTest m, HasCallStack) => Result -> String -> m a
fail_with_didNotMsg res didNot =
  withFrozenCallStack $
  fail_with
    ( "Assertion failed: property test did not " ++ didNot)
    [ "actual (unexpected) result:"
    , Shw.showPretty res
    , ":: Hedgehog.Internal.Report.Result"
    ]


{- | Check a property and return 'True' if running it is a successful test. If it is a failing test or gives up generating input values, return 'False'.

Similar to "Hedgehog"s 'check', but this function does not write to stdout. It also uses the defaults defined by this module.
-}
checkSilently :: (MonadIO m, HasCallStack) => Property -> m Bool
checkSilently prop =
  withFrozenCallStack $ do
    res <- evalCheckProperty prop
    pure $ case outcomeOf res of
      Passing -> True
      GivinUp -> False
      Failing -> False


-- --- convenience functions ---

isFailed :: Result -> Bool
isFailed = \case
  Report.Failed _ -> True
  _               -> False

{- | The outcome of running a property test for a property.

This type is similar to "Hedgehog"s type 'Result', but it omits the failure report field for failing tests.
-}
data Outcome =
    Passing  -- ^ the test was successful
  | GivinUp  -- ^ the test gave up generating input values
  | Failing  -- ^ there was a test failure
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

outcomeOf :: Result -> Outcome
outcomeOf = \case
  Report.OK       -> Passing
  Report.GaveUp   -> GivinUp
  Report.Failed _ -> Failing

outcomeOf' :: Report Result -> Outcome
outcomeOf' = outcomeOf . Report.reportStatus


-- --- defaults references ---

{- | A default 'Prop.PropertyConfig' suitable for meta-properties.

It is almost the same as "Hedgehog"'s 'Prop.defaultConfig'. It differs only by not taking the /skip/ value from environment variable @HEDGEHOG_SKIP@ in case there is one. This is a small tweak for improved determinism.
-}
defaultMetaConfig :: Prop.PropertyConfig
defaultMetaConfig =
  Prop.defaultConfig{ Prop.propertySkip = Just Cfg.SkipNothing }

-- | An initial size when running a property test.
type Size0 = Size

{- | A default initial t'Size' when running property test.

It is set to the same value that "Hedgehog" uses internally by default.
-}
defaultSize0 :: Size0
defaultSize0 = 0

{- | A default t'Seed' whose value is hard-coded by this module.

@
import "Hedgehog.Internal.Seed" ('Seed.from')

'defaultSeed'  ==  'Seed.from' (1 :: 'Word64')
@
-}
defaultSeed :: Seed
defaultSeed = Seed.from default_seed_arg

default_seed_arg :: Word64
default_seed_arg = 1


-- --- utilities ---

{- | A property that when evaluated has roughly equal probability to succeed, fail or give up. (As long as the property config parameters are left unchanged.)
-}
randomOutcomeProp :: HasCallStack => Property
randomOutcomeProp =
    withTests    1
  . withDiscards 1
  . withShrinks  0
  $ Prop.Property defaultMetaConfig propT
  where
    propT = do
      (tag,innerProp) <- forAllWith fst gen
      footnote $ "this property is " ++ show tag
      innerProp

    gen = Gen.frequency $
      [ (10,Gen.constant ("succeeding"       ,success    ))
      , ( 5,Gen.constant ("failing"          ,failure    ))
      , ( 5,Gen.constant ("failing (IO-exc.)",ioException))
      , (10,Gen.constant ("discarding"       ,discard    ))
      ]

    ioException = evalIO (throwIO TestException)


data TestException = TestException deriving Show
instance Exception TestException


{- | Random t'Seed' values, properly constructed.

Generated seeds have one shrink: 'defaultSeed'.
-}
genSeed :: Gen Seed
genSeed =
  Gen.frequency
    [ ( 1,Gen.constant defaultSeed)
    , (20,Seed.from <$> Gen.prune (Gen.word64 range))
    ]
  where
    range = Range.constant r_0 (r_0 + r_width)

    r_0     = default_seed_arg + 1
    r_width = 0xffff_ffff


{- | Use a generator of random properties to draw a random property. Run a property test on it and return the outcome of doing so.

If the outer property fails, the failure report will show the inner property using its 'Prop.PropertyConfig' and a footnote will show the size, seed and 'Outcome'.
-}
forAllProps :: (MonadIO m,HasCallStack) =>
     Gen Property        -- ^ a generator of properties
  -> Size0               -- ^ (for the inner property - not the generator)
  -> Seed                -- ^ (for the inner property - not the generator)
  -> PropertyT m Outcome -- ^ the outcome of running the generated property
forAllProps = forAllProps_impl id shw
  where
    shw = Shw.showPretty . Prop.propertyConfig


{- | Like 'forAllProps', but with a generator that also generates a 'String' for each property.

If the user test fails, the failure report will use the 'String' to show the inner property.
-}
forAllPropsWith :: (MonadIO m,HasCallStack) =>
     Gen (String,Property) -- ^ a generator of properties
  -> Size0
  -> Seed
  -> PropertyT m Outcome
forAllPropsWith = forAllProps_impl snd fst


forAllProps_impl :: (MonadIO m,HasCallStack) =>
     (a -> Property)
  -> (a -> String)
  -> Gen a
  -> Size0
  -> Seed
  -> PropertyT m Outcome
forAllProps_impl f shw gen sz sd = do
  prop    <- forAllWith shw gen
  outcome <- outcomeOf' <$> evalCheckPropertyWith sz sd (f prop)
  footnote (footMsg outcome)
  pure outcome
  where
    footMsg outcome = unlines
      [ "running the inner property"
      , "  - used size" ++ show sz ++ ", seed " ++ show sd
      , "  - resulted in the outcome " ++ show outcome
      ]


-- --- misc ---

{- dev notes:

my understanding: Runner.checkReport catches exceptions in the properties it runs, considering such properties test failures. That would mean that the IO that Runner.checkReport returns is only expected to throw (uncaught) exceptions in case of some unexpected problem with actually running the test at all.
  -> choose to evaluate Runner.checkReport with liftIO, not evalIO
  -> allows our functions to return values in MonadIO, rather than the stronger MonadTest
    -> MonadIO rather than MonadTest here has the further benefit of being more intuitive (clearer distinction in signatures between inner properties and the outer MonadIO)

Hedgehog.Internal.Runner:
  call chain:
    check --> checkNamed --> checkRegion --> checkReport

test/Test/Hedgehog/Skip.hs
  their checkProp is similar to our checkSilently
  https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog/test/Test/Hedgehog/Skip.hs
-}


{- some dead code; keep for now

hoistMb :: MonadTest m => Maybe b -> MaybeT m b
hoistMb mb = hoistMaybe mb

generalizePropToMonadIO :: (MonadIO m) => PropertyT IO a -> PropertyT m a
generalizePropToMonadIO = hoist liftIO

-- | Lift a 'PropertyT' in IO to a 'PropertyT' in MonadTest.
--
-- If the 'IO' action throws an exception, the test is failed.
generalizePropToMonadTest :: (MonadTest m, MonadIO m) => PropertyT IO a -> PropertyT m a
generalizePropToMonadTest = hoist evalIO

-}
