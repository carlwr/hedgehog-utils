module Hedgehog.Utils.MetaTestingSpec
( spec
) where

import Hedgehog.Utils.MetaTesting qualified as UUT

import TestsPrelude hiding (withFrozenCallStack)

import Hedgehog.Utils.MetaTesting (Outcome)
import Hedgehog.Utils             qualified as Utils

import Hedgehog.Gen               qualified as Gen

import Hedgehog.Internal.Report   qualified as Report
import Hedgehog.Internal.Show     qualified as Shw
import Hedgehog.Internal.Source   qualified as Src

import Data.List qualified as L



-- --- preamble, documentation ---

{-| Enable tests that will deliberately fail.

Useful for meta-testing the tests, and to be able to inspect failure reports.

Always checked in as 'False'.
-}
enableFailingTests :: Bool
enableFailingTests = False

{-| Enable tests on the source locations in failure reports.

Not overly refactor- or future-proof, but very helpful to at least have at hand.

=== Debugging hints

Likely causes to source locations...
  ...missing:
    -> missing HasCallStack
  ...wrong/pointing into MetaTesting.hs:
    -> missing withFrozenCallStack

-}
enableCheckSourceLocationFilename :: Bool
enableCheckSourceLocationFilename = True

-- | Name of this filename; used in source location tests
fname_self :: FilePath
fname_self = "MetaTestingSpec.hs"

-- | Don't import/use 'GHC.Stack.withFrozenCallStack' in this file.
--
-- For tests, the withFrozenCallStack highest in the call chain should be one _in the code under test_, so proper callstack freezing in code under test is observable in testing.
withFrozenCallStack :: don't use me
withFrozenCallStack =
  error ""

-- | If this is not an ambiguous definition error, withFrozenCallStack is not imported (good).
_shouldn't_be_ambiguous :: a
_shouldn't_be_ambiguous = undefined withFrozenCallStack


-- --- specs ---

spec :: Spec
spec = do
  describe "smoke tests"                   spec_smoke
  describe "main"                          spec_main
  describe "source locations"              spec_sourceLocations
  describe "main, provoke failing"         spec_mainFailing
  describe "inner bottoms"                 spec_innerBottoms
  describe "genSeed"                       spec_genSeed
  describe "randomOutcomeProp"             spec_randomOutcomeProp
  describe "seed integrity (nested props)" spec_seedIntegrity


-- --- example properties ---

bottomIO :: (MonadTest m, MonadIO m) => m a
bottomIO = do
  footnote "inner is bottomIO"
  evalIO (error "error: bottomIO")

bottom :: a
bottom = error "I am a bottom"

data SpecTestException = SpecTestException deriving Show
instance Exception SpecTestException

throwin :: (MonadTest m, MonadIO m) => m a
throwin = do
  footnote "inner is throwin"
  evalIO (throwIO SpecTestException)


-- --- smoke tests ---

spec_smoke :: Spec
spec_smoke = unitTests $ do
  it "assertPassing   " $  UUT.assertPassing success
  it "assertFailing   " $  UUT.assertFailing failure
  it "evalCheck: pass " $ (UUT.evalCheck success) >>= (===Report.OK)
  it "evalCheck: fail " $ (UUT.evalCheck failure) >>= (assert . UUT.isFailed)
  it "evalCheck: throw" $ (UUT.evalCheck throwin) >>= (assert . UUT.isFailed)


-- --- main spec ---

type WithInner =
  ( Prop -> Prop
  , Prop -> Prop
  , Prop -> Prop
  )

type WithInnerP =
  ( Property -> Prop
  , Property -> Prop
  , Property -> Prop
  )

spec_main :: Spec
spec_main = unitTests $ do

  mkSpec  "evalCheck            " evalChecks
  mkSpec  "evalCheck + outcomeOf" outcomeOf
  mkSpec  "UUT.assert*          " uut_asserts
  mkSpecP "checkSilently        " checkSilentlies

  where

    evalChecks =
      ( \p -> do  res <- UUT.evalCheck p; res===Report.OK
      , \p -> do  res <- UUT.evalCheck p; assert (UUT.isFailed res)
      , \p -> do  res <- UUT.evalCheck p; res===Report.GaveUp
      )

    outcomeOf =
      ( \p -> do  res <- UUT.evalCheck p; UUT.outcomeOf res===UUT.Passing
      , \p -> do  res <- UUT.evalCheck p; UUT.outcomeOf res===UUT.Failing
      , \p -> do  res <- UUT.evalCheck p; UUT.outcomeOf res===UUT.GivinUp
      )

    checkSilentlies =
      ( \p -> do  res <- UUT.checkSilently p; assert res
      , \p -> do  res <- UUT.checkSilently p; assert (not res)
      , \p -> do  res <- UUT.checkSilently p; assert (not res)
      )

    uut_asserts =
      ( UUT.assertPassing
      , UUT.assertFailing
      , UUT.assertGivingUp
      )

    mkSpec :: String -> WithInner -> Spec
    mkSpec s (asrtSucc,asrtFail,asrtGU) =
      describe s $ do
        it "inner succeeds " $ asrtSucc success
        it "inner fails    " $ asrtFail failure
        it "inner IO-throws" $ asrtFail throwin
        it "inner gives up " $ asrtGU   discard

    mkSpecP :: String -> WithInnerP -> Spec
    mkSpecP s = mkSpec s . map3 (. property)


-- --- source locations ---

spec_sourceLocations :: Spec
spec_sourceLocations = unitTests $ do
  let

    fReportOf = \case
      Report.Failed fReport -> pure fReport
      Report.OK             -> annotate "did not fail" >> failure
      Report.GaveUp         -> annotate "did not fail" >> failure

    checkFailureLocation :: MonadTest m => Report.FailureReport -> m ()
    checkFailureLocation rep = do
      annotate (Shw.showPretty rep)
      case Report.failureLocation rep of
        Nothing    -> annotate "source location is missing" >> failure
        Just span' ->
          when enableCheckSourceLocationFilename $
            let (Src.Span fpath _ _ _ _) = span' in
            diff fname_self L.isSuffixOf fpath

  describe "assert*" $ do

    let
      checkIsFromAssertion =
        diff "Assertion failed:" L.isInfixOf . Report.failureMessage

    it "assertFailing" $ do
      result  <- UUT.evalCheck (UUT.assertFailing success)
      fReport <- fReportOf result
      checkIsFromAssertion fReport
      checkFailureLocation fReport

    it "assertPassing" $ do
      result  <- UUT.evalCheck (UUT.assertPassing failure)
      fReport <- fReportOf result
      checkIsFromAssertion fReport
      checkFailureLocation fReport

  describe "evalCheck*" $ do

    it "evalCheck" $ do
      result  <- UUT.evalCheck failure
      fReport <- fReportOf result
      checkFailureLocation fReport

    it "evalCheckWith" $ do
      let
        uut_evalCheckWith =
          UUT.evalCheckWith
            UUT.defaultMetaConfig
            UUT.defaultSize0
            UUT.defaultSeed

      rResult <- uut_evalCheckWith failure
      fReport <- fReportOf (Report.reportStatus rResult)
      checkFailureLocation fReport


-- --- main - failing ---

spec_mainFailing :: Spec
spec_mainFailing =
  unitTests
  $ whenEnableFailing "(inner is,assert that)..."
  $ do

    describe "UUT.assert*, non-bottom inners" $ do

      it "succ ,Fail  " $ UUT.assertFailing  success
      xt "succ ,GaveUp" $ UUT.assertGivingUp success
      xt "fail ,Succ  " $ UUT.assertPassing  failure
      xt "fail ,GaveUp" $ UUT.assertGivingUp failure
      xt "g.up ,Succ  " $ UUT.assertPassing  discard
      xt "g.up ,Fail  " $ UUT.assertFailing  discard
      xt "pThrw,GaveUp" $ UUT.assertGivingUp throwin
      xt "pThrw,succ  " $ UUT.assertPassing  throwin
      -- (not necessarily exhaustive)

    describe "evalCheck" $ do
      xt "fail ,OK    " $ UUT.evalCheck failure  >>= (=== Report.OK)
      xt "g.up ,OK    " $ UUT.evalCheck discard  >>= (=== Report.OK)
      xt "IOerr,GaveUp" $ UUT.evalCheck bottomIO >>= (=== Report.GaveUp)
      xt "succ ,Failed" $ UUT.evalCheck success  >>= (assert . UUT.isFailed)


-- --- inner bottoms ---

-- documentation of the current behaviour; possibly, other behaviours could be defensible as well

spec_innerBottoms :: Spec
spec_innerBottoms = unitTests $ do

  describe "(inner-is,assert-that):" $ do
    it "bottom  ,fail" $ UUT.assertFailing  bottom
    it "bottomIO,fail" $ UUT.assertFailing  bottomIO

  whenEnableFailing "(inner is,assert that)..." $ do
    xt "bottom  ,succ" $ UUT.assertPassing  bottom
    xt "bottomIO,succ" $ UUT.assertPassing  bottomIO
    xt "bottom  ,g.up" $ UUT.assertGivingUp bottom
    xt "bottomIO,g.up" $ UUT.assertGivingUp bottomIO


-- --- genSeed ---

spec_genSeed :: Spec
spec_genSeed =
  modifyMaxSuccess (const 1000) $
  it "coverage" $ do
    s <- forAll genSeed
    cover 1  "defaultSeed" (s==UUT.defaultSeed)


-- --- randomOutcomeProp ---

-- | convenience helper
chk :: MonadIO m => Seed -> Property -> m Outcome
chk seed pr =
    UUT.outcomeOf' <$> UUT.evalCheckPropertyWith UUT.defaultSize0 seed pr

spec_randomOutcomeProp :: Spec
spec_randomOutcomeProp = do

  let
    f :: MonadIO m => Seed -> m Outcome
    f s = chk s UUT.randomOutcomeProp

  modifyMaxSuccess (const 1000) $ do

    it "coverage (outcome)" $ do
      s <- forAll genSeed
      x <- f s
      cover 15 "Failing" (x==UUT.Failing)
      cover 15 "GivinUp" (x==UUT.GivinUp)
      cover 15 "Passing" (x==UUT.Passing)

  ( modifyMaxSuccess      (const 100) .
    modifyMaxDiscardRatio (const   1) ) $

    describe "property is" $ do

      it "deterministic" $ do
        s  <- forAll genSeed
        Utils.deterministic (f s)

      it "deterministic (interleaved)" $ do
        (s0,s1) <- forAll genSeeds
        Utils.deterministicF f (s0,s1)

      it "seed-sensitive" $ do
        (s0,s1) <- forAll genSeeds
        Utils.notConstF_M f (s0,s1)


-- --- seed integrity ---

-- to be able to vary an outer and an inner seed in a controlled way, use a meta-property nested inside another meta-property

-- use tagged phantom types to enforce correct pairing of seed<->outcome for each of the inner and outer properties

data I   -- (I)nner
data O   -- (O)uter

type Outcome' :: Type -> Type
type Seed'    :: Type -> Type

newtype Outcome' b = Outcome' Outcome deriving (Eq,Show,Enum,Bounded)
newtype Seed'    b = Seed'    Seed    deriving (Eq,Show)

-- | tagged version of 'chk'; using this to run properties enforces that the same tag is used for the seed and the outcome
chk' :: ∀ b m. MonadIO m
  => Seed' b
  -> Property
  -> m (Outcome' b)
chk' (Seed' seed) pr = Outcome' <$> chk seed pr

-- | two nested property tests
nested_ :: MonadIO m
  => Property             -- ^ inner property to run
  -> (Outcome' I -> Prop) -- ^ assertion for the outcome of the inner property
  -> Seed' I
  -> Seed' O
  -> m (Outcome' O)
nested_ innerProp asrt i o = chk' o outerProp
  where
    outerProp = property $ do
      r <- chk' i innerProp
      asrt r

spec_seedIntegrity :: Spec
spec_seedIntegrity =
  let

    f :: MonadIO m
      => (Outcome' I -> Prop)
      -> Seed' I
      -> Seed' O
      -> m (Outcome' O)
    f = nested_ UUT.randomOutcomeProp

    gen_i    = Seed' @I <$> genSeed
    gen_o    = Seed' @O <$> genSeed
    gen_asrt = Gen.enumBounded :: Gen (Outcome' I)

    gen =
      (,,,,)
        <$> gen_asrt
        <*> gen_i
        <*> gen_i
        <*> gen_o
        <*> gen_o

  in
  modifyMaxSuccess (const 50) $ do

  describe "pre-conditions" $ do

    modifyMaxSuccess (const 200) $
      it "coverage (outer outcome)" $ do
        (asrt,i,_,o,_) <- forAll gen
        res <- f (===asrt) i o
        cover 20 "Passing" (res == Outcome' UUT.Passing)
        cover 40 "Failing" (res == Outcome' UUT.Failing)

    it "outer not giving up" $ do
      (asrt,i,_,o,_) <- forAll gen
      res <- f (===asrt) i o
      res /== Outcome' UUT.GivinUp

  describe "outer outcome is" $ do

    it "deterministic" $ do
      (asrt,i,_,o,_) <- forAll gen
      Utils.deterministic $ f (===asrt) i o

    it "deterministic (interleaved)" $ do
      (asrt,o0,o1,i0,i1) <- forAll gen
      let f' = uncurry (f (===asrt))
      Utils.deterministicF f' ((o0,i0),(o1,i1))

    let
      notConstF_M = Utils.notConstF_M
      constF_M    = Utils.constF_M

      notConst (asrt,i0,i1,o ,_ ) = notConstF_M (\i -> f (===asrt) i o) (i0,i1)
      isConst  (asrt,i ,_ ,o0,o1) = constF_M    (\o -> f (===asrt) i o) (o0,o1)

    it "provedly non-constant w.r.t. inner seed" $ forAll gen >>= notConst
    it "seemingly constant    w.r.t. outer seed" $ forAll gen >>= isConst


-- --- general helpers ---

whenEnableFailing  :: HasCallStack => String -> Spec -> Spec
xwhenEnableFailing :: HasCallStack => String -> Spec -> Spec

whenEnableFailing  t = when enableFailingTests .  describe (failDesc t)
xwhenEnableFailing t = when enableFailingTests . xdescribe (failDesc t)

failDesc :: String -> String
failDesc = \case
  "" -> "SHOULD FAIL"
  t  -> "SHOULD FAIL: " ++ t


genSeed :: Gen Seed
genSeed = UUT.genSeed

genSeeds :: Gen (Seed,Seed)
genSeeds = (,) <$> UUT.genSeed <*> UUT.genSeed

map3 :: (a -> b) -> (a,a,a) -> (b,b,b)
map3 f (x,y,z) = (f x,f y,f z)


_silence_unusedWarnings :: _
_silence_unusedWarnings = undefined xwhenEnableFailing
