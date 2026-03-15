
module Hedgehog.Utils.BottomsSpec
( spec
) where

import Hedgehog.Utils.Bottoms qualified as UUT
import TestsPrelude


-- | Option to temporarily enable tests that will fail.
--
-- Useful for meta-testing the tests, and to be able to inspect failure reports.
--
-- default: False
enableFailingTests :: Bool
enableFailingTests = False


spec :: Spec
spec = do

  describe "*Bottom" $ do
    it "not bottom" $ UUT.assertNotBottom fine
    it "is  bottom" $ UUT.assertBottom    crash

  describe "*BottomNF" $ do
    it "not bottom" $ UUT.assertNotBottomNF fine
    it "is  bottom" $ UUT.assertBottomNF    crash

  when enableFailingTests $
    describe "SHOULD FAIL: assert that..." $ do

      describe "(*Bottom:)" $ do
        it "...a non-bottom is bottom    " $ UUT.assertBottom    fine
        it "...a bottom     is non-bottom" $ UUT.assertNotBottom crash
      describe "(*BottomNF:)" $ do
        it "...a non-bottom is bottom    " $ UUT.assertBottomNF    fine
        it "...a bottom     is non-bottom" $ UUT.assertNotBottomNF crash


fine :: HasCallStack => Int
fine  = length $ take 1 (1:error "fine boom")

crash :: HasCallStack => Int
crash = length $ take 2 (1:error "crash boom")

-- (not sure if sharing can be an issue)
{-# INLINE fine  #-}
{-# INLINE crash #-}
