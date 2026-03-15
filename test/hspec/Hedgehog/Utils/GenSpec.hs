module Hedgehog.Utils.GenSpec
( spec
) where

import Hedgehog.Utils.Gen qualified as UUT
import TestsPrelude


spec :: Spec
spec = do
  describe "elementFrequency" spec_elementFrequency

spec_elementFrequency :: Spec
spec_elementFrequency =
  modifyMaxSuccess (const 1_000) $ do
    it "covers" $ do
      let
        gen = UUT.elementFrequency
          [(1,'a')
          ,(9,'b')
          ]
      x <- forAll gen
      cover  5 "a" (x=='a')
      cover 80 "b" (x=='b')
