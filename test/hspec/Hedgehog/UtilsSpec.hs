module Hedgehog.UtilsSpec
( spec
) where

import Hedgehog.Utils qualified as UUT

import TestsPrelude

import Hedgehog.Utils.MetaTesting qualified as Meta

import Hedgehog.Gen   qualified as Gen
import Hedgehog.Range qualified as Range



spec :: Spec
spec = do

  it "deterministic" $ do
    x <- forAll $ Gen.int (Range.linear 0 100)
    UUT.deterministic (pure x)

  it "deterministicF" $ do
    let gen = Gen.int (Range.linear 0 100)
    (x,y) <- forAll $ (,) <$> gen <*> gen
    UUT.deterministicF pure (x,y)

  it "constF_M" $ do
    let gen_x = Gen.int (Range.linear 0 100)
        gen_y = Gen.alpha
    (x0,x1,y) <- forAll $ (,,) <$> gen_x <*> gen_x <*> gen_y
    let f _ = pure y
    UUT.constF_M f (x0,x1)

  describe "notConstF_M" $ do
    it "can prove non-constancy" $ do
      (_,(f,(x0,x1))) <- forAllWith fst (genF 30 30)
      UUT.notConstF_M f (x0,x1)
    it "gives up if function is constant" $ do
      (_,(f,(x0,x1))) <- forAllWith fst (genF 30 0)
      Meta.assertGivingUp $ UUT.notConstF_M f (x0,x1)
    it "gives up if only equal evaluation points" $ do
      (_,(f,(x0,x1))) <- forAllWith fst (genF 0 30)
      Meta.assertGivingUp $ UUT.notConstF_M f (x0,x1)


newtype X = X Int deriving (Eq,Show)
newtype Y = Y Int deriving (Eq,Show)

type RandomFunction a b =
  ( String       -- some String representation of the function
  , ( a -> IO b  -- the function
    , (a,a)      -- supported points of evaluation
    )
  )

-- | a generator of random functions
genF
  :: Int  -- ^ number of 'X' values to draw from for random 'X' values
  -> Int  -- ^ (the same, for 'Y')
  -> Gen (RandomFunction X Y)
genF w_x w_y = do
  (x0,x1,y0,y1) <- gen
  let
    f x|x==x0     = pure y0
       |x==x1     = pure y1
       |otherwise = error "unsupported evaluation point"
    f_show = "<function:" ++ show (x0,x1) ++ "-->" ++ show (y0,y1) ++ ">"
  pure (f_show,(f,(x0,x1)))
  where
    gen_x = X <$> Gen.int (Range.linear 0 w_x)
    gen_y = Y <$> Gen.int (Range.linear 0 w_y)
    gen =
      (,,,)
        <$> gen_x
        <*> gen_x
        <*> gen_y
        <*> gen_y


