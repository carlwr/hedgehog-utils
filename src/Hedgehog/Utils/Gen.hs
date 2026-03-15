{-# LANGUAGE OverloadedStrings #-}

module Hedgehog.Utils.Gen
(
  -- * Generators
  boolSeq
, nat
, str
, elementFrequency

  -- * Utilities
, includeSize
, includeSize'
, shrinkFirst
, trace

-- * "Hedgehog.Gen" re-export
, module Hedgehog.Gen
)
where

import Hedgehog.Utils.Internal

import Hedgehog
import Hedgehog.Gen -- for re-export
import Hedgehog.Gen   qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Internal.Gen  qualified as I_Gen
import Hedgehog.Internal.Tree qualified as Tree

import Data.Sequence qualified as Seq
import Debug.Trace   qualified as Debug
import GHC.Stack


boolSeq :: MonadGen m => Int -> m (Seq.Seq Bool)
boolSeq n = Gen.seq (Range.linear 0 n) Gen.bool

nat :: MonadGen m => m Int
nat = Gen.int (Range.linear 0 100)

str :: MonadGen m => m String
str = Gen.string (Range.linear 0 4) Gen.alpha


{- | Uses a weighted distribution to randomly select one of the elements in the list.

The generator shrinks towards the first element in the list.

/The input list must be non-empty./
-}
elementFrequency :: (MonadGen m, HasCallStack) => [(Int,a)] -> m a
elementFrequency xs =
  withFrozenCallStack $
  Gen.frequency [(w,Gen.constant a) | (w,a) <- xs]


includeSize :: MonadGen m => m a -> m (Size, a)
includeSize gen = Gen.sized $ \sz -> (sz,)<$>gen


includeSize' :: MonadGen m => m a -> m (String,a)
includeSize' = fmap f . includeSize
  where
    f (Size i,x) = (padLeft ' ' 3 (show i),x)


-- | Make a generator print any generated values using 'Debug.Trace.trace'. A custom string is prepended to each trace message.
trace :: (MonadGen m, Show a) => String -> m a -> m a
trace desc g = Gen.sized $ \(Size _sz) -> do
  x <- g
  Debug.trace (desc++" "++show x) (pure x)


{- | Give the generator additional shrinking options, while keeping the
  existing shrinks intact.

The new shrinks will be tried before existing shrinks. This is different from 'Gen.shrink', which is otherwise similar.
-}
shrinkFirst :: MonadGen m => (a -> [a]) -> m a -> m a
shrinkFirst f = withGenT $ I_Gen.mapGenT (expand' f)
  where

    -- "Expand a tree using an unfolding function."
    -- (adjusted version of expand' from Hedgehog.Internal.Tree)
    expand' :: Monad m => (a -> [a]) -> Tree.TreeT m a -> Tree.TreeT m a
    expand' f' m =
      Tree.TreeT $ do
        Tree.NodeT x xs <- Tree.runTreeT m
        pure . Tree.NodeT x $
          Tree.unfoldForest f' x ++ fmap (expand' f') xs
          -- orig.: fmap (expand' f) xs ++ Tree.unfoldForest f x

    withGenT g = fromGenT . g . toGenT
