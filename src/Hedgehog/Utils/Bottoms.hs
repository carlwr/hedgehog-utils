{- | Description: Hedgehog bottom assertions

These helpers classify “bottom” by catching t'SomeException'. This includes asynchronous exceptions (e.g. runner interrupts/timeouts), so results can be affected by external cancellation and do not always imply a defect in the pure expression.

-}

module Hedgehog.Utils.Bottoms
(
  assertBottom
, assertNotBottom
, assertBottomNF
, assertNotBottomNF
) where

import Hedgehog.Utils.Internal
import Hedgehog

import Control.Monad.IO.Class
import Control.Exception
import Control.DeepSeq
import GHC.Stack


-- | Expect evaluating to WHNF to throw.
assertBottom
  :: (MonadTest m, MonadIO m, HasCallStack)
  => a
  -> m ()
assertBottom x = do
  r <- evalIO $ try (evaluate x)
  case r :: Either SomeException _ of
    Left  _ -> success
    Right _ ->
      withFrozenCallStack $
      fail_with
        "Expected non-bottom"
        [ "(instead got some value)"
        ]



-- | Expect evaluating to WHNF __not__ to throw.
assertNotBottom
  :: (MonadTest m, MonadIO m, HasCallStack)
  => a
  -> m ()
assertNotBottom x = do
  r <- evalIO $ try (evaluate x)
  case r :: Either SomeException _ of
    Right _ -> footnote "assertNotBottom: passed." >> success
    Left  e ->
      withFrozenCallStack $
      fail_with
        "Expected non-bottom"
        [ "Got exception:"
        , "\"" ++ show e ++ "\""
        ]


-- | Expect evaluating to NF (deep) to throw.
assertBottomNF
  :: (MonadTest m, MonadIO m, NFData a, HasCallStack)
  => a
  -> m ()
assertBottomNF x =
  withFrozenCallStack $
  assertBottom (force x)


-- | Expect evaluating to NF (deep) __not__ to throw.
assertNotBottomNF
  :: (MonadTest m, MonadIO m, NFData a, HasCallStack)
  => a
  -> m ()
assertNotBottomNF x =
  withFrozenCallStack $
  assertNotBottom (force x)
