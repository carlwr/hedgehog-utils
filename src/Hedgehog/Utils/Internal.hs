module Hedgehog.Utils.Internal
(
  fail_with
, showq
, padLeft
, whenJust
) where

import Hedgehog
import GHC.Stack
import Hedgehog.Internal.Property qualified  as Prop


fail_with :: (MonadTest m, HasCallStack) => String -> [String] -> m a
fail_with heading body =
  withFrozenCallStack $
  Prop.failWith Nothing str
  where
    str = unlines ([ "━━ " ++ heading ++ " ━━" ] ++ body)

showq :: Show a => a -> String
showq x = "\"" ++ show x ++ "\""

padLeft :: a -> Int -> [a] -> [a]
padLeft x n xs = take k (repeat x) ++ xs
  where k = n - length xs

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust = \case
  Just x -> \f -> f x
  _      -> const (pure ())
