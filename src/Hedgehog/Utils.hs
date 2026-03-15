module Hedgehog.Utils
(
  -- * Assertions
  assertWith
, noneUnequal
, allEqual
, FuncTest
, (===.)
, (===..)
, (===...)
, MaterialTest
, (<==>)
, (<==>.)
, (<==>..)
, deterministic
, deterministicF
, constF_M
, notConstF_M

  -- * Meta-testing
, module Hedgehog.Utils.MetaTesting

  -- * Bottoms
, module Hedgehog.Utils.Bottoms

  -- * Annotations
, maybeCollect
, maybeLabelS
, labelS

  -- * Misc.
, failWith
, checkOnce

)
where

import Hedgehog.Utils.Internal

import Hedgehog.Utils.MetaTesting
import Hedgehog.Utils.Bottoms

import Hedgehog
import Hedgehog.Internal.Show     qualified as Shw

import Data.Foldable
import Data.List qualified as L
import Data.String
import Control.Monad.IO.Class
import Control.Monad
import GHC.Stack


{- $setup
>>> import Hedgehog
-}


--- Assertions ---

assertWith :: (MonadTest m, Show a, HasCallStack) => (a -> Bool) -> a -> m ()
assertWith f x =
  withFrozenCallStack $
  when (not (f x)) $
    fail_with
      "The assertion returned False when applied to the value."
      [Shw.showPretty x]


{-| Fail the test if, for a 'Foldable' with at least two elements, not all elements are equal.

The 'Foldable' is tested by comparing the first element to each subsequent element.

Note: This function trades performance for some other potential benefits (laziness, no 'Ord' requirement). It may be slow on large inputs.

== Examples

>>> checkOnce (noneUnequal [1,1,1])
...
True

>>> checkOnce (noneUnequal [1,1,99])
...
False

The traversal short-circuits if a non-equal element is encountered:

>>> checkOnce (noneUnequal [1,1,99,error "boom"])
...
False

If <= 1 element, the test will always pass:

>>> checkOnce (noneUnequal [1])
...
True
-}
noneUnequal
  :: (MonadTest m, Foldable t, Eq a, Show a, HasCallStack)
  => t a
  -> m ()
noneUnequal ys =
  withFrozenCallStack $
  case toList ys of
    x0:xs -> should_eq x0 `traverse_` xs
    _     -> pure ()
  where
    should_eq x0 x
      |x==x0     = pure ()
      |otherwise =
        withFrozenCallStack $
        fail_with
          "some are unequal"
          ["element " ++ showq x ++ " not equal to first element " ++ showq x0
          ]


{- | Fail the test if, for a 'Foldable' with at least two elements, not all elements are equal. With no or one element, the test is discarded.

>>> checkOnce (allEqual [1,1,1])
...
True

>>> checkOnce (allEqual [1,1,99])
...
False

>>> check . withDiscards 10 . property $ (allEqual [1])
...gave up...
False

-}
allEqual :: (Monad m, Foldable t, Eq a, Show a, HasCallStack) =>
  t a -> PropertyT m ()
allEqual ys
  | x0:x1:xs <- toList ys  = (x1===x0) >> (=== x0) `traverse_` xs
  | otherwise              = discard


type FuncTest m r = (MonadTest m, Show r, Eq r, HasCallStack)

{- | Fail the test if the two functions, after each having been applied to the value, do not return equal results.

'===..' and '===...' are similar, but for functions taking two and three arguments, respectively.
-}
(===.)  ::FuncTest m r =>       (a->r) ->       (a->r)           -> a -> m ()
(===..) ::FuncTest m r =>    (b->a->r) ->    (b->a->r)      -> b -> a -> m ()
(===...)::FuncTest m r => (c->b->a->r) -> (c->b->a->r) -> c -> b -> a -> m ()

lhs ===.   rhs = withFrozenCallStack $ \    a -> lhs     a === rhs     a
lhs ===..  rhs = withFrozenCallStack $ \  b a -> lhs   b a === rhs   b a
lhs ===... rhs = withFrozenCallStack $ \c b a -> lhs c b a === rhs c b a

infix 4 ===.
infix 4 ===..
infix 4 ===...


type MaterialTest m a t u = (MonadTest m, Show a, Ord a, Foldable t, Foldable u, HasCallStack)

-- | Fail the test if the two 'Foldable's are not equal up to ordering.
(<==>) :: MaterialTest m a t u => t a -> u a -> m ()
(<==>) xs ys = L.sort (toList xs) ===
               L.sort (toList ys)

-- | Fail the test if the two 'Foldable's obtained by applying each function to the value do not return results that are equal up to ordering.
(<==>.)  :: MaterialTest m r t u => (   a->t r) -> (   a->u r)     ->a->m ()
(<==>..) :: MaterialTest m r t u => (b->a->t r) -> (b->a->u r) -> b->a->m ()

lhs <==>.  rhs = withFrozenCallStack $ \  a -> lhs   a <==> rhs   a
lhs <==>.. rhs = withFrozenCallStack $ \b a -> lhs b a <==> rhs b a

infix 4 <==>.
infix 4 <==>..

{- | Fail the test if an action does not produce the same value when evaluated twice.
-}
deterministic :: (MonadTest m, MonadIO m, Eq a, Show a, HasCallStack) => IO a -> m ()
deterministic m =
  withFrozenCallStack $
  do
    res0 <- liftIO m
    res1 <- liftIO m
    res0===res1

{- | Fail the test if an effectful function does not return the same value when evaluated twice on the same point of the domain.

The function is evaluated at two different points (provided by the caller). Both are evaluated a first time, before they are then evaluated a second time.
-}
deterministicF :: (MonadTest m, MonadIO m, Show a, Eq b, Show b, HasCallStack) =>
  (a -> IO b) -> (a,a) -> m ()
deterministicF f (x0,x1) =
  withFrozenCallStack $
  do
    annotate $ "(x0,x1) is " ++ show (x0,x1)

    f_x0_first  <- f_x0
    f_x1_first  <- f_x1
    f_x0_second <- f_x0
    f_x0_first === f_x0_second

    f_x1_second <- f_x1
    f_x1_first === f_x1_second

  where
    f_x0 = liftIO (f x0)
    f_x1 = liftIO (f x1)

{-| /Constancy of effectful function./

__Fail__ the test if evaluating the function at the two given points results in two values that are __equal__, i.e. if results are not consistent with @f@ being a constant function.

If @x0 == x1@, the test is discarded (@f@ is never evaluated).
-}
constF_M :: (MonadIO m, Eq a, Show a, Show b, Eq b, HasCallStack) =>
     (a -> IO b) -- ^ @f@
  -> (a,a)       -- ^ @(x0,x1)@
  -> PropertyT m ()
constF_M f (x0,x1) =
  withFrozenCallStack $ do
    when (x0==x1) discard
    y0 <- liftIO (f x0)
    y1 <- liftIO (f x1)
    annotate $ "not constant: " ++ show (x0,x1) ++ " --> " ++ show (y0,y1)
    y0 === y1


{- | /Non-constancy of effectful function./

__Succeed__ the test if evaluating the function at the two given points results in two values that are __/un/equal__, i.e. if results are proof that @f@ is a non-constant function. Otherwise, __discard__ the test.

=== Details

If, with @y0@ as the effectful result of @f x0@ and similar for @y1@,

- @y0 /= y1@, __succeed__ the test

    - @f@ has been proven non-constant over @a@

- @y0 == y1@, __discard__ the test

    - constancy of @f@ over @a@ is not yet proven; further points should be tried

Further, if

- @x0 == x1@, __discard__ the test

    - non-constancy cannot be proven with these inputs
    - note: @f@ is not evaluated in this case

The returned property will __fail__ only if it throws an exception.

The test runner will __give up__ if it is unable to prove non-constancy within the discard limit ('withDiscards').

It is likely suitable to let tests running this property succeed the test on the first succeeding property (@'withTests' 1@).
-}
notConstF_M :: (MonadIO m, Eq a, Eq b) =>
     (a -> IO b)  -- ^ @f@
  -> (a,a)        -- ^ @(x0,x1)@
  -> PropertyT m ()
notConstF_M f (x0,x1) =
  withFrozenCallStack
  $ do
    when (x0==x1) discard
    y0 <- liftIO $ (f x0)
    y1 <- liftIO $ (f x1)
    if (y0 /= y1)
      then success  -- non-constancy proved, done
      else discard  -- non-constancy still unknown, caller should keep trying


--- Annotations ---

maybeCollect :: (MonadTest m, Show a, HasCallStack) => Maybe a -> m ()
maybeCollect = (`whenJust` collect)

maybeLabelS :: (MonadTest m, HasCallStack) => Maybe String -> m ()
maybeLabelS = (`whenJust` label . fromString)

labelS :: (MonadTest m, HasCallStack) => String -> m ()
labelS = label . fromString


--- Misc. ---

failWith
  :: (MonadTest m, HasCallStack)
  => String    -- ^ heading
  -> [String]  -- ^ body
  -> m a
failWith heading body =
  withFrozenCallStack $
  fail_with heading body

{-| Run a property test once using "Hedgehog"s 'check'.

If the test succeeds, 'True' is returned.

This function does not suppress output to stdout. If that is desired, functions from "Hedgehog.Utils.MetaTesting" should be used instead.
-}
checkOnce :: MonadIO m => PropertyT IO () -> m Bool
checkOnce x = check . withTests 1 . property $ x
