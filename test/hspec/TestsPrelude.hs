module TestsPrelude
(
  Prop
, unitTests
, it
, xt

  -- * re-exports
, module Test.Hspec
, module Test.Hspec.Hedgehog
, module Data.Kind
, module Control.Monad.IO.Class
, module Control.Monad
, module Control.Exception
, module GHC.Stack

) where


--- for re-export ---

import Test.Hspec hiding (it)
import Test.Hspec.Hedgehog

import Data.Kind
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception hiding (assert)
import GHC.Stack


--- for internal use ---

import Test.Hspec qualified as Hspec


--- identifiers ---

type Prop = PropertyT IO ()

unitTests :: Spec -> Spec
unitTests =
    modifyMaxShrinks (const 0)
  . modifyMaxSuccess (const 1)

it :: HasCallStack => String -> Prop -> Spec
it desc = Hspec.it desc . hedgehog

xt :: HasCallStack => String -> Prop -> Spec
xt desc = Hspec.xit desc . hedgehog
