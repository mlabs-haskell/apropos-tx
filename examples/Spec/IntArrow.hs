{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.IntArrow (
  intArrowPlutarchTests,
) where

import Apropos
import Apropos.Tx.Arrow
import Apropos.Tx.Arrow.Runner
import Plutarch.Builtin
import Plutarch.Prelude
import Plutus.V1.Ledger.Api (ExCPU (..), ExMemory (..))
import Spec.Int
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

instance HasLogicalModel IntProp Integer where
  satisfiesProperty p i = satisfiesProperty p (fromIntegral i :: Int)

instance HasParameterisedGenerator IntProp Integer where
  parameterisedGenerator s = fromIntegral <$> (parameterisedGenerator s :: Gen Int)

intArrow :: TxArrow s Integer Integer
intArrow =
  TxArrow
    { haskArrow = (10 -)
    , plutarchArrow = plam $ \i -> pdata $ 10 - pfromData i
    }

instance HasMemoryBounds (TxArrow s Integer Integer) Integer where
  memoryBounds _ _ = (ExMemory minBound, ExMemory maxBound)

instance HasCPUBounds (TxArrow s Integer Integer) Integer where
  cpuBounds _ _ = (ExCPU minBound, ExCPU maxBound)

intArrowPlutarchTests :: TestTree
intArrowPlutarchTests =
  testGroup "intArrowPlutarchTests" $
    fromGroup
      <$> [ runArrowTestsWhere intArrow "Plutarch Int Arrow" (Yes :: Formula IntProp)
          ]