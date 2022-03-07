{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.IntConstraint (
  intConstraintPlutarchTests,
) where

import Apropos
import Apropos.Script.Iso.Constraint
import Apropos.Script.Iso.Constraint.Runner
import Plutarch
import Plutarch.Prelude
import Plutus.V1.Ledger.Api (ExCPU (..), ExMemory (..))
import Spec.Int
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

instance HasLogicalModel IntProp Integer where
  satisfiesProperty p i = satisfiesProperty p (fromIntegral i :: Int)

instance HasParameterisedGenerator IntProp Integer where
  parameterisedGenerator s = fromIntegral <$> (parameterisedGenerator s :: Gen Int)

intConstraint :: IsoConstraint s Integer
intConstraint =
  IsoConstraint
    { haskConstraint = \i -> i > 0 && i < 11
    , plutarchConstraint = plam $ \i ->
        popaque
          ( pif
              ((0 #< pfromData i) #&& (pfromData i #< 11))
              (pcon PUnit)
              perror
          )
    }

instance HasMemoryBounds (IsoConstraint s Integer) Integer where
  memoryBounds _ _ = (ExMemory minBound, ExMemory maxBound)

instance HasCPUBounds (IsoConstraint s Integer) Integer where
  cpuBounds _ _ = (ExCPU minBound, ExCPU maxBound)

intConstraintPlutarchTests :: TestTree
intConstraintPlutarchTests =
  testGroup "intConstraintPlutarchTests" $
    fromGroup
      <$> [ runConstraintTestsWhere
              (Var IsSmall :&&: Var IsPositive) -- when the constraint should be SAT
              intConstraint
              "Plutarch Int Constraint"
              (Yes :: Formula IntProp) -- test suite filter
          ]
