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

intConstraint :: TxConstraint s Integer
intConstraint =
  TxConstraint
    { haskConstraint = (9 <)
    , plutarchConstraint = plam $ \i -> popaque (pif (9 #< pfromData i) (pcon PUnit) perror)
    }

instance HasMemoryBounds (TxConstraint s Integer) Integer where
  memoryBounds _ _ = (ExMemory minBound, ExMemory maxBound)

instance HasCPUBounds (TxConstraint s Integer) Integer where
  cpuBounds _ _ = (ExCPU minBound, ExCPU maxBound)

intConstraintPlutarchTests :: TestTree
intConstraintPlutarchTests =
  testGroup "intConstraintPlutarchTests" $
    fromGroup
      <$> [ runConstraintTestsWhere intConstraint "Plutarch Int Constraint" (Yes :: Formula IntProp)
          ]
