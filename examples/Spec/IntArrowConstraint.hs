{-# OPTIONS_GHC -fno-warn-orphans #-}
module Spec.IntArrowConstraint (
  intArrowConstraintPlutarchTests,
) where
import Spec.Int
import Apropos
import Apropos.Tx.Arrow
import Apropos.Tx.Constraint
import Apropos.Tx.Constraint.Runner
import Plutarch.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Plutus.V1.Ledger.Api (ExCPU (..), ExMemory (..))

instance HasLogicalModel IntProp Integer where
  satisfiesProperty p i = satisfiesProperty p (fromIntegral i :: Int)

instance HasParameterisedGenerator IntProp Integer where
  parameterisedGenerator s = fromIntegral <$> (parameterisedGenerator s :: Gen Int)


intArrow :: TxArrow s Integer Integer
intArrow = TxArrow {
             haskArrow = \i -> 10 - i
           , plutarchArrow = plam $ \i -> pdata $ 10 - (pfromData i)
           }

intArrowB :: TxArrow s Integer Integer
intArrowB = TxArrow {
             haskArrow = \i -> 5 + i
           , plutarchArrow = plam $ \i -> pdata $ 5 + (pfromData i)
           }

intConstraint :: TxConstraint s Integer
intConstraint = (intArrow &&&& intArrowB) >>>| txNeq

instance HasMemoryBounds (TxConstraint s Integer) Integer where
  memoryBounds _ _ = (ExMemory minBound, ExMemory maxBound)

instance HasCPUBounds (TxConstraint s Integer) Integer where
  cpuBounds _ _ = (ExCPU minBound, ExCPU maxBound)

intArrowConstraintPlutarchTests :: TestTree
intArrowConstraintPlutarchTests =
  testGroup "intArrowConstraintPlutarchTests" $
    fromGroup <$> [ runConstraintTestsWhere intConstraint "Plutarch Int Constraint" (Yes :: Formula IntProp)
                  ]

