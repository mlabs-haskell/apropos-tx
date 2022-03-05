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


intArrowA :: TxArrow s Integer Integer
intArrowA = TxArrow {
             haskArrow = \i -> i + 10
           , plutarchArrow = plam $ \i -> pdata $ (pfromData i) + 10
           }

intArrowA' :: TxArrow s Integer Integer
intArrowA' = TxArrow {
             haskArrow = \i -> i - 10
           , plutarchArrow = plam $ \i -> pdata $ (pfromData i) - 10
           }

intArrowB :: TxArrow s Integer Integer
intArrowB = TxArrow {
             haskArrow = \i -> i + 5
           , plutarchArrow = plam $ \i -> pdata $ (pfromData i) + 5
           }

intArrowB' :: TxArrow s Integer Integer
intArrowB' = TxArrow {
             haskArrow = \i -> i - 5
           , plutarchArrow = plam $ \i -> pdata $ (pfromData i) - 5
           }


intConstraintA :: TxConstraint s Integer
intConstraintA = (intArrowA &&&& intArrowB) >>>| txNeq


intConstraintB :: TxConstraint s Integer
intConstraintB = (intArrowA &&&& intArrowB) >>>> (intArrowA' <++> intArrowB') >>>| txEq

-- this is not a good way to do the memoryBounds - they should just be args
instance HasMemoryBounds (TxConstraint s Integer) Integer where
  memoryBounds _ _ = (ExMemory minBound, ExMemory maxBound)

instance HasCPUBounds (TxConstraint s Integer) Integer where
  cpuBounds _ _ = (ExCPU minBound, ExCPU maxBound)

intArrowConstraintPlutarchTests :: TestTree
intArrowConstraintPlutarchTests =
  testGroup "intArrowConstraintPlutarchTests" $
    fromGroup <$> [ runConstraintTestsWhere intConstraintA "Plutarch Int Constraint A" (Yes :: Formula IntProp)
                  , runConstraintTestsWhere intConstraintB "Plutarch Int Constraint B" (Yes :: Formula IntProp)

                  ]

