{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.IntArrowConstraint (
  intArrowConstraintPlutarchTests,
) where

import Apropos
import Apropos.Script.Iso.Arrow
import Apropos.Script.Iso.Constraint
import Apropos.Script.Iso.Constraint.Runner
import Plutarch.Prelude
import Plutus.V1.Ledger.Api (ExCPU (..), ExMemory (..))
import Spec.Int
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

instance HasLogicalModel IntProp Integer where
  satisfiesProperty p i = satisfiesProperty p (fromIntegral i :: Int)

instance HasParameterisedGenerator IntProp Integer where
  parameterisedGenerator s = fromIntegral <$> (parameterisedGenerator s :: Gen Int)

intArrowA :: IsoArrow s Integer Integer
intArrowA =
  IsoArrow
    { haskArrow = (+ 10)
    , plutarchArrow = plam $ \i -> pdata $ pfromData i + 10
    }

intArrowA' :: IsoArrow s Integer Integer
intArrowA' =
  IsoArrow
    { haskArrow = \i -> i - 10
    , plutarchArrow = plam $ \i -> pdata $ pfromData i - 10
    }

intArrowB :: IsoArrow s Integer Integer
intArrowB =
  IsoArrow
    { haskArrow = (+ 5)
    , plutarchArrow = plam $ \i -> pdata $ pfromData i + 5
    }

intArrowB' :: IsoArrow s Integer Integer
intArrowB' =
  IsoArrow
    { haskArrow = \i -> i - 5
    , plutarchArrow = plam $ \i -> pdata $ pfromData i - 5
    }

intConstraintA :: IsoConstraint s Integer
intConstraintA = (intArrowA &&&& intArrowB) >>>| txNeq

intConstraintB :: IsoConstraint s Integer
intConstraintB = (intArrowA &&&& intArrowB) >>>> (intArrowA' <++> intArrowB') >>>| txEq

-- this is not a good way to do the memoryBounds - they should just be args
instance HasMemoryBounds (IsoConstraint s Integer) Integer where
  memoryBounds _ _ = (ExMemory minBound, ExMemory maxBound)

instance HasCPUBounds (IsoConstraint s Integer) Integer where
  cpuBounds _ _ = (ExCPU minBound, ExCPU maxBound)

intArrowConstraintPlutarchTests :: TestTree
intArrowConstraintPlutarchTests =
  testGroup "intArrowConstraintPlutarchTests" $
    fromGroup
      <$> [ runConstraintTestsWhere intConstraintA "Plutarch Int Constraint A" (Yes :: Formula IntProp)
          , runConstraintTestsWhere intConstraintB "Plutarch Int Constraint B" (Yes :: Formula IntProp)
          ]
