{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.IntArrowConstraint (
  intArrowConstraintPlutarchTests,
) where

import Apropos
import Apropos.Script.Iso.Arrow
import Apropos.Script.Iso.Constraint
import Apropos.Script.Iso.Constraint.Runner
import Control.Category ((>>>))
import Control.Category qualified as Cat
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
intConstraintA = (intArrowA &&&& intArrowB) >>>| constraintNeq

intConstraintB :: IsoConstraint s Integer
intConstraintB = ((intArrowA &&&& intArrowB) >>> (intArrowA' <++> intArrowB')) >>>| constraintEq

intConstraintC :: IsoConstraint s Integer
intConstraintC =
  ( (Cat.id &&&& Cat.id)
      >>> ((intArrowA &&&& intArrowB) <++> (intArrowA &&&& intArrowB))
      >>> ((intArrowA' <++> intArrowB') <++> (intArrowA' <++> intArrowB'))
  )
    >>>| constraintEq

intConstraintA' :: IsoConstraint s Integer
intConstraintA' = (intArrowA &&&& intArrowB) >>>| constraintEq

intConstraintB' :: IsoConstraint s Integer
intConstraintB' = ((intArrowA &&&& intArrowB) >>> (intArrowA' <++> intArrowB')) >>>| constraintNeq

intConstraintC' :: IsoConstraint s Integer
intConstraintC' =
  ( (Cat.id &&&& Cat.id)
      >>> ((intArrowA &&&& intArrowB) <++> (intArrowA &&&& intArrowB))
      >>> ((intArrowA' <++> intArrowB') <++> (intArrowA' <++> intArrowB'))
  )
    >>>| constraintNeq

-- this is not a good way to do the memoryBounds
-- we want them to be parameterised by the type but there is probably a better way
instance HasMemoryBounds (IsoConstraint s Integer) Integer where
  memoryBounds _ _ = (ExMemory minBound, ExMemory maxBound)

instance HasCPUBounds (IsoConstraint s Integer) Integer where
  cpuBounds _ _ = (ExCPU minBound, ExCPU maxBound)

intArrowConstraintPlutarchTests :: TestTree
intArrowConstraintPlutarchTests =
  testGroup "intArrowConstraintPlutarchTests" $
    fromGroup
      <$> [ runConstraintTestsWhere Yes intConstraintA "Plutarch Int Constraint A" (Yes :: Formula IntProp)
          , runConstraintTestsWhere Yes intConstraintB "Plutarch Int Constraint B" (Yes :: Formula IntProp)
          , runConstraintTestsWhere Yes intConstraintC "Plutarch Int Constraint C" (Yes :: Formula IntProp)
          , runConstraintTestsWhere No intConstraintA' "Plutarch Int Constraint A'" (Yes :: Formula IntProp)
          , runConstraintTestsWhere No intConstraintB' "Plutarch Int Constraint B'" (Yes :: Formula IntProp)
          , runConstraintTestsWhere No intConstraintC' "Plutarch Int Constraint C'" (Yes :: Formula IntProp)
          ]
