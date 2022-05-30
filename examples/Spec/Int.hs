module Spec.Int (
  HasParameterisedGenerator (..),
  HasLogicalModel (..),
  IntProp (..),
  intGenTests,
  intPureTests,
  intPlutarchTests,
) where

import Apropos
import Apropos.LogicalModel
import Apropos.Script
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

import Plutarch (compile)
import Plutarch.Prelude

data IntProp
  = IsNegative
  | IsPositive
  | IsZero
  | IsLarge
  | IsSmall
  | IsMaxBound
  | IsMinBound
  deriving stock (Eq, Ord, Enum, Show, Bounded)

instance Enumerable IntProp where
  enumerated = [minBound .. maxBound]

instance LogicalModel IntProp where
  logic =
    ExactlyOne [Var IsNegative, Var IsPositive, Var IsZero]
      :&&: ExactlyOne [Var IsLarge, Var IsSmall]
      :&&: (Var IsZero :->: Var IsSmall)
      :&&: (Var IsMaxBound :->: (Var IsLarge :&&: Var IsPositive))
      :&&: (Var IsMinBound :->: (Var IsLarge :&&: Var IsNegative))

instance HasLogicalModel IntProp Int where
  satisfiesProperty IsNegative i = i < 0
  satisfiesProperty IsPositive i = i > 0
  satisfiesProperty IsMaxBound i = i == maxBound
  satisfiesProperty IsMinBound i = i == minBound
  satisfiesProperty IsZero i = i == 0
  satisfiesProperty IsLarge i = i > 10 || i < -10
  satisfiesProperty IsSmall i = i <= 10 && i >= -10

instance HasParameterisedGenerator (Prop IntProp) Int where
  parameterisedGenerator s = do
    i <-
      if IsZero `elem` s
        then pure 0
        else
          if IsSmall `elem` s
            then int (linear 1 10)
            else
              if IsMaxBound `elem` s
                then pure maxBound
                else int (linear 11 (maxBound - 1))
    if IsNegative `elem` s
      then
        if IsMinBound `elem` s
          then pure minBound
          else pure (-i)
      else pure i

intGenTests :: TestTree
intGenTests =
  testGroup "intGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere @(Prop IntProp) "Int Generator" Yes
          ]

intPureRunner :: PureRunner (Prop IntProp) Int
intPureRunner =
  PureRunner
    { expect = Var (Prop IsSmall) :&&: Var (Prop IsNegative)
    , script = \i -> i < 0 && i >= -10
    }

intPureTests :: TestTree
intPureTests =
  testGroup "intPureTests" $
    fromGroup
      <$> [ runPureTestsWhere intPureRunner "AcceptsSmallNegativeInts" (Yes @(Prop IntProp))
          ]

instance ScriptModel (Prop IntProp) Int where
  expect = Var (Prop IsSmall) :&&: Var (Prop IsNegative)
  script i =
    let ii = fromIntegral i :: Integer
     in compile (pif ((fromInteger ii #< (0 :: Term s PInteger)) #&& ((fromInteger (-10) :: Term s PInteger) #<= fromInteger ii)) (pcon PUnit) perror)

intPlutarchTests :: TestTree
intPlutarchTests =
  testGroup "intPlutarchTests" $
    fromGroup
      <$> [ runScriptTestsWhere @(Prop IntProp) "AcceptsSmallNegativeInts" Yes
          ]
