module Spec.IntPermutationGen (
  intPermutationGenTests,
  intPermutationGenPureTests,
  intPermutationGenPlutarchTests,
  intPermutationGenSelfTests,
  IntProp (..),
) where

import Apropos
import Apropos.Script
import Apropos.LogicalModel
import Plutarch (compile)
import Plutarch.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data IntProp
  = IsNegative
  | IsPositive
  | IsZero
  | IsLarge
  | IsSmall
  | IsMaxBound
  | IsMinBound
  deriving stock (Eq, Ord, Enum, Show, Bounded, Generic)
  deriving anyclass (Enumerable, Hashable)

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

instance HasPermutationGenerator (Prop IntProp) Int where
  sources =
    [ Source
        { sourceName = "MakeZero"
        , covers = Prop <$> Var IsZero :&&: Var IsSmall
        , gen = pure 0
        }
    , Source
        { sourceName = "MakeMaxBound"
        , covers = Prop <$> Var IsMaxBound :&&: Var IsLarge :&&: Var IsPositive
        , gen = pure maxBound
        }
    , Source
        { sourceName = "MakeMinBound"
        , covers = Prop <$> Var IsMinBound :&&: Var IsLarge :&&: Var IsNegative
        , gen = pure minBound
        }
    , Source
        { sourceName = "MakeLarge"
        , covers = Prop <$> Var IsLarge :&&: Var IsPositive :&&: Not (Var IsMaxBound :||: Var IsMinBound)
        , gen = int (linear 11 (maxBound - 1))
        }
    , Source
        { sourceName = "MakeSmall"
        , covers = Prop <$> Var IsSmall :&&: Var IsPositive
        , gen = int (linear 1 10)
        }
    ]

  generators =
    [ Morphism
        { name = "Negate"
        , match = Not $ Var (Prop IsZero)
        , contract =
            branches
              [ has (Prop IsNegative) >> remove (Prop IsNegative) >> add (Prop IsPositive)
              , has (Prop IsPositive) >> remove (Prop IsPositive) >> add (Prop IsNegative)
              ]
        , morphism = \i -> pure (-i)
        }
    ]

instance HasParameterisedGenerator (Prop IntProp) Int where
  parameterisedGenerator = buildGen @(Prop IntProp)

intPermutationGenTests :: TestTree
intPermutationGenTests =
  testGroup "intPermutationGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere @(Prop IntProp) "Int Generator" Yes
          ]

intPureRunner :: PureRunner (Prop IntProp) Int
intPureRunner =
  PureRunner
    { expect = Prop <$> Var IsSmall :&&: Var IsNegative
    , script = \i -> i < 0 && i >= -10
    }

intPermutationGenPureTests :: TestTree
intPermutationGenPureTests =
  testGroup "intPermutationGenPureTests" $
    fromGroup
      <$> [ runPureTestsWhere intPureRunner "AcceptsSmallNegativeInts" (Yes @(Prop IntProp))
          ]

instance ScriptModel (Prop IntProp) Int where
  expect = Prop <$> Var IsSmall :&&: Var IsNegative
  script i =
    let ii = fromIntegral i :: Integer
     in compile (pif ((fromInteger ii #< (0 :: Term s PInteger)) #&& ((fromInteger (-10) :: Term s PInteger) #<= fromInteger ii)) (pcon PUnit) perror)

intPermutationGenPlutarchTests :: TestTree
intPermutationGenPlutarchTests =
  testGroup "intPermutationGenPlutarchTests" $
    fromGroup
      <$> [ runScriptTestsWhere @(Prop IntProp) "AcceptsSmallNegativeInts" Yes
          ]

intPermutationGenSelfTests :: TestTree
intPermutationGenSelfTests =
  testGroup "intPermutationGenSelfTests" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(Prop IntProp)
