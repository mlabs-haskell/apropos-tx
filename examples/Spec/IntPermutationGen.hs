module Spec.IntPermutationGen (
  intPermutationGenTests,
  intPermutationGenPureTests,
  intPermutationGenPlutarchTests,
  intPermutationGenSelfTests,
  intPermutationScriptModel,
  intPermutationPureRunner,
  IntProp (..),
) where

import Apropos
import Apropos.Script
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

instance HasPermutationGenerator IntProp Int where
  sources =
    [ Source
        { sourceName = "Zero"
        , covers = Var IsZero
        , gen = pure 0
        }
    , Source
        { sourceName = "MaxBound"
        , covers = Var IsMaxBound
        , gen = pure maxBound
        }
    , Source
        { sourceName = "MinBbound"
        , covers = Var IsMinBound
        , gen = pure minBound
        }
    , Source
        { sourceName = "Large"
        , covers = Var IsLarge :&&: Var IsPositive :&&: Not (Var IsMaxBound)
        , gen = int (linear 11 (maxBound - 1))
        }
    , Source
        { sourceName = "Small"
        , covers = Var IsSmall :&&: Var IsPositive
        , gen = int (linear 1 10)
        }
    ]
  generators =
    [ Morphism
        { name = "Negate"
        , match = Not $ Var IsZero
        , contract =
            branches
              [ has IsNegative >> remove IsNegative >> add IsPositive
              , has IsPositive >> remove IsPositive >> add IsNegative
              ]
        , morphism = \i -> pure (-i)
        }
    ]

instance HasParameterisedGenerator IntProp Int where
  parameterisedGenerator = buildGen

intPermutationGenTests :: TestTree
intPermutationGenTests =
  testGroup "intPermutationGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere @IntProp "Int Generator" Yes
          ]
intPermutationPureRunner :: PureRunner IntProp Int
intPermutationPureRunner =
  PureRunner
    { expect = Var IsSmall :&&: Var IsNegative
    , script = \i -> i < 0 && i >= -10
    }

intPermutationGenPureTests :: TestTree
intPermutationGenPureTests =
  testGroup "intPermutationGenPureTests" $
    fromGroup
      <$> [ runPureTestsWhere intPermutationPureRunner "AcceptsSmallNegativeInts" Yes
          ]

intPermutationScriptModel :: ScriptModel IntProp Int
intPermutationScriptModel =
  ignoreBoundsScriptModel
    { expect = Var IsSmall :&&: Var IsNegative
    , script = \i ->
        let ii = fromIntegral i :: Integer
         in compile (pif ((fromInteger ii #< (0 :: Term s PInteger)) #&& ((fromInteger (-10) :: Term s PInteger) #<= fromInteger ii)) (pcon PUnit) perror)
    }

intPermutationGenPlutarchTests :: TestTree
intPermutationGenPlutarchTests =
  testGroup "intPermutationGenPlutarchTests" $
    fromGroup
      <$> [ runScriptTestsWhere intPermutationScriptModel "AcceptsSmallNegativeInts" Yes
          ]

intPermutationGenSelfTests :: TestTree
intPermutationGenSelfTests =
  testGroup "intPermutationGenSelfTests" $
    fromGroup
      <$> [permutationGeneratorSelfTest @IntProp]
