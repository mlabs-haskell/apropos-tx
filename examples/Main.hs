module Main (main) where

import Spec.Int
import Spec.IntPermutationGen
import Spec.Plutarch.CostModel
import Spec.Plutarch.MagicNumber
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ testGroup
        "Int model Hand Written Parameterised Generator"
        [ intGenTests
        , intPureTests
        , intPlutarchTests
        ]
    , testGroup
        "Int model using Permutation Generator"
        [
        intPermutationGenTests
        --, intPermutationGenPureTests -- fails
        --, intPermutationGenPlutarchTests
        -- intPermutationGenSelfTests
        ]
    , testGroup
        "Script As Object"
        [ magicNumberPropGenTests
        , addCostPropGenTests
        , addCostModelPlutarchTests
        ]
    ]
