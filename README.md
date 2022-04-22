# `apropos-tx`
Plutus and Plutarch extensions for [apropos](https://github.com/mlabs-haskell/apropos).

## Modelling Script behaviour with ScriptModel

A logical model that can describe when a script should validate/error can be used to test a Script. This example uses `Plutarch` to create the script inline but the `ScriptModel` class is language agnostic and will work with any means of creating a Plutus Script. The `script` function requires only that you can convert the type (in this case `Int`) to a `Script`.

```Haskell
instance ScriptModel IntProp Int where
  expect _ = Var IsSmall :&&: Var IsNegative
  script _ i =
    let ii = fromIntegral i :: Integer
     in compile (pif ((fromInteger ii #< (0 :: Term s PInteger)) #&& ((fromInteger (-10) :: Term s PInteger) #<= fromInteger ii)) (pcon PUnit) perror)

tests :: TestTree
tests =
  testGroup "intPermutationGenPlutarchTests" $
    fromGroup
      <$> [ runScriptTestsWhere (Apropos :: Int :+ IntProp) "AcceptsSmallNegativeInts" Yes
          ]

```


## What is this?
For more information please see the `examples` directory.

The code is licensed under Apache 2.0; check the LICENSE.md file for details.

## Discussion

To discuss the `apropos-tx` project, [join our Discord](https://discord.gg/Yfd3W66Cp5)!
