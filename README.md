# `apropos-tx`
Plutus and Plutarch extensions for [apropos](https://github.com/mlabs-haskell/apropos).

Protocol Specifications as compositions of Arrows and Constraints.

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

## Arrow and Constraint Isos

This API under `Apropos.Script.Iso` can be used in addition to `ScriptModel` and provides tools for structuring `Plutarch` scripts as Arrows and Constraints. These Arrows and Constraints are composable Isomorphisms between `Plutarch` and `Haskell` that allow specifications to be composed with implementations. The `Haskell` part of a Cosntraint or Arrow provides the specification against which the `Plutarch` instance is tested.


This is an excerpt from `examples/Spec/IntArrowConstraint.hs`
```Haskell
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
intConstraintB = (intArrowA &&&& intArrowB) >>>> (intArrowA' <++> intArrowB') >>>| constraintEq

```

Arrows and Constraints can be "wired together" with combinators. Test runners for Arrows and Constraints provide a means of granularly testing each composition. By testing in this way you can build your test suite as you build your application. It is recommended to also build logical models for the script behaviour with `apropos` since there may be bugs in the logic of the Arrows and Constraints not caught by the Isomorphism tests - since the Arrows and Constraints are tested to be Isomorphic the logical model for behaviour can test either the `Haskell` implementation or the `Plutus` implementation.

## Protocols

Under construction... An API for modelling protocols as Transaction Arrows such that protocol logic can be verified.


## What is this?
For more information please see the `examples` directory.

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
