{-# LANGUAGE TypeFamilies #-}

module Spec.Int (HasProperties (..), IntProp(..),intGenTests,intDeviceTests,intPlutarchTests) where
import Proper.HasProperties
import Proper.Proposition
import Proper.HasParameterisedGenerator
import Proper.IsPlutusModel
import Proper.IsDeviceModel
import SAT.MiniSat ( Formula (..) )
import Hedgehog (forAll)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear)
import Data.Proxy (Proxy(..))
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)

import Plutarch (compile)
import Plutarch.Prelude

data IntProp =
      IsNegative
    | IsPositive
    | IsZero
    | IsLarge
    | IsSmall
    | IsMaxBound
    | IsMinBound
    deriving stock (Eq,Ord,Enum,Show,Bounded)

instance Proposition IntProp where
  logic = ExactlyOne [Var IsNegative, Var IsPositive, Var IsZero]
     :&&: ExactlyOne [Var IsLarge, Var IsSmall]
     :&&: (Var IsZero :->: Var IsSmall)
     :&&: (Var IsMaxBound :->: (Var IsLarge :&&: Var IsPositive))
     :&&: (Var IsMinBound :->: (Var IsLarge :&&: Var IsNegative))

instance HasProperties Int IntProp where
  satisfiesProperty i IsNegative = i < 0
  satisfiesProperty i IsPositive = i > 0
  satisfiesProperty i IsMaxBound = i == maxBound
  satisfiesProperty i IsMinBound = i == minBound
  satisfiesProperty i IsZero     = i == 0
  satisfiesProperty i IsLarge    = i > 10 || i < -10
  satisfiesProperty i IsSmall    = i <= 10 && i >= -10

instance HasParameterisedGenerator Int IntProp where
  parameterisedGenerator s = forAll $ do
    i <- if IsZero `elem` s
           then pure 0
           else if IsSmall `elem` s
                  then Gen.int (linear 1 10)
                  else if IsMaxBound `elem` s
                         then pure maxBound
                         else Gen.int (linear 11 (maxBound -1))
    if IsNegative `elem` s
       then if IsMinBound `elem` s
               then pure minBound
               else pure (-i)
       else pure i

intGenTests :: TestTree
intGenTests = testGroup "Spec.Int" $
    fromGroup <$> [
      runGeneratorTestsWhere (Proxy :: Proxy Int) "Int Generator" (Yes :: Formula IntProp)
    ]

acceptsSmallNegativeInts :: Device Int IntProp
acceptsSmallNegativeInts = Device (Var IsSmall :&&: Var IsNegative)
                                  (\i -> i < 0 && i >= -10)

instance IsDeviceModel Int IntProp

intDeviceTests :: TestTree
intDeviceTests = testGroup "Device.AcceptsSmallNegativeInts" $
  fromGroup <$> [
    runDeviceTestsWhere acceptsSmallNegativeInts "AcceptsSmallNegativeInts" Yes
  ]

instance IsPlutusModel Int IntProp where
  expect _ _ = Var IsSmall :&&: Var IsNegative
  script _ i =
    let ii = (fromIntegral i) :: Integer
     in compile (pif (((fromInteger ii) #< ((fromInteger 0) :: Term s PInteger)) #&& (((fromInteger (-10)) :: Term s PInteger) #<= (fromInteger ii))) (pcon PUnit) perror)

intPlutarchTests :: TestTree
intPlutarchTests = testGroup "Plutarch.AcceptsSmallNegativeInts" $
  fromGroup <$> [
    runScriptTestsWhere (Proxy :: Proxy Int) (Proxy :: Proxy IntProp) "AcceptsSmallNegativeInts" Yes
  ]

