{- |
Module: Apropos.Gen.Value
Description: Plutus value generators.
Maintainer: jack@mlabs.city

`Gen`s for `Plutus.V1.Ledger.Value` types.
-}
module Apropos.Gen.Value (
  ada,
  value,
  valueWithAda,
  currencySymbol,
  tokenName,
) where

import Apropos.Gen (Gen, linear)
import Apropos.Gen.Api (builtinByteString)
import Apropos.Gen.Extra (integer)
import Apropos.Gen.Extra qualified as Gen (map)
import Apropos.Gen.Range (Range)
import Plutus.V1.Ledger.Value (
  CurrencySymbol (CurrencySymbol),
  TokenName (TokenName),
  Value (Value),
  unionWith,
 )
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as AssocMap (singleton)

-- | Generator for some amount of Ada
ada :: Range -> Gen Value
ada r = do
  n <- integer r
  let m :: Map CurrencySymbol (Map TokenName Integer) =
        AssocMap.singleton (CurrencySymbol "") $
          AssocMap.singleton (TokenName "") n
  return $ Value m

-- | Generator for a Plutus `Value`.
value :: Gen Value
value = do
  let tnvMap = Gen.map (linear 1 5) tokenName $ integer (linear 1 maxBound)
  v <- Gen.map (linear 1 5) currencySymbol tnvMap
  return $ Value v

{- | Generator for a Plutus `Value`. Guaranteed to contain some
     Ada.
-}
valueWithAda :: Gen Value
valueWithAda = do
  v <- value
  a <- ada $ linear 1 1000
  return $ unionWith (+) v a

-- | Generator for a Plutus `CurrencySymbol`.
currencySymbol :: Gen CurrencySymbol
currencySymbol = do
  bs <- builtinByteString
  return $ CurrencySymbol bs

-- | Generator for a Plutus `TokenName`.
tokenName :: Gen TokenName
tokenName = do
  bs <- builtinByteString
  return $ TokenName bs
