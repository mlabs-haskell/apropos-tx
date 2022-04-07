{- |
Module: Apropos.Gen.Value
Description: Plutus value generators.
Maintainer: jack@mlabs.city

`Gen`s for `Plutus.V1.Ledger.Value` types.
-}
module Apropos.Gen.Value (
  currencySymbol,
  value,
) where

import Apropos.Gen (Gen, linear)
import Apropos.Gen.Api (builtinByteString)
import Apropos.Gen.Extra (integer)
import Apropos.Gen.Extra qualified as Gen (map)
import Plutus.V1.Ledger.Value (
  CurrencySymbol (CurrencySymbol),
  TokenName (TokenName),
  Value (Value),
 )

-- | Generator for a Plutus `Value`.  
value :: Gen Value
value = do
  let tnvMap = Gen.map (linear 1 5) tokenName $ integer (linear 1 maxBound)
  v <- Gen.map (linear 1 5) currencySymbol tnvMap
  return $ Value v

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
