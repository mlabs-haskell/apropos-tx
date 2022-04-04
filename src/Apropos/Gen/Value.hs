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

value :: Gen Value
value = do
  let tnvMap = Gen.map (linear 1 5) tokenName $ integer (linear 1 maxBound)
  v <- Gen.map (linear 1 5) currencySymbol tnvMap
  return $ Value v

currencySymbol :: Gen CurrencySymbol
currencySymbol = do
  bs <- builtinByteString
  return $ CurrencySymbol bs

tokenName :: Gen TokenName
tokenName = do
  bs <- builtinByteString
  return $ TokenName bs
