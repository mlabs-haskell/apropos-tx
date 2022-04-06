{- |
Module: Apropos.Gen.Api
Description: Misc. Plutus generators.
Maintainer: jack@mlabs.city

`Gen`s for `Plutus.V1.Ledger.Api` types.
-}
module Apropos.Gen.Api (
  builtinByteString,
  builtinData,
) where

import Apropos.Gen (Gen)
import Apropos.Gen.Extra (sha256)
import PlutusTx (Data)
import PlutusTx.Builtins.Internal (
  BuiltinByteString (BuiltinByteString),
  BuiltinData (BuiltinData),
 )

-- | `Gen` for Plutus `BuiltinByteString`.
builtinByteString :: Gen BuiltinByteString
builtinByteString = do
  bs <- sha256
  return $ BuiltinByteString bs

-- | `Gen` for Plutus `BuiltinData`.
builtinData :: Gen BuiltinData
builtinData = do
  d <- data'
  return $ BuiltinData d

-- | TODO: Finish.
data' :: Gen Data
data' = do
  undefined
