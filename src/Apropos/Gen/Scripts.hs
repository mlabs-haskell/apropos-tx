{- |
Module: Apropos.Gen.Scripts
Description: Script generators.
Maintainer: jack@mlabs.city

`Gen`s for `Plutus.V1.Ledger.Scripts` types.
-}
module Apropos.Gen.Scripts (
  validatorHash,
  datum,
  datumHash,
) where

import Apropos.Gen (Gen)
import Apropos.Gen.Api (builtinByteString, builtinData)
import Plutus.V1.Ledger.Scripts (
  Datum (Datum),
  DatumHash (DatumHash),
  ValidatorHash (ValidatorHash),
 )

-- | Generator for Plutus `Datum` type.
datum :: Gen Datum
datum = do
  bid <- builtinData
  return $ Datum bid

-- | Generator for Plutus `DatumHash` type.
datumHash :: Gen DatumHash
datumHash = do
  bs <- builtinByteString
  return $ DatumHash bs

-- | Generator for Plutus `validatorHash` type.
validatorHash :: Gen ValidatorHash
validatorHash = do
  bs <- builtinByteString
  return $ ValidatorHash bs
