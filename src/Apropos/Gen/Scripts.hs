module Apropos.Gen.Scripts (
  validatorHash,
  datum,
  datumHash,
) where

import Apropos.Gen (Gen)
import Apropos.Gen.Api (builtinData, builtinByteString)
import Plutus.V1.Ledger.Scripts (
  Datum (Datum),
  DatumHash (DatumHash),
  ValidatorHash (ValidatorHash),
 )

datum :: Gen Datum
datum = do
  bid <- builtinData
  return $ Datum bid

datumHash :: Gen DatumHash
datumHash = do
  bs <- builtinByteString
  return $ DatumHash bs

validatorHash :: Gen ValidatorHash
validatorHash = do
  bs <- builtinByteString
  return $ ValidatorHash bs
