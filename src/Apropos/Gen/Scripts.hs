module Apropos.Gen.Scripts (
  validatorHash,
  datum,
  datumHash,
) where

import Apropos.Gen (Gen)
import Apropos.Gen.Api (builtinByteString)
import Plutus.V1.Ledger.Scripts (Datum, DatumHash, ValidatorHash (ValidatorHash))

datum :: Gen Datum
datum = undefined

datumHash :: Gen DatumHash
datumHash = undefined

validatorHash :: Gen ValidatorHash
validatorHash = do
  bs <- builtinByteString
  return $ ValidatorHash bs
