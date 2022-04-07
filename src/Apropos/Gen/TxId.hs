{- |
Module: Apropos.Gen.TxId
Description: Plutus credential generators.
Maintainer: jack@mlabs.city

`Gen`s for `Plutus.V1.Ledger.TxId` types.
-}
module Apropos.Gen.TxId (txId) where

import Apropos.Gen (Gen)
import Apropos.Gen.Api (builtinByteString)
import Plutus.V1.Ledger.TxId (TxId (TxId))

-- | Generator for Plutus `TxId` type.
txId :: Gen TxId
txId = do
  bs <- builtinByteString
  return $ TxId bs
