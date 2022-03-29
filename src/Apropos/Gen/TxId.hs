module Apropos.Gen.TxId (txId) where

import Apropos.Gen (Gen)
import Apropos.Gen.Api (builtinByteString)
import Plutus.V1.Ledger.TxId (TxId (TxId))

txId :: Gen TxId
txId = do
  bs <- builtinByteString
  return $ TxId bs
