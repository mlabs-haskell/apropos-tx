module Apropos.Gen.Crypto (pubKeyHash) where

import Apropos.Gen (Gen)
import Apropos.Gen.Api (builtinByteString)
import Plutus.V1.Ledger.Crypto (PubKeyHash (PubKeyHash))

pubKeyHash :: Gen PubKeyHash
pubKeyHash = do
  bs <- builtinByteString
  return $ PubKeyHash bs
