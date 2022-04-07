{- |
Module: Apropos.Gen.Crypto
Description: Plutus crypto type generators.
Maintainer: jack@mlabs.city

`Gen`s for `Plutus.V1.Ledger.Crypto` types.
-}
module Apropos.Gen.Crypto (pubKeyHash) where

import Apropos.Gen (Gen)
import Apropos.Gen.Api (builtinByteString)
import Plutus.V1.Ledger.Crypto (PubKeyHash (PubKeyHash))

-- | `Gen` for Plutus `PubKeyHash`s.
pubKeyHash :: Gen PubKeyHash
pubKeyHash = do
  bs <- builtinByteString
  return $ PubKeyHash bs
