{- |
Module: Apropos.Gen.Address
Description: Address generators.
Maintainer: jack@mlabs.city

`Gen`s for `Plutus.V1.Ledger.Address` types.
-}
module Apropos.Gen.Address (address) where

import Apropos.Gen (Gen)
import Apropos.Gen.Credential (credential, stakingCredential)
import Apropos.Gen.Extra qualified as Gen (maybe)
import Plutus.V1.Ledger.Address (
  Address (Address),
 )

-- | Generator for Plutus `Address` types.
address :: Gen Address
address = do
  cred <- credential
  scred <- Gen.maybe stakingCredential
  return $ Address cred scred
