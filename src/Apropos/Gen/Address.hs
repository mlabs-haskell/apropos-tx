module Apropos.Gen.Address (address) where

import Apropos.Gen (Gen)
import Apropos.Gen.Credential (credential, stakingCredential)
import Apropos.Gen.Extra qualified as Gen (maybe)
import Plutus.V1.Ledger.Address (
  Address (Address),
 )

address :: Gen Address
address = do
  cred <- credential
  scred <- Gen.maybe stakingCredential
  return $ Address cred scred
