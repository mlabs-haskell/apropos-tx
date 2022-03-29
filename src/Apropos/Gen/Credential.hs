module Apropos.Gen.Credential (credential, stakingCredential) where

import Apropos.Gen (Gen, element, linear)
import Apropos.Gen.Crypto (pubKeyHash)
import Apropos.Gen.Extra (integer)
import Apropos.Gen.Scripts (validatorHash)
import Plutus.V1.Ledger.Credential (
  Credential (PubKeyCredential, ScriptCredential),
  StakingCredential (StakingHash, StakingPtr),
 )

credential :: Gen Credential
credential = do
  pkh <- pubKeyHash
  vh <- validatorHash
  element [PubKeyCredential pkh, ScriptCredential vh]

stakingCredential :: Gen StakingCredential
stakingCredential = do
  cred <- credential
  p0 <- integer $ linear 0 1000
  p1 <- integer $ linear 0 1000
  p2 <- integer $ linear 0 1000
  element [StakingHash cred, StakingPtr p0 p1 p2]
