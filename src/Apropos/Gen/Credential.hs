{- |
Module: Apropos.Gen.Credential
Description: Plutus credential generators.
Maintainer: jack@mlabs.city

`Gen`s for `Plutus.V1.Ledger.Credential` types.
-}
module Apropos.Gen.Credential (credential, stakingCredential) where

import Apropos.Gen (Gen, choice, linear)
import Apropos.Gen.Crypto (pubKeyHash)
import Apropos.Gen.Extra (integer)
import Apropos.Gen.Scripts (validatorHash)
import Plutus.V1.Ledger.Credential (
  Credential (PubKeyCredential, ScriptCredential),
  StakingCredential (StakingHash, StakingPtr),
 )

-- | `Gen` for Plutus `Credential`s.
credential :: Gen Credential
credential =
  choice
    [ PubKeyCredential <$> pubKeyHash
    , ScriptCredential <$> validatorHash
    ]

-- | `Gen` for Plutus `StakingCredential`s.
stakingCredential :: Gen StakingCredential
stakingCredential =
  let p = integer $ linear 0 1000
   in choice
        [ StakingHash <$> credential
        , StakingPtr <$> p <*> p <*> p
        ]
