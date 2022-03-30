module Apropos.Gen.DCert (dCert) where

import Apropos.Gen (Gen, element, linear)
import Apropos.Gen.Credential (stakingCredential)
import Apropos.Gen.Crypto (pubKeyHash)
import Apropos.Gen.Extra (integer)
import Plutus.V1.Ledger.DCert (
  DCert (
    DCertDelegDeRegKey,
    DCertDelegDelegate,
    DCertDelegRegKey,
    DCertGenesis,
    DCertMir,
    DCertPoolRegister,
    DCertPoolRetire
  ),
 )

dCert :: Gen DCert
dCert = do
  sc <- stakingCredential
  pkh <- pubKeyHash
  pkh' <- pubKeyHash
  n <- integer (linear 0 300)
  element
    [ DCertDelegRegKey sc
    , DCertDelegDeRegKey sc
    , DCertDelegDelegate sc pkh
    , DCertPoolRegister pkh pkh'
    , DCertPoolRetire pkh n
    , DCertGenesis
    , DCertMir
    ]
