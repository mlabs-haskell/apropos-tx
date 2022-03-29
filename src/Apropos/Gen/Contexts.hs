{-# OPTIONS_GHC -Wwarn #-}

module Apropos.Gen.Contexts (scriptContext) where

import Apropos.Gen (Gen, choice)
import Plutus.V1.Ledger.Contexts (
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting, Rewarding, Spending, Certifying),
  TxInfo,
  TxOutRef,
 )
import Plutus.V1.Ledger.Value (CurrencySymbol)
import Plutus.V1.Ledger.Credential (StakingCredential)

scriptContext :: Gen ScriptContext
scriptContext = do
  i <- txInfo
  p <- scriptPurpose
  return $ ScriptContext i p

txInfo :: Gen TxInfo
txInfo = undefined

scriptPurpose :: Gen ScriptPurpose
scriptPurpose = do
  c <- currencySymbol
  t <- txOutRef
  s <- undefined
  d <- undefined
  choice
    [ return $ Minting c
    , return $ Spending t
    , return $ Rewarding s
    , return $ Certifying d
    ]

currencySymbol :: Gen CurrencySymbol
currencySymbol = undefined

txOutRef :: Gen TxOutRef
txOutRef = undefined

stakingCredential :: Gen StakingCredential
stakingCredential = undefined

