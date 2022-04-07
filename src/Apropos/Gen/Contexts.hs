{- |
Module: Apropos.Gen.Context
Description: Plutus context generators.
Maintainer: jack@mlabs.city

`Gen`s for `Plutus.V1.Ledger.Context` types.
-}
module Apropos.Gen.Contexts (
  scriptContext,
  txInfo,
  txInInfo,
  txOut,
  scriptPurpose,
  txOutRef,
) where

import Apropos.Gen (Gen, element, linear, list)
import Apropos.Gen.Address (address)
import Apropos.Gen.Credential (stakingCredential)
import Apropos.Gen.Crypto (pubKeyHash)
import Apropos.Gen.DCert (dCert)
import Apropos.Gen.Extra (integer, pair)
import Apropos.Gen.Extra qualified as Gen (maybe)
import Apropos.Gen.Scripts (datum, datumHash)
import Apropos.Gen.Time (posixTimeRange)
import Apropos.Gen.TxId (txId)
import Apropos.Gen.Value (currencySymbol, value)
import Plutus.V1.Ledger.Contexts (
  ScriptContext (ScriptContext),
  ScriptPurpose (Certifying, Minting, Rewarding, Spending),
  TxInInfo (TxInInfo),
  TxInfo (
    TxInfo,
    txInfoDCert,
    txInfoData,
    txInfoFee,
    txInfoId,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoSignatories,
    txInfoValidRange,
    txInfoWdrl
  ),
  TxOut (TxOut),
  TxOutRef (TxOutRef),
 )

-- | `Gen` for Plutus `ScriptContext`s.
scriptContext :: Gen ScriptContext
scriptContext = do
  i <- txInfo
  p <- scriptPurpose
  return $ ScriptContext i p

-- | `Gen` for Plutus `TxInfo`s.
txInfo :: Gen TxInfo
txInfo = do
  ins <- list (linear 1 5) txInInfo
  outs <- list (linear 1 5) txOut
  fee <- value
  mint <- value
  dCert' <- list (linear 1 10) dCert
  wdrl <-
    list (linear 0 10) $
      pair stakingCredential $
        integer (linear 0 50)
  range <- posixTimeRange (linear 0 2000) (linear 6000 maxBound)
  sigs <- list (linear 0 10) pubKeyHash
  data' <- list (linear 1 5) $ pair datumHash datum
  id' <- txId
  return $
    TxInfo
      { txInfoInputs = ins
      , txInfoOutputs = outs
      , txInfoFee = fee
      , txInfoMint = mint
      , txInfoDCert = dCert'
      , txInfoWdrl = wdrl
      , txInfoValidRange = range
      , txInfoSignatories = sigs
      , txInfoData = data'
      , txInfoId = id'
      }

-- | `Gen` for Plutus `TxInInfo`s.
txInInfo :: Gen TxInInfo
txInInfo = do
  oRef <- txOutRef
  o <- txOut
  return $ TxInInfo oRef o

-- | `Gen` for Plutus `TxOut`s.
txOut :: Gen TxOut
txOut = do
  a <- address
  v <- value
  h <- Gen.maybe datumHash
  return $ TxOut a v h

-- | `Gen` for Plutus `ScriptPurpose`s.
scriptPurpose :: Gen ScriptPurpose
scriptPurpose = do
  c <- currencySymbol
  t <- txOutRef
  s <- stakingCredential
  d <- dCert
  element
    [ Minting c
    , Spending t
    , Rewarding s
    , Certifying d
    ]

-- | `Gen` for Plutus `TxOutRef`s.
txOutRef :: Gen TxOutRef
txOutRef = do
  id' <- txId
  idx <- integer (linear 0 maxBound)
  return $ TxOutRef id' idx
