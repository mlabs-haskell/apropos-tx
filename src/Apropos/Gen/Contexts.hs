{-# OPTIONS_GHC -Wwarn #-}

module Apropos.Gen.Contexts (scriptContext) where

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

scriptContext :: Gen ScriptContext
scriptContext = do
  i <- txInfo
  p <- scriptPurpose
  return $ ScriptContext i p

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
  range <- posixTimeRange
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

txInInfo :: Gen TxInInfo
txInInfo = do
  oRef <- txOutRef
  o <- txOut
  return $ TxInInfo oRef o

txOut :: Gen TxOut
txOut = do
  a <- address
  v <- value
  h <- Gen.maybe datumHash
  return $ TxOut a v h

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

txOutRef :: Gen TxOutRef
txOutRef = do
  id' <- txId
  idx <- integer (linear 0 maxBound)
  return $ TxOutRef id' idx
