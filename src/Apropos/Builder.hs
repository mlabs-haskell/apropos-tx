{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO better name?
module Apropos.Builder (
  ScriptBuilder (..),
  scriptFromBuilder,
  BuilderState (..),
  addDatum,
  addOutput,
  addInput,
  setTimeRange,
  continueWith,
  startingState,
  buildScript,
  sign,
  mint,
) where

import Control.Lens (use, (%=), (.=))
import Control.Lens.TH (makeLenses)
import Control.Monad.State

import Plutarch.Prelude

import Plutarch (compile)
import Plutarch.Api.V1 (PScriptContext, scriptHash, validatorHash)
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  DCert (DCertMir),
  Datum,
  DatumHash (DatumHash),
  Extended (NegInf, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  POSIXTimeRange,
  PubKeyHash,
  Script,
  ScriptContext (ScriptContext),
  ScriptPurpose (Certifying),
  TxId (TxId),
  TxInInfo (TxInInfo),
  TxInfo (TxInfo),
  TxOut (TxOut),
  TxOutRef (TxOutRef),
  UpperBound (UpperBound),
  Validator (Validator),
  Value,
 )
import Plutus.V1.Ledger.Scripts (ScriptHash (ScriptHash))

class (PIsData (DatumFor m), PIsData (RedeemerFor m)) => ScriptBuilder m where
  type DatumFor m :: PType
  type RedeemerFor m :: PType

  plutarchScript :: Term s (PAsData (DatumFor m) :--> PAsData (RedeemerFor m) :--> PAsData PScriptContext :--> PUnit)

  builder :: m -> BuilderM ()

  makeDatum :: m -> Term s (DatumFor m)

  makeRedeemer :: m -> Term s (RedeemerFor m)

  makeContext :: m -> Term s PScriptContext
  makeContext m = contextFrom $ execState (builder m) (startingState @m)

buildScript :: forall m. ScriptBuilder m => m -> Script
buildScript m =
  compile $
    (plutarchScript @m)
      # pdata (makeDatum m)
      # pdata (makeRedeemer m)
      # pdata (makeContext m)

contextFrom :: BuilderState -> Term s PScriptContext
contextFrom m =
  pconstant $
    ScriptContext
      ( TxInfo
          (_inputs m)
          (_outputs m)
          mempty -- fee
          -- TODO make the fee a random plausible value
          (_minting m) --minting
          [] -- dcerts
          -- TODO what should this be?
          [] -- withdrawls
          -- TODO do we need this?
          (_timeRange m)
          [] -- signatures
          -- TODO add signatures
          (_datumTable m)
          (TxId "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
          -- TODO
          -- afaict this can be a dummy value
          -- randome might be better?
      )
      $ Certifying DCertMir

-- TODO is this correct

scriptFromBuilder :: forall m. ScriptBuilder m => m -> Script
scriptFromBuilder m = compile $ plutarchScript @m # pdata (makeDatum m) # pdata (makeRedeemer m) # pdata (makeContext m)

startingState :: forall m. ScriptBuilder m => BuilderState
startingState =
  BuilderState
    []
    []
    []
    []
    -- TODO random migt be a better default
    -- this would probably require a wrapper on m or similar
    (Interval (LowerBound NegInf False) (UpperBound PosInf False))
    mempty
    (scriptAdr $ compile (plutarchScript @m))

type BuilderM = State BuilderState

scriptAdr :: Script -> Address
scriptAdr s = Address (ScriptCredential . validatorHash $ Validator s) Nothing

data BuilderState = BuilderState
  { _datumTable :: [(DatumHash, Datum)]
  , _outputs :: [TxOut]
  , _inputs :: [TxInInfo]
  , _signatures :: [PubKeyHash]
  , _timeRange :: POSIXTimeRange
  , _minting :: Value
  , _ownScriptAdr :: Address
  }

makeLenses ''BuilderState

-- TODO
-- I'm not sure this is consistant with the way datum are hashed in nodes
-- I don't think it matters though
hashDatum :: Datum -> DatumHash
hashDatum d =
  let ScriptHash h = scriptHash $ compile $ pconstant d
   in DatumHash h

addDatum :: Datum -> BuilderM DatumHash
addDatum d = do
  let hash = hashDatum d
  datumTable %= ((hash, d) :)
  pure hash

addInput :: Address -> Value -> Datum -> BuilderM ()
addInput adr val datum = do
  hash <- addDatum datum
  inputs
    %= ( TxInInfo
          (TxOutRef "" 0)
          -- TODO I'm not sure what this should actually be
          (TxOut adr val $ Just hash)
          :
       )

addOutput :: Address -> Value -> Datum -> BuilderM ()
addOutput adr val datum = do
  hash <- addDatum datum
  outputs %= (TxOut adr val (Just hash) :)

continueWith :: Value -> Datum -> BuilderM ()
continueWith val datum = do
  adr <- use ownScriptAdr
  addOutput adr val datum

setTimeRange :: POSIXTimeRange -> BuilderM ()
setTimeRange tr = timeRange .= tr

sign :: PubKeyHash -> BuilderM ()
sign pkh = signatures %= (pkh :)

mint :: Value -> BuilderM ()
mint v = minting %= (v <>)
