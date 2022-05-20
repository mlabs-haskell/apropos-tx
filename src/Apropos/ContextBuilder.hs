module Apropos.ContextBuilder (
  TxInfoBuilder (..),
  ScriptContextBuilder (..),
  buildContext,
  withTxInfo,
  nullTxId,
  nullTxOutRef,
) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (StateT, execStateT, get, modify)
import Data.Functor.Identity (Identity, runIdentity)
import Plutarch.Api.V1 (datumHash)
import Plutus.V1.Ledger.Api (
  Address,
  BuiltinByteString,
  DCert,
  DatumHash (..),
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext (..),
  ScriptPurpose (..),
  StakingCredential,
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  Value (..),
  toBuiltinData,
 )
import Plutus.V1.Ledger.Interval (Extended (..), Interval (..), LowerBound (..), UpperBound (..))
import Plutus.V1.Ledger.Scripts (Context (..), Datum)
import Plutus.V2.Ledger.Api (fromList)

-- with concrete types and extra packaging for convenience
buildContext :: StateT ScriptContext Identity () -> Context
buildContext builder = Context $ toBuiltinData sc
  where
    sc = runIdentity $ buildScriptContext @(StateT ScriptContext) @Identity builder

-- with concrete types for convenience
withTxInfo :: StateT TxInfo Identity () -> StateT ScriptContext Identity ()
withTxInfo = withTxInfoBuilder

nullTxId :: TxId
nullTxId = TxId "0000000000000000000000000000000000000000000000000000000000000000"

nullTxOutRef :: TxOutRef
nullTxOutRef = TxOutRef nullTxId 0

emptyScriptContext :: ScriptContext
emptyScriptContext = ScriptContext emptyTxInfo (Spending (TxOutRef nullTxId 0))

class (MonadTrans t, Monad m, Monad (t m)) => ScriptContextBuilder t m where
  runScriptContextBuilder :: ScriptContext -> t m () -> m ScriptContext
  buildScriptContext :: t m () -> m ScriptContext
  getTxInfo :: t m TxInfo
  setTxInfo :: TxInfo -> t m ()
  setScriptPurpose :: ScriptPurpose -> t m ()
  withTxInfoBuilder :: TxInfoBuilder u m => u m () -> t m ()
  withTxInfoBuilder b = getTxInfo >>= (\t -> lift (runTxInfoBuilder t b)) >>= setTxInfo

instance Monad m => ScriptContextBuilder (StateT ScriptContext) m where
  runScriptContextBuilder = flip execStateT
  buildScriptContext = runScriptContextBuilder emptyScriptContext
  getTxInfo = scriptContextTxInfo <$> get
  setTxInfo t = modify (\s -> s {scriptContextTxInfo = t})
  setScriptPurpose s = modify (\sc -> sc {scriptContextPurpose = s})

emptyTxInfo :: TxInfo
emptyTxInfo =
  TxInfo
    { txInfoInputs = []
    , txInfoOutputs = []
    , txInfoFee = Value (fromList [])
    , txInfoMint = Value (fromList [])
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = Interval (LowerBound NegInf True) (UpperBound PosInf True)
    , txInfoSignatories = []
    , txInfoData = []
    , txInfoId = nullTxId
    }

class (MonadTrans t, Monad m) => TxInfoBuilder t m where
  runTxInfoBuilder :: TxInfo -> t m () -> m TxInfo
  buildTxInfo :: t m () -> m TxInfo
  addInput :: TxOutRef -> Address -> Value -> Maybe Datum -> t m ()
  addOutput :: Address -> Value -> Maybe Datum -> t m ()

  addFee :: Value -> t m ()
  mint :: Value -> t m ()

  addTxInfoInput :: TxInInfo -> t m ()
  addTxInfoOutput :: TxOut -> t m ()
  addTxInfoDCert :: DCert -> t m ()
  addTxInfoWdrl :: (StakingCredential, Integer) -> t m ()
  addTxInfoSignatory :: PubKeyHash -> t m ()
  addTxInfoData :: (DatumHash, Datum) -> t m ()

  setTxInfoInputs :: [TxInInfo] -> t m ()
  setTxInfoOutputs :: [TxOut] -> t m ()
  setTxInfoFee :: Value -> t m ()
  setTxInfoMint :: Value -> t m ()
  setTxInfoDCert :: [DCert] -> t m ()
  setTxInfoWdrl :: [(StakingCredential, Integer)] -> t m ()
  setTxInfoValidRange :: POSIXTimeRange -> t m ()
  setTxInfoSignatories :: [PubKeyHash] -> t m ()
  setTxInfoData :: [(DatumHash, Datum)] -> t m ()
  setTxInfoId :: BuiltinByteString -> t m ()

  txInfoInputsUntouched :: t m ()
  txInfoOutputsUntouched :: t m ()
  txInfoFeeUntouched :: t m ()
  txInfoMintUntouched :: t m ()
  txInfoDCertUntouched :: t m ()
  txInfoWdrlUntouched :: t m ()
  txInfoValidRangeUntouched :: t m ()
  txInfoSignatoriesUntouched :: t m ()
  txInfoDataUntouched :: t m ()
  txInfoIdUntouched :: t m ()

instance Monad m => TxInfoBuilder (StateT TxInfo) m where
  runTxInfoBuilder = flip execStateT
  buildTxInfo = runTxInfoBuilder emptyTxInfo
  addInput r a v d =
    let i = TxInInfo r (TxOut a v (datumHash <$> d))
        addDatum = case d of
          Nothing -> id
          Just so -> (<> [(datumHash so, so)])
     in modify
          ( \txi ->
              txi
                { txInfoInputs = txInfoInputs txi <> [i]
                , txInfoData = addDatum (txInfoData txi)
                }
          )
  addOutput a v d =
    let i = TxOut a v (datumHash <$> d)
        addDatum = case d of
          Nothing -> id
          Just so -> (<> [(datumHash so, so)])
     in modify
          ( \txi ->
              txi
                { txInfoOutputs = txInfoOutputs txi <> [i]
                , txInfoData = addDatum (txInfoData txi)
                }
          )

  addFee f = modify (\txi -> txi {txInfoFee = f <> txInfoFee txi})
  mint f = modify (\txi -> txi {txInfoMint = f <> txInfoMint txi})
  addTxInfoInput i = modify (\txi -> txi {txInfoInputs = txInfoInputs txi <> [i]})
  addTxInfoOutput o = modify (\txi -> txi {txInfoOutputs = txInfoOutputs txi <> [o]})
  addTxInfoDCert d = modify (\txi -> txi {txInfoDCert = txInfoDCert txi <> [d]})
  addTxInfoWdrl w = modify (\txi -> txi {txInfoWdrl = txInfoWdrl txi <> [w]})
  addTxInfoSignatory s = modify (\txi -> txi {txInfoSignatories = txInfoSignatories txi <> [s]})
  addTxInfoData d = modify (\txi -> txi {txInfoData = txInfoData txi <> [d]})

  setTxInfoInputs i = modify (\txi -> txi {txInfoInputs = i})
  setTxInfoOutputs o = modify (\txi -> txi {txInfoOutputs = o})
  setTxInfoFee f = modify (\txi -> txi {txInfoFee = f})
  setTxInfoMint m = modify (\txi -> txi {txInfoMint = m})
  setTxInfoDCert d = modify (\txi -> txi {txInfoDCert = d})
  setTxInfoWdrl w = modify (\txi -> txi {txInfoWdrl = w})
  setTxInfoValidRange r = modify (\txi -> txi {txInfoValidRange = r})
  setTxInfoSignatories s = modify (\txi -> txi {txInfoSignatories = s})
  setTxInfoData d = modify (\txi -> txi {txInfoData = d})
  setTxInfoId b = modify (\txi -> txi {txInfoId = TxId b})

  txInfoInputsUntouched = modify (\txi -> txi {txInfoInputs = untouched "txInfoInputs"})
  txInfoOutputsUntouched = modify (\txi -> txi {txInfoOutputs = untouched "txInfoOutputs"})
  txInfoFeeUntouched = modify (\txi -> txi {txInfoFee = untouched "txInfoFee"})
  txInfoMintUntouched = modify (\txi -> txi {txInfoMint = untouched "txInfoMint"})
  txInfoDCertUntouched = modify (\txi -> txi {txInfoDCert = untouched "txInfoDCert"})
  txInfoWdrlUntouched = modify (\txi -> txi {txInfoWdrl = untouched "txInfoWdrl"})
  txInfoValidRangeUntouched = modify (\txi -> txi {txInfoValidRange = untouched "txInfoValidRange"})
  txInfoSignatoriesUntouched = modify (\txi -> txi {txInfoSignatories = untouched "txInfoSignatories"})
  txInfoDataUntouched = modify (\txi -> txi {txInfoData = untouched "txInfoData"})
  txInfoIdUntouched = modify (\txi -> txi {txInfoId = untouched "txInfoId"})

untouched :: String -> a
untouched e = error ("Did not expect script to touch " <> e <> ".")
