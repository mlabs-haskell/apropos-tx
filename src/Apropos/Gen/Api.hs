{-# OPTIONS_GHC -Wwarn #-}

{- |
Module: Apropos.Gen.Api
Description: Misc. Plutus generators.
Maintainer: jack@mlabs.city

`Gen`s for `Plutus.V1.Ledger.Api` types.
-}
module Apropos.Gen.Api (
  builtinByteString,
  -- builtinData,
) where

import Apropos.Gen (Gen, choice, int, linear, list, singleton)
import Apropos.Gen.Extra (integer, pair, sha256)
import PlutusTx (Data (B, Constr, I, List, Map))
import PlutusTx.Builtins.Internal (
  BuiltinByteString (BuiltinByteString),
  BuiltinData (BuiltinData),
 )

-- | `Gen` for Plutus `BuiltinByteString`.
builtinByteString :: Gen BuiltinByteString
builtinByteString = do
  bs <- sha256
  return $ BuiltinByteString bs

-- | `Gen` for Plutus `BuiltinData`.
builtinData :: Gen BuiltinData
builtinData = do
  d <- data'
  return $ BuiltinData d

-- | TODO: Re-do; too slow. `Gen` for Plutus `Data`.
data' :: Gen Data
data' = do
  let constrGen = do
        len <- int (linear 0 50)
        l <- list (singleton len) data'
        let i = toInteger len
        return $ Constr i l
  choice
    [ constrGen
    , Map <$> list (linear 0 20) (pair data' data')
    , List <$> list (linear 0 10) data'
    , I <$> integer (linear minBound maxBound)
    , B <$> sha256
    ]
