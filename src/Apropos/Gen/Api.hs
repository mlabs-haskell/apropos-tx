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

import Apropos.Gen (Gen, element, int, linear, list, singleton)
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
  let mapGen :: Gen [(Data, Data)]
      mapGen = list (linear 0 20) $ pair data' data'
  n <- integer $ linear minBound maxBound
  b <- sha256
  m <- mapGen
  i <- int $ linear 0 50
  l <- list (singleton i) data'
  element
    [ Constr (toInteger i) l
    , Map m
    , List l
    , I n
    , B b
    ]
