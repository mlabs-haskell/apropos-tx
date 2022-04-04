module Apropos.Gen.Api (
  builtinByteString,
  builtinData,
) where

import Apropos.Gen (Gen)
import Apropos.Gen.Extra (sha256)
import PlutusTx (Data)
import PlutusTx.Builtins.Internal (
  BuiltinByteString (BuiltinByteString),
  BuiltinData (BuiltinData),
 )

builtinByteString :: Gen BuiltinByteString
builtinByteString = do
  bs <- sha256
  return $ BuiltinByteString bs

builtinData :: Gen BuiltinData
builtinData = do
  d <- data'
  return $ BuiltinData d

data' :: Gen Data
data' = do
  undefined
