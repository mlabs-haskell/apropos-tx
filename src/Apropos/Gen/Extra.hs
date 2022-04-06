{- |
Module: Apropos.Gen.Extra
Description: Generator helper functions.
Maintainer: jack@mlabs.city

Helper functions for `Apropos.Gen`.
-}
module Apropos.Gen.Extra (
  sha256,
  map,
  pair,
  integer,
  maybe,
) where

import Apropos.Gen (Gen, choice, element, int, list)
import Apropos.Gen.Range (Range, singleton)
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import PlutusTx.AssocMap (Map, fromList)
import Prelude hiding (map, maybe)

-- | Creates a generator for a 2-tuple.
pair ::
  -- | Generator for the type of the first element.
  Gen a ->
  -- | Generator for the type of the second element.
  Gen b ->
  -- | Generator for a pair of a value of the first type and a
  --   value of the second type.
  Gen (a, b)
pair genA genB = do
  a <- genA
  b <- genB
  return (a, b)

-- | Function producing a generator for an `Integer`.
integer ::
  -- | Provides min and max bound for the integer to be
  --   generated.
  Range ->
  -- | The `Integer` generator.
  Gen Integer
integer r = toInteger <$> int r

-- | Function producing a generator for a `Maybe` type.
maybe ::
  Show a =>
  -- | A generator for the type to be wrapped by the `Maybe`
  --   monad.
  Gen a ->
  -- | A generator for the desired `Maybe` type.
  Gen (Maybe a)
maybe genA = do
  x <- genA
  element [Just x, Nothing]

{- | Given a generator for a key-type k and a value-type v
     returns a generator for a `Map` k v.
-}
map ::
  -- | `Range` for the number of map entries.
  Range ->
  -- | Generator for the desired type of the keys.
  Gen k ->
  -- | Generator for the desired type of the values.
  Gen v ->
  -- | Generator for a map.
  Gen (Map k v) --
map r genK genV = do
  ks <- list r genK
  vs <- list r genV
  let mapList = zip ks vs
  return $ fromList mapList

-- | Generator for an upper case alphabetic character.
upperChar :: Gen Char
upperChar = element ['A' .. 'Z']

-- | Generator for a lower case alphaberic character.
lowerChar :: Gen Char
lowerChar = element ['a' .. 'z']

-- | Generator for a numeric character.
numChar :: Gen Char
numChar = element ['0' .. '9']

-- | Generator for a hexademical character.
hexChar :: Gen Char
hexChar = element $ ['0' .. '9'] ++ ['a' .. 'f']

-- | Returns a generator for a mixed case alphabetic string.
alphaString ::
  -- | `Range` for the length of the string.
  Range ->
  Gen String
alphaString r = list r gen
  where
    gen = choice [upperChar, lowerChar]

-- | Returns a generator for a mixed case alphanumeric string.
alphanumericString ::
  -- | `Range` for the length of the string.
  Range ->
  Gen String
alphanumericString r = list r gen
  where
    gen = choice [upperChar, lowerChar, numChar]

-- | Returns a generator for a numeric string.
numericString ::
  -- | `Range` for the length of the string.
  Range ->
  Gen String
numericString r = list r numChar

-- | Return a generator for a hexadecimal string.
hexString :: Range -> Gen String
hexString r = list r hexChar

genText :: (Range -> Gen String) -> (Range -> Gen Text)
genText f r = pack <$> f r

alphaText :: Range -> Gen Text
alphaText = genText alphaString

alphanumericText :: Range -> Gen Text
alphanumericText = genText alphanumericString

numericText :: Range -> Gen Text
numericText = genText alphaString

hexText :: Range -> Gen Text
hexText = genText hexString

genBS :: (Range -> Gen Text) -> (Range -> Gen ByteString)
genBS f r = encodeUtf8 <$> f r

alphaBS :: Range -> Gen ByteString
alphaBS = genBS alphaText

alphanumericBS :: Range -> Gen ByteString
alphanumericBS = genBS alphanumericText

numericBS :: Range -> Gen ByteString
numericBS = genBS numericText

hexBS :: Range -> Gen ByteString
hexBS = genBS hexText

sha256 :: Gen ByteString
sha256 = hexBS $ singleton 64
