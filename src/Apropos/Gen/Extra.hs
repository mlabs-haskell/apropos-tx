{-# OPTIONS_GHC -Wwarn #-}

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

pair :: Gen a -> Gen b -> Gen (a, b)
pair genA genB = do
  a <- genA
  b <- genB
  return (a, b)

integer :: Range -> Gen Integer
integer r = toInteger <$> int r

maybe :: Show a => Gen a -> Gen (Maybe a)
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

upperChar :: Gen Char
upperChar = element ['A' .. 'Z']

lowerChar :: Gen Char
lowerChar = element ['a' .. 'z']

numChar :: Gen Char
numChar = element ['0' .. '9']

hexChar :: Gen Char
hexChar = element $ ['0' .. '9'] ++ ['a' .. 'f']

alphaString :: Range -> Gen String
alphaString r = list r gen
  where
    gen = choice [upperChar, lowerChar]

alphaNumericString :: Range -> Gen String
alphaNumericString r = list r gen
  where
    gen = choice [upperChar, lowerChar, numChar]

numericString :: Range -> Gen String
numericString r = list r numChar

hexString :: Range -> Gen String
hexString r = list r hexChar

genText :: (Range -> Gen String) -> (Range -> Gen Text)
genText f r = pack <$> f r

alphaText :: Range -> Gen Text
alphaText = genText alphaString

alphaNumericText :: Range -> Gen Text
alphaNumericText = genText alphaNumericString

numericText :: Range -> Gen Text
numericText = genText alphaString

hexText :: Range -> Gen Text
hexText = genText hexString

genBS :: (Range -> Gen Text) -> (Range -> Gen ByteString)
genBS f r = encodeUtf8 <$> f r

alphaBS :: Range -> Gen ByteString
alphaBS = genBS alphaText

alphaNumericBS :: Range -> Gen ByteString
alphaNumericBS = genBS alphaNumericText

numericBS :: Range -> Gen ByteString
numericBS = genBS numericText

hexBS :: Range -> Gen ByteString
hexBS = genBS hexText

sha256 :: Gen ByteString
sha256 = hexBS $ singleton 64
