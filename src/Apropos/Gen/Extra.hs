module Apropos.Gen.Extra (pair, integer, maybe) where

import Apropos.Gen (Gen, choice, int)
import Apropos.Gen.Range (Range)
import Prelude hiding (maybe)

pair :: Gen a -> Gen b -> Gen (a, b)
pair genA genB = do
  a <- genA
  b <- genB
  return (a, b)

integer :: Range -> Gen Integer
integer r = toInteger <$> int r

maybe :: Gen a -> Gen (Maybe a)
maybe genA = do
  x <- genA
  choice [return (Just x), return Nothing]
