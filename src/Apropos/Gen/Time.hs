{- |
Module: Apropos.Gen.Time
Description: Plutus POSIX time generators.
Maintainer: jack@mlabs.city

`Gen`s for `Plutus.V1.Ledger.Time` types.
-}
module Apropos.Gen.Time (posixTimeRange) where

import Apropos.Gen (Gen)
import Apropos.Gen.Extra (integer)
import Apropos.Gen.Interval (interval)
import Apropos.Gen.Range (Range)
import Plutus.V1.Ledger.Time (POSIXTime (POSIXTime), POSIXTimeRange)

-- | Function for producing `POSIXTimeRange` generators.
posixTimeRange ::
  -- | `Range` you wish for the lower bound to take.
  Range ->
  -- | `Range` you wish for the upper bound to take.
  Range ->
  -- | A generator for a `POSIXTimeRange`.
  Gen POSIXTimeRange
posixTimeRange rL rU = do
  let lGen :: Gen POSIXTime = posixTime rL
      uGen :: Gen POSIXTime = posixTime rU
  interval lGen uGen

-- | Function for producing `POSIXTime` generators.
posixTime :: Range -> Gen POSIXTime
posixTime r = do
  n <- integer r
  return $ POSIXTime n
