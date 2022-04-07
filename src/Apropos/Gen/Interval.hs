module Apropos.Gen.Interval (interval) where

import Apropos.Gen (Gen)
import Plutus.V1.Ledger.Interval (Interval)

interval :: Gen a -> Gen (Interval a)
interval = undefined
