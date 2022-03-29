module Apropos.Gen.Value (
  currencySymbol,
  value
) where

import Apropos.Gen (Gen)
import Plutus.V1.Ledger.Value (Value, CurrencySymbol)

value :: Gen Value 
value = undefined

currencySymbol :: Gen CurrencySymbol
currencySymbol = undefined
