module Apropos.Tx.Protocol (
  Protocol (..),
) where

import Apropos.Tx.Constraint.Policy

newtype Protocol = Protocol [ConstraintPolicy]

-- from a protocol we can generate a validator script
---datum :: ([CurrencySymbo], Data)
-- redeemer :: ()
-- by reading the currency symbols and ensuring each ConstraintPolicy runs
-- we delegate all protocol logic to the Policies
