module Apropos.Tx.Constraint (
  Constraint,
  ) where
import Plutarch.Prelude

type Constraint s a b = Term s (a :--> b :--> PUnit)

