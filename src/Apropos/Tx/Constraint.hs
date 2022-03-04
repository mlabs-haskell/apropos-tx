{-# LANGUAGE RankNTypes #-}
module Apropos.Tx.Constraint (
  PlutarchConstraint,
  TxConstraint(..),
  ) where
import Plutarch.Prelude

type PlutarchConstraint debruijn domain = Term debruijn (domain :--> PUnit)

data TxConstraint haskDomain debruijn plutarchDomain =
  TxConstraint {
    haskConstraint :: haskDomain -> Bool
  , plutarchConstraint :: PlutarchConstraint debruijn plutarchDomain
  }

-- this is like && for constraints on the same type
instance Semigroup (TxConstraint a debruijn a') where
  (<>) a b = TxConstraint {
                 haskConstraint = \c -> (haskConstraint a) c && (haskConstraint b) c
               , plutarchConstraint = plam $ \c -> (papp (plutarchConstraint a) c)
                                                <> (papp (plutarchConstraint b) c)
               }

instance Monoid (TxConstraint a debruijn a') where
  mempty = TxConstraint {
             haskConstraint = \_ -> True
           , plutarchConstraint = plam $ \_ -> pcon PUnit
           }

