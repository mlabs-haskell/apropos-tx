{-# LANGUAGE RankNTypes #-}
module Apropos.Tx.Constraint (
  PlutarchConstraint,
  TxConstraint(..),
  ) where
import Plutarch (POpaque,popaque)
import Plutarch.Prelude

-- where POpaque is a truthy value e.g. perror = False, PUnit = True
type PlutarchConstraint debruijn domain = Term debruijn (domain :--> POpaque)

data TxConstraint haskDomain debruijn plutarchDomain =
  TxConstraint {
    haskConstraint :: haskDomain -> Bool
  , plutarchConstraint :: PlutarchConstraint debruijn plutarchDomain
  }

-- this is like && for constraints on the same type
instance Semigroup (TxConstraint a debruijn a') where
  (<>) a b = TxConstraint {
                 haskConstraint = \c -> (haskConstraint a) c && (haskConstraint b) c
               , plutarchConstraint = (plutarchConstraint a)
                          `plutarchConstraintSemigroup` (plutarchConstraint b)
               }


plutarchConstraintSemigroup :: PlutarchConstraint debruijn a
                             -> PlutarchConstraint debruijn a
                             -> PlutarchConstraint debruijn a
plutarchConstraintSemigroup x y = plam $ \a -> opaqueSemigroup (papp x a) (papp y a)
  where
    opaqueSemigroup ig nor = papp (papp (plam $ \_ig _nor-> popaque $ pcon PUnit) ig) nor



instance Monoid (TxConstraint a debruijn a') where
  mempty = TxConstraint {
             haskConstraint = \_ -> True
           , plutarchConstraint = plam $ \_ -> popaque $ pcon PUnit
           }

