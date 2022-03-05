{-# LANGUAGE RankNTypes #-}
module Apropos.Tx.Constraint (
  PlutarchConstraint,
  TxConstraint(..),
  txEq,
  ) where
import Plutarch (POpaque,popaque)
import Plutarch.Prelude
import Plutarch.Lift

-- where POpaque is a truthy value e.g. perror = False, PUnit = True
type PlutarchConstraint debruijn domain = Term debruijn (domain :--> POpaque)

data TxConstraint debruijn domain =
  TxConstraint {
    haskConstraint :: PConstantRepr domain -> Bool
  , plutarchConstraint :: PlutarchConstraint debruijn (PConstanted domain)
  }

-- this is like && for constraints on the same type
instance Semigroup (TxConstraint debruijn a) where
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



instance Monoid (TxConstraint debruijn a) where
  mempty = TxConstraint {
             haskConstraint = \_ -> True
           , plutarchConstraint = plam $ \_ -> popaque $ pcon PUnit
           }

-- TODO redundant pairing?
txEq :: (Eq (PConstantRepr a), PEq (PConstanted a)) => TxConstraint debruijn (a,a)
txEq = TxConstraint
  { haskConstraint = uncurry (==)
  , plutarchConstraint = plam $ \pp -> pif (papp pfstBuiltin pp  #== papp psndBuiltin pp)
                                           (popaque $ pcon PUnit)
                                           perror
  }
