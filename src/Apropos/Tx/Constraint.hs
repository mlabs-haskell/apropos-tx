{-# LANGUAGE RankNTypes #-}
module Apropos.Tx.Constraint (
  PlutarchConstraint,
  TxConstraint(..),
  txEq,
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

-- we are introducing PBuiltinPair here for arrow composition
-- e.g. compose two getter arrows for the same type in parallel then pipe into this
-- can we strip out the construction and deconstruction of these pairs on composition?
-- either that or introduce a richer family of constraint types
-- e.g. UnaryConstraint, BinaryConstraint, ListConstraint...
txEq :: (Eq a) => TxConstraint (a,a) debruijn (PBuiltinPair (PAsData p) (PAsData p))
txEq = TxConstraint
  { haskConstraint = uncurry (==)
  , plutarchConstraint = plam $ \pp -> pif (papp pfstBuiltin pp  #== papp psndBuiltin pp)
                                           (popaque $ pcon PUnit)
                                           perror
  }
