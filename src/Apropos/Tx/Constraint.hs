{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Apropos.Tx.Constraint (
  PlutarchConstraint,
  TxConstraint (..),
  Tuple (..),
  txEq,
  txNeq,
) where

import Plutarch (POpaque, popaque)
import Plutarch.Lift
import Plutarch.Prelude

-- where POpaque is a truthy value e.g. perror = False, PUnit = True
type PlutarchConstraint debruijn domain = Term debruijn (domain :--> POpaque)

data TxConstraint debruijn domain = TxConstraint
  { haskConstraint :: PConstantRepr domain -> Bool
  , plutarchConstraint :: PlutarchConstraint debruijn (PAsData (PConstanted domain))
  }

-- this is like && for constraints on the same type
instance Semigroup (TxConstraint debruijn a) where
  (<>) a b =
    TxConstraint
      { haskConstraint = \c -> haskConstraint a c && haskConstraint b c
      , plutarchConstraint =
          plutarchConstraint a
            `plutarchConstraintSemigroup` plutarchConstraint b
      }

plutarchConstraintSemigroup ::
  PlutarchConstraint debruijn a ->
  PlutarchConstraint debruijn a ->
  PlutarchConstraint debruijn a
plutarchConstraintSemigroup x y = plam $ \a -> opaqueSemigroup (papp x a) (papp y a)
  where
    opaqueSemigroup ig nor = papp (papp (plam $ \_ig _nor -> popaque $ pcon PUnit) ig) nor

instance Monoid (TxConstraint debruijn a) where
  mempty =
    TxConstraint
      { haskConstraint = const True
      , plutarchConstraint = plam $ \_ -> popaque $ pcon PUnit
      }

txNeq ::
  (Eq (PConstantRepr a)) =>
  TxConstraint debruijn (Tuple a a)
txNeq =
  TxConstraint
    { haskConstraint = uncurry (/=)
    , plutarchConstraint = plam $ \pp ->
        pif
          (papp pnot (papp pfstBuiltin (pfromData pp) #== papp psndBuiltin (pfromData pp)))
          (popaque $ pcon PUnit)
          perror
    }

txEq ::
  (Eq (PConstantRepr a)) =>
  TxConstraint debruijn (Tuple a a)
txEq =
  TxConstraint
    { haskConstraint = uncurry (==)
    , plutarchConstraint = plam $ \pp ->
        pif
          (papp pfstBuiltin (pfromData pp) #== papp psndBuiltin (pfromData pp))
          (popaque $ pcon PUnit)
          perror
    }

-- hmmmmm...
data Tuple a b = Tuple a b

instance (PConstant a, PConstant b) => PConstant (Tuple a b) where
  type PConstantRepr (Tuple a b) = (PConstantRepr a, PConstantRepr b)
  type PConstanted (Tuple a b) = PBuiltinPair (PAsData (PConstanted a)) (PAsData (PConstanted b))
  pconstantToRepr (Tuple x y) = (pconstantToRepr x, pconstantToRepr y)
  pconstantFromRepr (x, y) = do
    x' <- pconstantFromRepr @a x
    y' <- pconstantFromRepr @b y
    Just (Tuple x' y')
