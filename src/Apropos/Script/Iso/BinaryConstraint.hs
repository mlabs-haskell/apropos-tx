{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Apropos.Script.Iso.BinaryConstraint (
  PlutarchBinaryConstraint,
  IsoBinaryConstraint (..),
  constraintEq,
  binConstraintNeq,
) where

import Plutarch (POpaque, popaque)
import Plutarch.Lift
import Plutarch.Prelude

-- where POpaque is a truthy value e.g. perror = False, PUnit = True
type PlutarchBinaryConstraint debruijn domainL domainR = Term debruijn (domainL :--> domainR :--> POpaque)

data IsoBinaryConstraint debruijn domainL domainR = IsoBinaryConstraint
  { haskBinaryConstraint :: PConstantRepr domainL -> PConstantRepr domainR -> Bool
  , plutarchBinaryConstraint :: PlutarchBinaryConstraint debruijn (PAsData (PConstanted domainL)) (PAsData (PConstanted domainR))

  }

-- this is like && for constraints on the same type
instance Semigroup (IsoBinaryConstraint debruijn a b) where
  (<>) a b =
    IsoBinaryConstraint
      { haskBinaryConstraint = \cL cR -> haskBinaryConstraint a cL cR && haskBinaryConstraint b cL cR
      , plutarchBinaryConstraint =
          plutarchBinaryConstraint a
            `plutarchBinaryConstraintSemigroup` plutarchBinaryConstraint b
      }

plutarchBinaryConstraintSemigroup ::
  PlutarchBinaryConstraint debruijn a b ->
  PlutarchBinaryConstraint debruijn a b ->
  PlutarchBinaryConstraint debruijn a b
plutarchBinaryConstraintSemigroup x y =
  plam $ \a -> (plam $ \b -> opaqueSemigroup (papp (papp x a) b) (papp (papp y a) b))
  where
    opaqueSemigroup ig nor = papp (papp (plam $ \_ig _nor -> popaque $ pcon PUnit) ig) nor

instance Monoid (IsoBinaryConstraint debruijn a b) where
  mempty =
    IsoBinaryConstraint
      { haskBinaryConstraint = \_ _ -> True
      , plutarchBinaryConstraint = plam $ \_ -> (plam $ \_ -> popaque $ pcon PUnit)
      }

binConstraintNeq ::
  (Eq (PConstantRepr a)) =>
  IsoBinaryConstraint debruijn a a
binConstraintNeq =
  IsoBinaryConstraint
    { haskBinaryConstraint = (/=)
    , plutarchBinaryConstraint = plam $ \a ->
                                   plam $ \b -> pif (papp pnot (a #== b))
                                                    (popaque $ pcon PUnit)
                                                    perror
    }

constraintEq ::
  (Eq (PConstantRepr a)) =>
  IsoBinaryConstraint debruijn a a
constraintEq =
  IsoBinaryConstraint
    { haskBinaryConstraint = (==)
    , plutarchBinaryConstraint = plam $ \a ->
                                   plam $ \b -> pif (a #== b)
                                                  (popaque $ pcon PUnit)
                                                  perror
    }

