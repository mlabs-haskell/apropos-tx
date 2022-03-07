{-# LANGUAGE RankNTypes #-}

module Apropos.Script.Iso.DSL (
  NamedIsoArrow(..),
  NamedIsoUnaryConstraint(..),
  NamedIsoBinaryConstraint(..),
  haskUnaryConstraint,
) where
import Plutarch
import Plutarch.Lift
import Plutarch.Api.V1.Tuple

class NamedIsoArrow arrow where
  arrowName :: arrow s a b -> String
  isoArrow :: (PLifted a -> PLifted b) -> PlutarchArrow s a b -> arrow s a b
  hArrow :: arrow s a b -> (PLifted a -> PLifted b)
  pArrow :: arrow s a b -> PlutarchArrow s a b


class NamedIsoUnaryConstraint constraint where
  unaryConstraintName :: constraint s a -> String
  unaryConstraint :: (PLifted a -> Bool) -> PlutarchUnaryConstraint s a -> constraint s a
  hUnaryConstraint :: constraint s a -> (PLifted a -> Bool)
  pUnaryConstraint :: constraint s a -> PlutarchUnaryConstraint s a

class NamedIsoBinaryConstraint constraint where
  binaryConstraintName :: constraint s a b -> String
  binaryConstraint :: (PLifted a -> PLifted b -> Bool) -> PlutarchBinaryConstraint s a b -> constraint s a b
  hBinaryConstraint :: constraint s a b -> (PLifted a -> PLifted b -> Bool)
  pBinaryConstraint :: constraint s a b -> PlutarchBinaryConstraint s a b


type PlutarchArrow s antecedent consequent = Term s (antecedent :--> consequent)

-- where POpaque is a truthy value e.g. perror = False, PUnit = True
type PlutarchUnaryConstraint s domain = Term s (domain :--> POpaque)

-- where POpaque is a truthy value e.g. perror = False, PUnit = True
type PlutarchBinaryConstraint s domainL domainR = Term s (domainL :--> domainR :--> POpaque)


data IsoArrow s a b where
  IsoArrow :: NamedIsoArrow arrow => arrow s a b -> IsoArrow s a b
  IsoArrowSeq :: IsoArrow s a v -> IsoArrow s v b -> IsoArrow s a b
  IsoArrowFanout :: (b ~ PTuple bl br, PLifted (PTuple bl br) ~ (PLifted bl, PLifted br))
                 => IsoArrow s a bl -> IsoArrow s a br -> IsoArrow s a b
  IsoArrowParallel :: (a ~ PTuple al ar, PLifted (PTuple al ar) ~ (PLifted al, PLifted ar)
                      ,b ~ PTuple bl br, PLifted (PTuple bl br) ~ (PLifted bl, PLifted br))
                   => IsoArrow s al bl -> IsoArrow s ar br -> IsoArrow s a b

data IsoBinaryConstraint s a b where
  IsoBinaryConstraint :: NamedIsoBinaryConstraint constraint
                      => constraint s a b -> IsoBinaryConstraint s a b
  IsoBinaryConstraintAssoc :: IsoBinaryConstraint s a b -> IsoBinaryConstraint s a b -> IsoBinaryConstraint s a b
  IsoBinaryConstraintArrowLeft :: IsoArrow s a c -> IsoBinaryConstraint s c b -> IsoBinaryConstraint s a b
  IsoBinaryConstraintArrowRight :: IsoBinaryConstraint s a c -> IsoArrow s b c -> IsoBinaryConstraint s a b
  IsoBinaryConstraintArrowParallel :: (ab ~ PTuple a b, PLifted (PTuple a b) ~ (PLifted a, PLifted b)
                                      ,v ~ PTuple vl vr, PLifted (PTuple vl vr) ~ (PLifted vl, PLifted vr))
                                  => IsoArrow s ab v -> IsoBinaryConstraint s vl vr -> IsoBinaryConstraint s a b


data IsoUnaryConstraint s a where
  IsoUnaryConstraint :: NamedIsoUnaryConstraint constraint
                     => constraint s a -> IsoUnaryConstraint s a
  IsoUnaryFromBinaryConstraint :: IsoBinaryConstraint s a a -> IsoUnaryConstraint s a
  IsoUnaryConstraintAssoc :: IsoUnaryConstraint s a -> IsoUnaryConstraint s a -> IsoUnaryConstraint s a
  IsoUnaryConstraintArrow :: IsoArrow s a b -> IsoUnaryConstraint s b -> IsoUnaryConstraint s a


haskArrow :: IsoArrow s a b -> ((PLifted a) -> (PLifted b))
haskArrow (IsoArrow a) = hArrow a
haskArrow (IsoArrowSeq l r) = haskArrow r . haskArrow l
haskArrow (IsoArrowFanout l r) = \a -> ((haskArrow l) a, (haskArrow r) a)
haskArrow (IsoArrowParallel l r) = \(al,ar) -> ((haskArrow l) al, (haskArrow r) ar)

haskBinaryConstraint :: IsoBinaryConstraint s a b -> ((PLifted a) -> (PLifted b) -> Bool)
haskBinaryConstraint (IsoBinaryConstraint c) = hBinaryConstraint c
haskBinaryConstraint (IsoBinaryConstraintAssoc l r) = \a b -> (haskBinaryConstraint l) a b && (haskBinaryConstraint r) a b
haskBinaryConstraint (IsoBinaryConstraintArrowLeft ar c) = \a b -> (haskBinaryConstraint c) (haskArrow ar $ a) b
haskBinaryConstraint (IsoBinaryConstraintArrowRight c ar) = \a b -> (haskBinaryConstraint c) a (haskArrow ar $ b)
haskBinaryConstraint (IsoBinaryConstraintArrowParallel ar c) = \a b -> uncurry (haskBinaryConstraint c) ((haskArrow ar) (a, b))

haskUnaryConstraint :: IsoUnaryConstraint s a -> (PLifted a -> Bool)
haskUnaryConstraint (IsoUnaryConstraint c) = hUnaryConstraint c
haskUnaryConstraint (IsoUnaryFromBinaryConstraint bc) = \a -> (haskBinaryConstraint bc) a a
haskUnaryConstraint (IsoUnaryConstraintAssoc l r) = \a -> (haskUnaryConstraint l) a && (haskUnaryConstraint r) a
haskUnaryConstraint (IsoUnaryConstraintArrow ar c) = \a -> (haskUnaryConstraint c) ((haskArrow ar) a)


plutarchArrow :: IsoArrow s a b -> PlutarchArrow s a b
plutarchArrow (IsoArrow a) = pArrow a

plutarchBinaryConstraint :: IsoBinaryConstraint s a b -> PlutarchArrow s a b
plutarchBinaryConstraint (IsoBinaryConstraintArrowParallel (IsoArrowParallel l r) c) =
  plutarchBinaryConstraint $ IsoBinaryConstraintArrowRight (IsoBinaryConstraintArrowLeft l c) r





