{-# LANGUAGE RankNTypes #-}

module Apropos.Script.Iso.DSL (
  NamedArrow(..),
  UnaryConstraint(..),
  BinaryConstraint(..),
  haskUnaryConstraint,
) where
import Plutarch
import Plutarch.Lift
import Plutarch.Api.V1.Tuple

class NamedArrow arrow where
  arrow :: (a -> b) -> arrow a b
  unArrow :: arrow a b -> (a -> b)
  arrowName :: arrow a b -> String

class UnaryConstraint c where
  unaryConstraint :: (a -> Bool) -> c a
  unUnaryConstraint :: c a -> (a -> Bool)
  unaryConstraintName :: c a -> String

class BinaryConstraint c where
  binaryConstraint :: (a -> b -> Bool) -> c a b
  unBinaryConstraint :: c a b -> (a -> b -> Bool)
  binaryConstraintName :: c a b -> String


type PlutarchArrow s antecedent consequent = Term s (antecedent :--> consequent)

-- where POpaque is a truthy value e.g. perror = False, PUnit = True
type PlutarchUnaryConstraint s domain = Term s (domain :--> POpaque)

-- where POpaque is a truthy value e.g. perror = False, PUnit = True
type PlutarchBinaryConstraint s domainL domainR = Term s (domainL :--> domainR :--> POpaque)


data IsoArrow s a b where
  IsoArrow :: (PLift a, PLift b, NamedArrow arrow)
           => arrow (PLifted a) (PLifted b) -> PlutarchArrow s a b -> IsoArrow s a b
  IsoArrowSeq :: IsoArrow s a v -> IsoArrow s v b -> IsoArrow s a b
  IsoArrowFanout :: (b ~ PTuple bl br, PLifted (PTuple bl br) ~ (PLifted bl, PLifted br))
                 => IsoArrow s a bl -> IsoArrow s a br -> IsoArrow s a b
  IsoArrowParallel :: (a ~ PTuple al ar, PLifted (PTuple al ar) ~ (PLifted al, PLifted ar)
                      ,b ~ PTuple bl br, PLifted (PTuple bl br) ~ (PLifted bl, PLifted br))
                   => IsoArrow s al bl -> IsoArrow s ar br -> IsoArrow s a b

data IsoBinaryConstraint s a b where
  IsoBinaryConstraint :: (PLift a, PLift b, BinaryConstraint c , Show (c (PLifted a) (PLifted b)))
                      => c (PLifted a) (PLifted b) -> PlutarchBinaryConstraint s a b -> IsoBinaryConstraint s a b
  IsoBinaryConstraintAssoc :: IsoBinaryConstraint s a b -> IsoBinaryConstraint s a b -> IsoBinaryConstraint s a b
  IsoBinaryConstraintArrowLeft :: IsoArrow s a c -> IsoBinaryConstraint s c b -> IsoBinaryConstraint s a b
  IsoBinaryConstraintArrowRight :: IsoBinaryConstraint s a c -> IsoArrow s b c -> IsoBinaryConstraint s a b
  IsoBinaryConstraintArrowParallel :: (ab ~ PTuple a b, PLifted (PTuple a b) ~ (PLifted a, PLifted b)
                                      ,v ~ PTuple vl vr, PLifted (PTuple vl vr) ~ (PLifted vl, PLifted vr))
                                  => IsoArrow s ab v -> IsoBinaryConstraint s vl vr -> IsoBinaryConstraint s a b


data IsoUnaryConstraint s a where
  IsoUnaryConstraint :: (PLift a, UnaryConstraint c, Show (c (PLifted a)))
                     => c (PLifted a) -> PlutarchUnaryConstraint s a -> IsoUnaryConstraint s a
  IsoUnaryFromBinaryConstraint :: IsoBinaryConstraint s a a -> IsoUnaryConstraint s a
  IsoUnaryConstraintAssoc :: IsoUnaryConstraint s a -> IsoUnaryConstraint s a -> IsoUnaryConstraint s a
  IsoUnaryConstraintArrow :: IsoArrow s a b -> IsoUnaryConstraint s b -> IsoUnaryConstraint s a


haskArrow :: IsoArrow s a b -> ((PLifted a) -> (PLifted b))
haskArrow (IsoArrow a _) = unArrow a
haskArrow (IsoArrowSeq l r) = haskArrow r . haskArrow l
haskArrow (IsoArrowFanout l r) = \a -> ((haskArrow l) a, (haskArrow r) a)
haskArrow (IsoArrowParallel l r) = \(al,ar) -> ((haskArrow l) al, (haskArrow r) ar)

haskBinaryConstraint :: IsoBinaryConstraint s a b -> ((PLifted a) -> (PLifted b) -> Bool)
haskBinaryConstraint (IsoBinaryConstraint c _) = unBinaryConstraint c
haskBinaryConstraint (IsoBinaryConstraintAssoc l r) = \a b -> (haskBinaryConstraint l) a b && (haskBinaryConstraint r) a b
haskBinaryConstraint (IsoBinaryConstraintArrowLeft ar c) = \a b -> (haskBinaryConstraint c) (haskArrow ar $ a) b
haskBinaryConstraint (IsoBinaryConstraintArrowRight c ar) = \a b -> (haskBinaryConstraint c) a (haskArrow ar $ b)
haskBinaryConstraint (IsoBinaryConstraintArrowParallel ar c) = \a b -> uncurry (haskBinaryConstraint c) ((haskArrow ar) (a, b))

haskUnaryConstraint :: IsoUnaryConstraint s a -> (PLifted a -> Bool)
haskUnaryConstraint (IsoUnaryConstraint c _) = unUnaryConstraint c
haskUnaryConstraint (IsoUnaryFromBinaryConstraint bc) = \a -> (haskBinaryConstraint bc) a a
haskUnaryConstraint (IsoUnaryConstraintAssoc l r) = \a -> (haskUnaryConstraint l) a && (haskUnaryConstraint r) a
haskUnaryConstraint (IsoUnaryConstraintArrow ar c) = \a -> (haskUnaryConstraint c) ((haskArrow ar) a)



