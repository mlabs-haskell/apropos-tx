{-# LANGUAGE RankNTypes #-}

module Apropos.Script.Iso.DSL (
  NamedArrow(..),
  NamedUnaryConstraint(..),
  NamedBinaryConstraint(..),
  compileArrow,
  compileBinaryConstraint,
  compileUnaryConstraint,
) where
import Plutarch
import Plutarch.Prelude
import Plutarch.Lift
import Plutarch.Api.V1.Tuple

class NamedArrow arrow where
  arrowName :: arrow s a b -> String
  mkArrow :: PlutarchArrow s a b -> arrow s a b
  arrow :: arrow s a b -> PlutarchArrow s a b


class NamedUnaryConstraint constraint where
  unaryConstraintName :: constraint s a -> String
  mkUnaryConstraint :: PlutarchUnaryConstraint s a -> constraint s a
  unaryConstraint :: constraint s a -> PlutarchUnaryConstraint s a

class NamedBinaryConstraint constraint where
  binaryConstraintName :: constraint s a b -> String
  mkbinaryConstraint :: PlutarchBinaryConstraint s a b -> constraint s a b
  binaryConstraint :: constraint s a b -> PlutarchBinaryConstraint s a b

type PlutarchArrow s a b = Term s (a :--> b)

-- where POpaque is a truthy value e.g. perror = False, PUnit = True
type PlutarchUnaryConstraint s domain = Term s (domain :--> POpaque)

-- where POpaque is a truthy value e.g. perror = False, PUnit = True
type PlutarchBinaryConstraint s domainL domainR = Term s (domainL :--> domainR :--> POpaque)


data Arrow s a b where
  Arrow :: NamedArrow arrow => arrow s a b -> Arrow s a b
  ArrowSeq :: Arrow s a v -> Arrow s v b -> Arrow s a b
  ArrowFanout :: (PIsData bl, PIsData br, b ~ PTuple bl br, PLifted (PTuple bl br) ~ (PLifted bl, PLifted br))
                 => Arrow s a bl -> Arrow s a br -> Arrow s a b
  ArrowParallel :: (PIsData al, PIsData ar, a ~ PTuple al ar, PLifted (PTuple al ar) ~ (PLifted al, PLifted ar)
                   ,PIsData bl, PIsData br, b ~ PTuple bl br, PLifted (PTuple bl br) ~ (PLifted bl, PLifted br))
                   => Arrow s al bl -> Arrow s ar br -> Arrow s a b

data BinaryConstraint s a b where
  BinaryConstraint :: NamedBinaryConstraint constraint
                      => constraint s a b -> BinaryConstraint s a b
  BinaryConstraintFlip :: BinaryConstraint s b a -> BinaryConstraint s a b
  BinaryConstraintAssoc :: BinaryConstraint s a b -> BinaryConstraint s a b -> BinaryConstraint s a b
  BinaryConstraintArrowLeft :: Arrow s a c -> BinaryConstraint s c b -> BinaryConstraint s a b
  BinaryConstraintArrowRight :: BinaryConstraint s a c -> Arrow s b c -> BinaryConstraint s a b
  BinaryConstraintArrowParallel :: (ab ~ PTuple a b, PLifted (PTuple a b) ~ (PLifted a, PLifted b)
                                      ,v ~ PTuple vl vr, PLifted (PTuple vl vr) ~ (PLifted vl, PLifted vr))
                                  => Arrow s ab v -> BinaryConstraint s vl vr -> BinaryConstraint s a b


data UnaryConstraint s a where
  UnaryConstraint :: NamedUnaryConstraint constraint
                     => constraint s a -> UnaryConstraint s a
  UnaryFromBinaryConstraint :: BinaryConstraint s a a -> UnaryConstraint s a
  UnaryConstraintAssoc :: UnaryConstraint s a -> UnaryConstraint s a -> UnaryConstraint s a
  UnaryConstraintArrow :: Arrow s a b -> UnaryConstraint s b -> UnaryConstraint s a
  UnaryConstraintFanoutBinary ::  (v ~ PTuple vl vr, PLifted (PTuple vl vr) ~ (PLifted vl, PLifted vr))
                                  => Arrow s a v -> BinaryConstraint s vl vr -> UnaryConstraint s a

compileArrow :: Arrow s a b -> PlutarchArrow s a b
compileArrow (Arrow a) = arrow a
compileArrow (ArrowSeq l r) =
  plam $ \a -> papp (compileArrow r) (papp (compileArrow l) a)
compileArrow (ArrowFanout l r) =
  plam $ \a -> papp (papp ptuple (pdata (papp (compileArrow l) a))) (pdata (papp (compileArrow r) a))
compileArrow (ArrowParallel l r) =
  plam $ \alar->
    let bip = pfromData $ pbuiltinPairFromTuple $ pdata alar
        bl = pdata $ papp (compileArrow l) $ pfromData $ papp pfstBuiltin bip
        br = pdata $ papp (compileArrow r) $ pfromData $ papp psndBuiltin bip
      in papp (papp ptuple bl) br


compileBinaryConstraint :: BinaryConstraint s a b -> PlutarchBinaryConstraint s a b
compileBinaryConstraint (BinaryConstraintArrowParallel (ArrowParallel l r) c) =
  compileBinaryConstraint $ BinaryConstraintArrowRight (BinaryConstraintArrowLeft l c) r


compileUnaryConstraint :: UnaryConstraint s a -> PlutarchUnaryConstraint s a
compileUnaryConstraint (UnaryConstraintFanoutBinary (ArrowFanout l r) c) =
  compileUnaryConstraint (UnaryFromBinaryConstraint (BinaryConstraintArrowRight (BinaryConstraintArrowLeft l c) r))




