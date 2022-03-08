{-# LANGUAGE RankNTypes #-}

module Plutarch.ArrowDSL (
  arrow,
  binaryConstraint,
  unaryConstraint,
  (>>>),
  (&&&),
  (***),
  bflip,
  (>||),
  (||<),
  (>**),
  wye,
  (>>|),
  (>&|),

  Arrow,
  BinaryConstraint,
  UnaryConstraint,
  compileArrow,
  compileBinaryConstraint,
  compileUnaryConstraint,
) where
import Plutarch
import Plutarch.Prelude
import Plutarch.Lift
import Plutarch.Api.V1.Tuple

type PlutarchArrow s a b = Term s (a :--> b)

-- where POpaque is a truthy value e.g. perror = False, PUnit = True
type PlutarchUnaryConstraint s domain = Term s (domain :--> POpaque)

-- where POpaque is a truthy value e.g. perror = False, PUnit = True
type PlutarchBinaryConstraint s domainL domainR = Term s (domainL :--> domainR :--> POpaque)

arrow :: PlutarchArrow s a b -> Arrow s a b
arrow = Arrow

binaryConstraint :: PlutarchBinaryConstraint s a b -> BinaryConstraint s a b
binaryConstraint = BinaryConstraint

unaryConstraint :: PlutarchUnaryConstraint s a -> UnaryConstraint s a
unaryConstraint = UnaryConstraint

(>>>) :: Arrow s a b -> Arrow s b c -> Arrow s a c
(>>>) = ArrowSeq

(&&&) :: (PIsData bl, PIsData br, b ~ PTuple bl br, PLifted (PTuple bl br) ~ (PLifted bl, PLifted br))
      => Arrow s a bl -> Arrow s a br -> Arrow s a b
(&&&) = ArrowFanout

(***) :: (PIsData al, PIsData ar, a ~ PTuple al ar, PLifted (PTuple al ar) ~ (PLifted al, PLifted ar)
         ,PIsData bl, PIsData br, b ~ PTuple bl br, PLifted (PTuple bl br) ~ (PLifted bl, PLifted br))
      => Arrow s al bl -> Arrow s ar br -> Arrow s a b
(***) = ArrowParallel

bflip :: BinaryConstraint s a b -> BinaryConstraint s b a
bflip = BinaryConstraintFlip

instance Semigroup (BinaryConstraint s a b) where
  (<>) = BinaryConstraintAssoc

(>||) :: Arrow s a c -> BinaryConstraint s c b -> BinaryConstraint s a b
(>||) = BinaryConstraintArrowLeft

(||<) :: BinaryConstraint s a c -> Arrow s b c -> BinaryConstraint s a b
(||<) = BinaryConstraintArrowRight

(>**) :: (PIsData a, PIsData b, ab ~ PTuple a b, PLifted (PTuple a b) ~ (PLifted a, PLifted b)
         ,PIsData vl, PIsData vr, v ~ PTuple vl vr, PLifted (PTuple vl vr) ~ (PLifted vl, PLifted vr))
      => Arrow s ab v -> BinaryConstraint s vl vr -> BinaryConstraint s a b
(>**) = BinaryConstraintArrowParallel

wye :: BinaryConstraint s a a -> UnaryConstraint s a
wye = UnaryFromBinaryConstraint

instance Semigroup (UnaryConstraint s a) where
  (<>) = UnaryConstraintAssoc

(>>|) :: Arrow s a b -> UnaryConstraint s b -> UnaryConstraint s a
(>>|) = UnaryConstraintArrow

(>&|) :: (PIsData vl, PIsData vr, v ~ PTuple vl vr, PLifted (PTuple vl vr) ~ (PLifted vl, PLifted vr))
      => Arrow s a v -> BinaryConstraint s vl vr -> UnaryConstraint s a
(>&|) = UnaryConstraintFanoutBinary

data Arrow s a b where
  Arrow :: PlutarchArrow s a b -> Arrow s a b
  ArrowSeq :: Arrow s a v -> Arrow s v b -> Arrow s a b
  ArrowFanout :: (PIsData bl, PIsData br, b ~ PTuple bl br, PLifted (PTuple bl br) ~ (PLifted bl, PLifted br))
                 => Arrow s a bl -> Arrow s a br -> Arrow s a b
  ArrowParallel :: (PIsData al, PIsData ar, a ~ PTuple al ar, PLifted (PTuple al ar) ~ (PLifted al, PLifted ar)
                   ,PIsData bl, PIsData br, b ~ PTuple bl br, PLifted (PTuple bl br) ~ (PLifted bl, PLifted br))
                   => Arrow s al bl -> Arrow s ar br -> Arrow s a b

data BinaryConstraint s a b where
  BinaryConstraint :: PlutarchBinaryConstraint s a b -> BinaryConstraint s a b
  BinaryConstraintFlip :: BinaryConstraint s b a -> BinaryConstraint s a b
  BinaryConstraintAssoc :: BinaryConstraint s a b -> BinaryConstraint s a b -> BinaryConstraint s a b
  BinaryConstraintArrowLeft :: Arrow s a c -> BinaryConstraint s c b -> BinaryConstraint s a b
  BinaryConstraintArrowRight :: BinaryConstraint s a c -> Arrow s b c -> BinaryConstraint s a b
  BinaryConstraintArrowParallel :: (PIsData a, PIsData b, ab ~ PTuple a b, PLifted (PTuple a b) ~ (PLifted a, PLifted b)
                                   ,PIsData vl, PIsData vr, v ~ PTuple vl vr, PLifted (PTuple vl vr) ~ (PLifted vl, PLifted vr))
                                => Arrow s ab v -> BinaryConstraint s vl vr -> BinaryConstraint s a b

data UnaryConstraint s a where
  UnaryConstraint :: PlutarchUnaryConstraint s a -> UnaryConstraint s a
  UnaryFromBinaryConstraint :: BinaryConstraint s a a -> UnaryConstraint s a
  UnaryConstraintAssoc :: UnaryConstraint s a -> UnaryConstraint s a -> UnaryConstraint s a
  UnaryConstraintArrow :: Arrow s a b -> UnaryConstraint s b -> UnaryConstraint s a
  UnaryConstraintFanoutBinary :: (PIsData vl, PIsData vr, v ~ PTuple vl vr, PLifted (PTuple vl vr) ~ (PLifted vl, PLifted vr))
                              => Arrow s a v -> BinaryConstraint s vl vr -> UnaryConstraint s a

compileArrow :: Arrow s a b -> PlutarchArrow s a b
compileArrow (Arrow a) = a
compileArrow (ArrowSeq l r) =
  plam $ \a -> compileArrow r # (compileArrow l # a)
compileArrow (ArrowFanout l r) = -- this reification of the tuple can be avoided by compileUnaryConstraint
  plam $ \a -> (ptuple # pdata (compileArrow l # a)) # (pdata (compileArrow r # a))
compileArrow (ArrowParallel l r) = -- this reification of the tuple can be avoided by compileBinaryConstraint
  plam $ \alar->
    let bip = pfromData $ pbuiltinPairFromTuple $ pdata alar
        bl = pdata $ (compileArrow l) # (pfromData $ pfstBuiltin # bip)
        br = pdata $ (compileArrow r) # (pfromData $ psndBuiltin # bip)
      in ptuple # bl # br

opaqueSemigroup :: Term s a -> Term s b -> Term s POpaque
opaqueSemigroup ign ore = ((plam $ \_ign _ore -> popaque $ pcon PUnit) # ign) # ore

compileBinaryConstraint :: BinaryConstraint s a b -> PlutarchBinaryConstraint s a b
compileBinaryConstraint (BinaryConstraint c) = c
compileBinaryConstraint (BinaryConstraintArrowParallel (ArrowParallel l r) c) = -- this avoids the tuple reification
  compileBinaryConstraint $ BinaryConstraintArrowRight (BinaryConstraintArrowLeft l c) r
compileBinaryConstraint (BinaryConstraintArrowParallel arr c) = --TODO rewrite arrow to ArrowParallel if possible via normalisation
  plam $ \a ->
    plam $ \b ->
      let tup = (compileArrow arr) # ((ptuple # pdata a) # (pdata b))
          bip = pfromData $ pbuiltinPairFromTuple $ pdata tup
          bl = pfromData $ pfstBuiltin # bip
          br = pfromData $ psndBuiltin # bip
       in (compileBinaryConstraint c # bl) # br
compileBinaryConstraint (BinaryConstraintFlip c) =
  plam $ \a ->
    plam $ \b -> (compileBinaryConstraint c # b) # a
compileBinaryConstraint (BinaryConstraintAssoc l r) =
  plam $ \a ->
    plam $ \b -> opaqueSemigroup ((compileBinaryConstraint l # a) # b) ((compileBinaryConstraint r # a) # b)
compileBinaryConstraint (BinaryConstraintArrowLeft arr c) =
  plam $ \a ->
    plam $ \b ->
      (compileBinaryConstraint c # (compileArrow arr # a)) # b
compileBinaryConstraint (BinaryConstraintArrowRight c arr) =
  plam $ \a ->
    plam $ \b ->
      (compileBinaryConstraint c # a) # (compileArrow arr # b)

compileUnaryConstraint :: UnaryConstraint s a -> PlutarchUnaryConstraint s a
compileUnaryConstraint (UnaryConstraint c) = c
compileUnaryConstraint (UnaryConstraintFanoutBinary (ArrowFanout l r) c) = -- this avoids the tuple reification
  compileUnaryConstraint (UnaryFromBinaryConstraint (BinaryConstraintArrowRight (BinaryConstraintArrowLeft l c) r))
compileUnaryConstraint (UnaryFromBinaryConstraint c) =
  plam $ \a -> (compileBinaryConstraint c # a) # a
compileUnaryConstraint (UnaryConstraintAssoc l r) =
  plam $ \a -> opaqueSemigroup (compileUnaryConstraint l # a) (compileUnaryConstraint r # a)
compileUnaryConstraint (UnaryConstraintArrow arr c) =
  plam $ \a -> compileUnaryConstraint c # (compileArrow arr # a)
compileUnaryConstraint (UnaryConstraintFanoutBinary arr c) = --TODO rewrite arr to ArrowFanout if possible via normalisation
  plam $ \a ->
    let tup = compileArrow arr # a
        bip = pfromData $ pbuiltinPairFromTuple $ pdata tup
        bl = pfromData $ pfstBuiltin # bip
        br = pfromData $ psndBuiltin # bip
      in (compileBinaryConstraint c # bl) # br

