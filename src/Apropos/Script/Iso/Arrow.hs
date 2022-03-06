{-# LANGUAGE RankNTypes #-}

module Apropos.Script.Iso.Arrow (
  IsoArrow (..),
  (>>>>),
  (>>>|),
  (<++>),
  (&&&&),
) where

import Apropos.Script.Iso.Constraint
import Data.Bifunctor (bimap)
import Plutarch
import Plutarch.Builtin
import Plutarch.Lift
import Plutarch.Prelude

type PlutarchArrow debruijn antecedent consequent = Term debruijn (antecedent :--> consequent)

data IsoArrow s a b = IsoArrow
  { haskArrow :: PConstantRepr a -> PConstantRepr b
  , plutarchArrow :: PlutarchArrow s (PAsData (PConstanted a)) (PAsData (PConstanted b))
  }

-- sequential arrow composition
(>>>>) ::
  forall s a b c.
  IsoArrow s a b ->
  IsoArrow s b c ->
  IsoArrow s a c
(>>>>) x y =
  IsoArrow
    { haskArrow = haskArrow y . haskArrow x
    , plutarchArrow = plam $ \a -> plutarchArrow y # papp (plutarchArrow x) a
    }

-- compose an arrow with a constraint
(>>>|) ::
  IsoArrow s a b ->
  IsoConstraint s b ->
  IsoConstraint s a
(>>>|) arr c =
  IsoConstraint
    { haskConstraint = haskConstraint c . haskArrow arr
    , plutarchConstraint = plam $ \a -> plutarchConstraint c # papp (plutarchArrow arr) a
    }

-- run IsoArrows in parallel
(<++>) ::
  IsoArrow s a c ->
  IsoArrow s b d ->
  IsoArrow s (Tuple a b) (Tuple c d)
(<++>) x y =
  IsoArrow
    { haskArrow = bimap (haskArrow x) (haskArrow y)
    , plutarchArrow = plam $ \ac ->
        pdata
          ( papp
              ( papp
                  ppairDataBuiltin
                  (papp (plutarchArrow x) (papp pfstBuiltin $ pfromData ac))
              )
              (papp (plutarchArrow y) (papp psndBuiltin $ pfromData ac))
          )
    }

(&&&&) ::
  IsoArrow s a b ->
  IsoArrow s a c ->
  IsoArrow s a (Tuple b c)
(&&&&) x y =
  IsoArrow
    { haskArrow = \a -> (haskArrow x a, haskArrow y a)
    , plutarchArrow =
        plam $ \a ->
          pdata
            ( papp
                (papp ppairDataBuiltin (papp (plutarchArrow x) a))
                (papp (plutarchArrow y) a)
            )
    }
