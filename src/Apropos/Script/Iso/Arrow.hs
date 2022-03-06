{-# LANGUAGE RankNTypes #-}

module Apropos.Script.Iso.Arrow (
  TxArrow (..),
  (>>>>),
  (>>>|),
  (<++>),
  (&&&&),
) where

import Apropos.Script.Iso.Constraint
import Plutarch
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Builtin
import Data.Bifunctor (bimap)

type PlutarchArrow debruijn antecedent consequent = Term debruijn (antecedent :--> consequent)

data TxArrow s a b = TxArrow
  { haskArrow :: PConstantRepr a -> PConstantRepr b
  , plutarchArrow :: PlutarchArrow s (PAsData (PConstanted a)) (PAsData (PConstanted b))
  }

-- sequential arrow composition
(>>>>) ::
  forall s a b c.
  TxArrow s a b ->
  TxArrow s b c ->
  TxArrow s a c
(>>>>) x y =
  TxArrow
    { haskArrow = haskArrow y . haskArrow x
    , plutarchArrow = plam $ \a -> plutarchArrow y # papp (plutarchArrow x) a
    }

-- compose an arrow with a constraint
(>>>|) ::
  TxArrow s a b ->
  TxConstraint s b ->
  TxConstraint s a
(>>>|) arr c =
  TxConstraint
    { haskConstraint = haskConstraint c . haskArrow arr
    , plutarchConstraint = plam $ \a -> plutarchConstraint c # papp (plutarchArrow arr) a
    }

-- run TxArrows in parallel
(<++>) ::
  TxArrow s a c ->
  TxArrow s b d ->
  TxArrow s (Tuple a b) (Tuple c d)
(<++>) x y =
  TxArrow
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
  TxArrow s a b ->
  TxArrow s a c ->
  TxArrow s a (Tuple b c)
(&&&&) x y =
  TxArrow
    { haskArrow = \a -> (haskArrow x a, haskArrow y a)
    , plutarchArrow =
        plam $ \a ->
            pdata
              ( papp
                  (papp ppairDataBuiltin (papp (plutarchArrow x) a))
                  (papp (plutarchArrow y) a)
              )
    }
