{-# LANGUAGE RankNTypes #-}
module Apropos.Tx.Arrow (
  TxArrow(..),
  (>>>>),
  (>>>|),
--  (<++>),
  ) where
import Apropos.Tx.Constraint
import Plutarch
import Plutarch.Lift
--import Plutarch.Prelude
--import Plutarch.Api.V1.Tuple
--import Plutarch.Builtin (ppairDataBuiltin)


type PlutarchArrow debruijn antecedent consequent = Term debruijn (antecedent :--> consequent)

data TxArrow s a b =
  TxArrow {
    haskArrow :: PConstantRepr a -> PConstantRepr b
  , plutarchArrow :: PlutarchArrow s (PConstanted a) (PConstanted b)
  }

-- sequential arrow composition
(>>>>) :: forall s a b c .
          TxArrow s a b
       -> TxArrow s b c
       -> TxArrow s a c
(>>>>) x y = TxArrow
             { haskArrow  = haskArrow y . haskArrow x
             , plutarchArrow = plam $ \a -> plutarchArrow y # papp (plutarchArrow x) a
             }

-- compose an arrow with a constraint
(>>>|) :: TxArrow s a b
       -> TxConstraint s b
       -> TxConstraint s a
(>>>|) arr c = TxConstraint {
                 haskConstraint = haskConstraint c . haskArrow arr
               , plutarchConstraint = plam $ \a -> plutarchConstraint c # papp (plutarchArrow arr) a
               }

---- run TxArrows in parallel
---- TODO redundant pairing? can we remove somehow after composition?
--(<++>) :: TxArrow s a c
--       -> TxArrow s b d
--       -> TxArrow s (a,b) (c,d)
--(<++>) x y = TxArrow
--             { haskArrow = \(a, b) -> ((haskArrow x) a, (haskArrow y) b)
--             , plutarchArrow = plutarchArrow x `plutarchParArr` plutarchArrow y
--             }
--  where
--    plutarchParArr :: (PIsData b, PIsData d)
--                   => PlutarchArrow s a b
--                   -> PlutarchArrow s c d
--                   -> PlutarchArrow s (PBuiltinPair a c) (PBuiltinPair (PAsData b) (PAsData d))
--    plutarchParArr xp yp = plam $ \ac ->
--                             plet (pbuiltinPairFromTuple ac) $ \ac' ->
--                         papp (papp ptuple (papp xp (papp pfstBuiltin ac')))
--                                           (papp yp (papp psndBuiltin ac'))

