{-# LANGUAGE RankNTypes #-}
module Apropos.Tx.Arrow (
  TxArrow(..),
  (>>>>),
  (>>>|),
  (<++>),
  ) where
import Apropos.Tx.Constraint
import Plutarch
import Plutarch.Prelude
import Plutarch.Builtin (ppairDataBuiltin)


type PlutarchArrow debruijn antecedent consequent = Term debruijn (antecedent :--> consequent)

data TxArrow f t s a b =
  TxArrow {
    haskArrow :: f -> t
  , plutarchArrow :: PlutarchArrow s a b
  }

-- sequential arrow composition
(>>>>) :: forall from via to s a b c .
          TxArrow from via s a b
       -> TxArrow via to s b c
       -> TxArrow from to s a c
(>>>>) x y = TxArrow
             { haskArrow  = haskArrow y . haskArrow x
             , plutarchArrow = plam $ \a -> plutarchArrow y # papp (plutarchArrow x) a
             }

-- compose an arrow with a constraint
(>>>|) :: TxArrow from to debruijn a b
       -> TxConstraint to debruijn b
       -> TxConstraint from debruijn a
(>>>|) arr c = TxConstraint {
                 haskConstraint = haskConstraint c . haskArrow arr
               , plutarchConstraint = plam $ \a -> plutarchConstraint c # papp (plutarchArrow arr) a
               }

-- run TxArrows in parallel
(<++>) ::  (PIsData c, PIsData d)
       => TxArrow fromA toC s a c
       -> TxArrow fromB toD s b d
       -> TxArrow (fromA,fromB) (toC,toD) s (PBuiltinPair a b) (PBuiltinPair (PAsData c) (PAsData d))
(<++>) x y = TxArrow
             { haskArrow = \(a, b) -> ((haskArrow x) a, (haskArrow y) b)
             , plutarchArrow = plutarchArrow x `plutarchParArr` plutarchArrow y
             }
  where
    plutarchParArr :: (PIsData b, PIsData d)
                   => PlutarchArrow s a b
                   -> PlutarchArrow s c d
                   -> PlutarchArrow s (PBuiltinPair a c) (PBuiltinPair (PAsData b) (PAsData d))
    plutarchParArr xp yp = plam $ \ac ->
                         papp (papp ppairDataBuiltin (pdata (papp xp (papp pfstBuiltin ac))))
                                                     (pdata (papp yp (papp psndBuiltin ac)))

