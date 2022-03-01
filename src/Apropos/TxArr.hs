module Apropos.TxArr (
  TxArr(..),
  (>>>>),
  (<++>),
  ) where
import Plutarch
import Plutarch.Prelude
import Plutarch.Api.V1

data TxArr a b s a' b' =
  TxArr {
    arr :: a -> b
  , coerceIn  :: Term s a' -> a
  , coerceOut :: b -> Term s b'
  , con :: Term s (a' :--> b' :--> PUnit)
  }

liftTxArr :: TxArr a b s a' b' -> Term s a' -> Term s b'
liftTxArr x = (coerceOut x) . (arr x) . (coerceIn x)

und :: Term s PUnit -> Term s PUnit -> Term s PUnit
und x y = papp (papp (plam $ \_ -> plam $ \_ -> pcon PUnit) x) y

(>>>>) :: forall a b c s a' b' c' . TxArr a b s a' b' -> TxArr b c s b' c' -> TxArr a c s a' c'
(>>>>) x y = TxArr {
               arr = ((arr y) . (arr x))
             , coerceIn = coerceIn x
             , coerceOut = coerceOut y
             , con = (ccomp (con x) (con y))
             }
  where
    ccomp :: Term s (a' :--> b' :--> PUnit)
          -> Term s (b' :--> c' :--> PUnit)
          -> Term s (a' :--> c' :--> PUnit)
    ccomp xp yp = plam $ \a' ->
                     plam $ \c' ->
                             (papp (papp xp a') ((liftTxArr x) a'))
                       `und` (papp (papp yp ((liftTxArr x) a')) c')

(<++>) :: forall a b c d s a' b' c' d' .
   TxArr a b s a' b' -> TxArr c d s c' d' -> TxArr (a,c) (b,d) s (PTuple a' c') (PTuple b' d')
(<++>) _ _ = undefined

