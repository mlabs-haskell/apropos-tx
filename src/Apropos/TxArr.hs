module Apropos.TxArr (
  TxArr(..),
  (>>>>)
  ) where
import Plutarch
import Plutarch.Prelude

data TxArr s a b =
  TxArr {
    arr :: Term s a -> Term s b
  , con :: Term s (a :--> b :--> PUnit)
  }


(>>>>) :: forall s a b c . TxArr s a b -> TxArr s b c -> TxArr s a c
(>>>>) x y = TxArr ((arr y) . (arr x)) (ccomp (con x) (con y))
  where
    ccomp :: Term s (a :--> b :--> PUnit)
          -> Term s (b :--> c :--> PUnit)
          -> Term s (a :--> c :--> PUnit)
    ccomp xp yp = plam $ \a' ->
                     plam $ \c' ->
                          (papp (papp xp a') ((arr x) a'))
                       <> (papp (papp yp ((arr x) a')) c')
