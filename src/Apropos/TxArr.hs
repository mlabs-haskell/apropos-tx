{-# LANGUAGE NoImplicitPrelude #-}
module Apropos.TxArr (
  TxArr(..),
  (>>>>),
  (<++>),
  ) where
import Prelude hiding ((<>))
import Plutarch
import Plutarch.Prelude

data TxArr a b s a' b' =
  TxArr {
    -- arr is a specification for how the constraint should behave
    -- (in the constraint emitting model this is "pulls back" free variables such
    -- that the emitted constraints have at most a single solution b)
    arr :: a -> Maybe b
  , coerceIn  :: Term s a' -> a
  , coerceOut :: b -> Term s b'
    -- the constraint that is emitted given the inputs + pulled back free variables
  , con :: Term s (a' :--> b' :--> PUnit)
  }

liftTxArr :: TxArr a b s a' b' -> Term s a' -> Term s b'
liftTxArr x t = case (arr x) $ (coerceIn x) t of
                  Just so -> (coerceOut x) so
                  Nothing -> perror

-- this should be Semigroup.<>
(<>) :: Term s PUnit -> Term s PUnit -> Term s PUnit
(<>) x y = papp (papp (plam $ \_a _b -> pcon PUnit) x) y

-- sequential arrow like composition
(>>>>) :: forall a b c s a' b' c' . TxArr a b s a' b' -> TxArr b c s b' c' -> TxArr a c s a' c'
(>>>>) x y = TxArr {
               arr = \i -> (arr x) i >>= arr y
             , coerceIn = coerceIn x
             , coerceOut = coerceOut y
             , con = ccomp (con x) (con y)
             }
  where
    ccomp :: Term s (a' :--> b' :--> PUnit)
          -> Term s (b' :--> c' :--> PUnit)
          -> Term s (a' :--> c' :--> PUnit)
    ccomp xp yp = plam $ \a' ->
                     plam $ \c' ->
                             (papp (papp xp a') ((liftTxArr x) a'))
                          <> (papp (papp yp ((liftTxArr x) a')) c')


-- parallel monoidal composition
(<++>) :: forall a b c d s a' b' c' d' .
   TxArr a b s a' b' -> TxArr c d s c' d' -> TxArr (a,c) (b,d) s (PBuiltinPair a' c') (PBuiltinPair b' d')
(<++>) x y = TxArr {
               arr = \(a,c) -> (,) <$> arr x a <*> arr y c
             , coerceIn = \ac -> (coerceIn x $ papp pfstBuiltin ac, coerceIn y $ papp psndBuiltin ac)
             , coerceOut = \(b,d) -> papp (papp mkPair (coerceOut x b)) (coerceOut y d)
             , con = ccomp (con x) (con y)
             }
    where
      ccomp :: Term s (a' :--> b' :--> PUnit)
            -> Term s (c' :--> d' :--> PUnit)
            -> Term s (PBuiltinPair a' c' :--> PBuiltinPair b' d' :--> PUnit)
      ccomp xp yp = plam $ \ac ->
                        plam $ \bd ->
                             (papp (papp xp (papp pfstBuiltin ac)) (papp pfstBuiltin bd))
                          <> (papp (papp yp (papp psndBuiltin ac)) (papp psndBuiltin bd))

mkPair :: Term s (a :--> b :--> PBuiltinPair a b)
mkPair = undefined
