{-# LANGUAGE RankNTypes #-}
module Apropos.TxArr (
  TxSpec,
  TxArr(..),
  ConstraintBarrow(..),
  (>>>>),
  (<++>),
  ) where
import Apropos.Tx.Barrow
import Apropos.Tx.Constraint
import Prelude hiding ((<>))
import Plutus.V1.Ledger.Tx (Tx)
import Plutarch
import Plutarch.Prelude
import Control.Lens
import Control.Monad ((>=>))

type TxSpec = [TxArr]

data TxArr where
  TxArr :: forall f t s a b . ConstraintBarrow f t s a b -> TxArr

-- ConstraintBarrows compose sequentially
-- from one state to the next
-- on constrained paths
data ConstraintBarrow f t s a b =
  ConstraintBarrow {
    barrow :: Barrow f t
  , constraint :: Constraint s a b
  , flens :: Getter Tx f
  , tlens :: Getter Tx t
  , fiso :: Iso' f (Term s a)
  , tiso :: Iso' t (Term s b)
  } -- this must obey the laws of Iso' (Barrow f t) (Constraint s a b)
    -- but we don't enforce it with this type
    -- barrow is a specification for constraint
    -- that gets checked by a generated test suite

-- sequential arrow composition
(>>>>) :: forall from via to s a b c .
          ConstraintBarrow from via s a b
       -> ConstraintBarrow via to s b c
       -> ConstraintBarrow from to s a c
(>>>>) x y = ConstraintBarrow
             { barrow  = barrow x >=> barrow y
             , constraint = barrowConstraint x y
             , flens = flens x
             , tlens = tlens y
             , fiso = fiso x
             , tiso = tiso y
             }

-- parallel monoidal composition
(<++>) :: forall fromA fromB toC toD s a b c d .
          ConstraintBarrow fromA toC s a c
       -> ConstraintBarrow fromB toD s b d
       -> ConstraintBarrow (fromA,fromB) (toC,toD) s (PBuiltinPair a b) (PBuiltinPair c d)
(<++>) x y = ConstraintBarrow
             { barrow = \(a, b) -> (,) <$> (barrow x) a <*> (barrow y) b
             , constraint = constraint x <&&> constraint y
             , flens = to $ \tx -> (tx ^. (flens x), tx ^. (flens y))
             , tlens = to $ \tx -> (tx ^. (tlens x), tx ^. (tlens y))
             , fiso = pariso (fiso x) (fiso y)
             , tiso = pariso (tiso x) (tiso y)
             }

-- constraints form a monoidal category
(<&&>) :: Constraint s a' b'
       -> Constraint s c' d'
       -> Constraint s (PBuiltinPair a' c') (PBuiltinPair b' d')
(<&&>) xp yp = plam $ \ac ->
                  plam $ \bd ->
                       (papp (papp xp (papp pfstBuiltin ac)) (papp pfstBuiltin bd))
                    <> (papp (papp yp (papp psndBuiltin ac)) (papp psndBuiltin bd))


-- this should be Semigroup.<>
(<>) :: Term s PUnit -> Term s PUnit -> Term s PUnit
(<>) x y = papp (papp (plam $ \_a _b -> pcon PUnit) x) y

-- isos in parallel
pariso :: Iso' a (Term s c) -> Iso' b (Term s d) -> Iso' (a,b)  (Term s (PBuiltinPair c d))
pariso x y = iso (\(fa,fb) -> papp (papp mkPair (fa ^. x)) (fb ^. y))
                 (\faba -> (papp pfstBuiltin faba ^. from x
                           ,papp psndBuiltin faba ^. from y))


-- barrowConstraint is a metaprogramming operation
-- to compose constraints sequentially on chain we would require an on chain barrow
barrowConstraint :: forall from via to s a b c .
                    ConstraintBarrow from via s a b
                 -> ConstraintBarrow via to s b c
                 -> Constraint s a c
barrowConstraint x y = plam $ \a ->
                         plam $ \c -> (papp (papp (constraint x) a) (barrowVia a))
                                   <> (papp (papp (constraint y) (barrowVia a)) c)
  where
    barrowVia :: Term s a -> Term s b
    barrowVia t = case (barrow x) $ (^. from (fiso x)) t of
                    Just so -> (^. tiso x) so
                    Nothing -> perror

-- ?
mkPair :: Term s (a :--> b :--> PBuiltinPair a b)
mkPair = undefined

