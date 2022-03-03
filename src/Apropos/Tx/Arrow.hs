{-# LANGUAGE RankNTypes #-}
module Apropos.Tx.Arrow (
  Protocol,
  TxSpec,
  ClosedTxArrow(..),
  TxArrow(..),
  (>>>>),
  (<++>),
  ) where
import Apropos.Tx.Constraint
import Prelude hiding ((<>))
import Plutus.V1.Ledger.Tx (Tx,TxOut)
import Plutarch
import Plutarch.Prelude
import Control.Lens
import Control.Monad ((>=>))

--TODO
--buildScripts :: Protocol -> ProtocolScripts
--
---- requires DSL for constructing a Protocol that composes the constraints to build the Scripts
---- this should construct the arrow corepresentations of the scripts too
---- we can use the arrow corepresentations to define search routines that look for error conditions
---- these could be things specified by the TxSpecs - contracts not being obeyed
---- or protocol level bugs like unconsumable outputs
--
-- protocol "MyProtocol" $ do
--   tx "MyAction" $ do
--     txtmp "CheckTheFees" $ do
--       txarr checkFeeA
--       txarr checkFeeB
--     txtmp "CheckTheAction" $ do
--       txarr checkTheAction
--   tx "MyOtherAction $ do
--     txtmp "CheckTheFees" $ do
--       txarr checkFeeA
--       txarr checkFeeB
--       txarr checkFeeC
--     txtmp "CheckTheOtherAction" $ do
--       txarr checkTheAction
--   tx "EndMyProtocol" $ do
--     txtmp "CheckTheEndCondition" $ do
--       txarr checkTheEndCondition


type Protocol = [TxSpec]

type TxSpec = [ClosedTxArrow]

data ClosedTxArrow where
  ClosedTxArrow :: forall f t s a b . TxArrow f t s a b -> ClosedTxArrow

-- TxArrows compose sequentially
-- from one state to the next
-- on constrained paths
data TxArrow f t s a b =
  TxArrow {
    txArrow :: f -> Maybe t
  , constraint :: Constraint s a b
  , fget :: Getter Tx (Maybe f)         -- these getters should be a DSL that can be
  , tget :: Getter [TxOut] (Maybe t)    -- interpreted as a Lens.Getter or a Plutarch.Getter
  , fiso :: Iso' f (Term s a)
  , tiso :: Iso' t (Term s b)           -- these ISOs translate between the arr model and constraint
  }
  -- TxArrow must morally obey the laws of Iso (f -> Maybe t) (Constraint s a b)
  -- we don't enforce it with this type though we will check it with stochastic search

-- sequential arrow composition
(>>>>) :: forall from via to s a b c .
          TxArrow from via s a b
       -> TxArrow via to s b c
       -> TxArrow from to s a c
(>>>>) x y = TxArrow
             { txArrow  = txArrow x >=> txArrow y
             , constraint = txArrowConstraint x y
             , fget = fget x
             , tget = tget y
             , fiso = fiso x
             , tiso = tiso y
             }

-- parallel monoidal composition
(<++>) :: forall fromA fromB toC toD s a b c d .
          TxArrow fromA toC s a c
       -> TxArrow fromB toD s b d
       -> TxArrow (fromA,fromB) (toC,toD) s (PBuiltinPair a b) (PBuiltinPair c d)
(<++>) x y = TxArrow
             { txArrow = \(a, b) -> (,) <$> (txArrow x) a <*> (txArrow y) b
             , constraint = constraint x <&&> constraint y
             , fget = to $ \tx -> (,) <$> tx ^. (fget x) <*> tx ^. (fget y)
             , tget = to $ \tx -> (,) <$> tx ^. (tget x) <*> tx ^. (tget y)
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


-- txArrowConstraint is a metaprogramming operation
-- to compose constraints sequentially on chain we would require an on chain txArrow
txArrowConstraint :: forall from via to s a b c .
                    TxArrow from via s a b
                 -> TxArrow via to s b c
                 -> Constraint s a c
txArrowConstraint x y = plam $ \a ->
                         plam $ \c -> (papp (papp (constraint x) a) (txArrowVia a))
                                   <> (papp (papp (constraint y) (txArrowVia a)) c)
  where
    txArrowVia :: Term s a -> Term s b
    txArrowVia t = case (txArrow x) $ (^. from (fiso x)) t of
                    Just so -> (^. tiso x) so
                    Nothing -> perror

-- ?
mkPair :: Term s (a :--> b :--> PBuiltinPair a b)
mkPair = undefined

