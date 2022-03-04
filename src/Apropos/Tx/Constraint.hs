{-# LANGUAGE RankNTypes #-}
module Apropos.Tx.Constraint (
  PlutarchConstraint,
  TxConstraint(..),
  ) where
import Plutarch.Prelude
import Plutarch.Api.V1.Contexts (PScriptContext)
import Plutus.V1.Ledger.Contexts (ScriptContext)


type PlutarchConstraint debruijn domain = Term debruijn (domain :--> PUnit)

data TxConstraint haskDomain debruijn plutarchDomain =
  TxConstraint {
    haskConstraint :: haskDomain -> Bool
  , plutarchConstraint :: PlutarchConstraint debruijn plutarchDomain
  }

-- this is like && for constraints on the same type
instance Semigroup (TxConstraint a debruijn a') where
  (<>) a b = TxConstraint {
                 haskConstraint = \c -> (haskConstraint a) c && (haskConstraint b) c
               , plutarchConstraint = plam $ \c -> (papp (plutarchConstraint a) c)
                                                <> (papp (plutarchConstraint b) c)
               }

instance Monoid (TxConstraint a debruijn a') where
  mempty = TxConstraint {
             haskConstraint = \_ -> True
           , plutarchConstraint = plam $ \_ -> pcon PUnit
           }


-- isomorphic getters in hask and plutarch
data Glasses a b debruijn a' b' =
  Glasses {
    haskLens :: a -> Maybe b
  , plutarchLens :: Term debruijn (a' :--> b')
  }

-- this is like && for constraints of different types that have lenses into a type containing them
-- we should make the glasses part of a monadic context and this op as sequencing for constraints
-- i.e. withGlasses ownInput $ do
--        thisConstraintOnOwnInput
--        thatConstraintOnOwnInput
(<<>>) :: Glasses c a debruijn c' a' -> Glasses c b debruijn c' b'
       -> TxConstraint a debruijn a' -> TxConstraint b debruijn b' -> TxConstraint c debruijn c'
(<<>>) gca gcb ca cb =
  TxConstraint {
    haskConstraint = \c -> case ((haskLens gca) c,(haskLens gcb) c) of
                             (Just ea,Just eb) -> (haskConstraint ca) ea && (haskConstraint cb) eb
                             _ -> False
  , plutarchConstraint = plam $ \c -> (papp (plutarchConstraint ca) (papp (plutarchLens gca) c))
                                   <> (papp (plutarchConstraint cb) (papp (plutarchLens gcb) c))
  }
