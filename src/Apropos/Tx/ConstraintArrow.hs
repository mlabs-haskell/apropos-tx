{-# LANGUAGE RankNTypes #-}
module Apropos.Tx.ConnectiveImplicationGlasses (
  Protocol,
  TxSpec,
  ClosedConnectiveImplicationGlasses(..),
  ConnectiveImplicationGlasses(..),
  (>>>>),
  (<++>),
  ) where
import Prelude hiding ((<>))
import Plutus.V1.Ledger.Tx (Tx,TxOut)
import Plutarch
import Plutarch.Prelude
import Control.Lens
import Control.Monad ((>=>))

--type Connective s a b = Term s (a :--> b :--> PUnit)
type UniversalConnective operator antecedent consequent = operator antecedent consequent

type LogicalConnective formula universe = formula universe -> formula universe -> formula universe

formulaImplication :: LogicalConnective Formula a
formulaImplication = :->:

type HashConnective antecedent consequent = antecedent -> Maybe consequent

type PlutarchConnective debruijn antecedent consequent =
  Term debruijn (antecedent :--> consequent :--> PUnit)

data ClosedConnectiveImplicationGlasses debruijn where
  ClosedConnectiveImplicationGlasses :: forall f t debruijn a b .
    ConnectiveImplicationGlasses f t debruijn a b -> ClosedConnectiveImplicationGlasses

type GenGetter a b = Getter (Formula a) (Gen b)

-- a pair of isomorphic optics into a type
-- we are sliding between categories here
-- e.g. Hask X Plutarch
class Glasses hask haskCtx domain domainPure domainCtx where
  haskGetter :: Getter haskCtx (Maybe hask)
  domainGetter :: domainCtx domain
  getterIso :: Iso' hask (domainPure domain)

-- a telescope is a dual morphism between logical formulas and inhabitants of
-- a type described by them
-- e.g. genSatisfying X properties
class Telescope formula atoms haskCtx hask where
  generator :: Getter' (formula atoms) (haskCtx hask)
  classifier :: Getter' hask (Set atoms)

-- a connective scope is a triad of representations of a logical connective in 3 categories
-- e.g. Apropos X Hask X Platarch
data ConnectiveScope haskAntecedent domainAntecedent
               haskConsequent domainConsequent
               haskCtx domainPure domainCtx
               formula atoms haskGen
               haskConnective
               domainConnective
   = ConnectiveScope
  { antecedent :: Glasses haskAntecedent haskCtx domainAntecedent domainPure domainCtx
  , consequent :: Glasses haskConsequent haskCtx domainConsequent domainPure domainCtx
  , telescope  :: Telescope formula atoms haskGen haskCtx
  , connectiveFormula :: LogicalConnective formula atoms
  , connectiveHask :: UniversalConnective haskConnective haskAntecedent haskConsequent
  , connectiveDomain :: UniversalConnective domainConnective domainAntecedent domainConsequent
  }

type PlatarchConnectiveScope haskAntecedent haskConsequent atoms
                             debruijn domainAntecedent domainConsequent
                             haskConnective
                             domainConnective
  = ConnectiveScope haskAntecedent domainAntecedent
              haskConsequent domainConsequent
              Tx (PlutarchPure debruijn) (PlutarchScriptContext debruijn)
              Formula atoms Gen
              haskConnective
              domainConnective

plutarchConnectiveScope :: PlutarchAntecedent debruijn haskAntecedent plutarchAntecedent
                        -> PlutarchConsequent debruijn haskConsequent plutarchConsequent
                        -> Telescope Formula atoms Gen Tx
                        -> LogicalConnective Formula atoms -> haskConnective -> plutarchConnective
                        -> PlatarchConnectiveScope haskAntecedent haskConsequent atoms
                                  debruijn plutarchAntecedent plutarchConsequent
                                  (haskAntecedent -> Maybe haskConsequent)
                                  (PlutarchConnective debruijn plutarchAntecedent plutarchConsequent)
plutarchConnectiveScope a c t lc hlc plc =
  ConnectiveScope
  {
    antecedent = a
  , consequent = c
  , telescope = t
  , connectiveFormula = lc
  , connectiveHask = hlc
  , connectiveDomain = plc
  }

plutarchImplicationScope :: PlutarchAntecedent debruijn haskAntecedent plutarchAntecedent
                        -> PlutarchConsequent debruijn haskConsequent plutarchConsequent
                        -> Telescope Formula atoms Gen Tx
                        -> (haskAntecedent -> Maybe haskConsequent)
                        -> (PlutarchConnective debruijn plutarchAntecedent plutarchConsequent)
                        -> PlatarchConnectiveScope haskAntecedent haskConsequent atoms
                                  debruijn plutarchAntecedent plutarchConsequent
                                  (haskAntecedent -> Maybe haskConsequent)
                                  (PlutarchConnective debruijn plutarchAntecedent plutarchConsequent)

plutarchImplicationScope a c t hlc plc = plutarchConnectiveScope a c t (:->:)

intImpliesInt :: PlutarchAntecedent debruijn Int PInteger
              -> PlutarchConsequent debruijn Int PInteger
              -> Telescope Formula IntProp Gen Int
              -> (Int -> Maybe Int)
              -> (PlutarchConnective debruijn PInteger PInteger)
              -> PlatarchConnectiveScope haskAntecedent haskConsequent atoms
                                  debruijn plutarchAntecedent plutarchConsequent
                                  (haskAntecedent -> Maybe haskConsequent)
                                  (PlutarchConnective debruijn plutarchAntecedent plutarchConsequent)
intImpliesInt = plutarchImplicationScope


plutarchConjunctionScope :: PlutarchAntecedent debruijn haskAntecedent plutarchAntecedent
                         -> PlutarchConsequent debruijn haskConsequent plutarchConsequent
                         -> Telescope Formula atoms Gen Tx
                         -> (haskAntecedent -> Maybe haskConsequent)
                         -> (PlutarchConnective debruijn plutarchAntecedent plutarchConsequent)
                         -> PlatarchConnectiveScope haskAntecedent haskConsequent atoms
                                   debruijn plutarchAntecedent plutarchConsequent
                                   (haskAntecedent -> haskConsequent -> Bool)
                                   (PlutarchConnective debruijn plutarchAntecedent plutarchConsequent)

plutarchConjunctionScope a c t hlc plc = plutarchConnectiveScope a c t (:&&:)


type PlutarchScriptContext debruijn domain = Term debruijn ( ScriptContext :--> domain)

type PlutarchPure debruijn = Term debruijn

type PlutarchAntecedent debruijn hask plutarch =
  Glasses hask Tx plutarch (Term debruijn) (PlutarchDomain debruijn data)

type PlutarchConsequent debruijn hask plutarch =
  Glasses hask [TxOut] plutarch (Term debruijn) (PlutarchDomain debruijn data)

-- ConnectiveImplicationGlassess compose sequentially
-- from one state to the next
-- on constrained paths
data ConnectiveImplicationGlasses formula haskAntecedent domainAntecedent haskConsequent domainConsequent =
  ConnectiveImplicationGlasses {
    constraint :: Connective formula domainAntecedent domainConsequent
  , implication :: haskAntecedent -> Maybe haskConsequent
  , antecedent :: Antecedent debruijn haskAntecedent domainAntecedent
  , consequent :: Consequent debruijn haskConsequent domainConsequent
  }
  -- ConnectiveImplicationGlasses must morally obey the laws of Iso (f -> Maybe t) (Connective s a b)
  -- we don't enforce it with this type though we will check it with stochastic search

-- sequential arrow composition
(>>>>) :: forall from via to s a b c .
          ConnectiveImplicationGlasses from via s a b
       -> ConnectiveImplicationGlasses via to s b c
       -> ConnectiveImplicationGlasses from to s a c
(>>>>) x y = ConnectiveImplicationGlasses
             { implication  = implication x >=> implication y
             , constraint = implicationConnective x y
             , fget = fget x
             , tget = tget y
             , aiso = aiso x
             , ciso = ciso y
             }

-- parallel monoidal composition
(<++>) :: forall fromA fromB toC toD s a b c d .
          ConnectiveImplicationGlasses fromA toC s a c
       -> ConnectiveImplicationGlasses fromB toD s b d
       -> ConnectiveImplicationGlasses (fromA,fromB) (toC,toD) s (PBuiltinPair a b) (PBuiltinPair c d)
(<++>) x y = ConnectiveImplicationGlasses
             { implication = \(a, b) -> (,) <$> (implication x) a <*> (implication y) b
             , constraint = constraint x <&&> constraint y
             , fget = to $ \tx -> (,) <$> tx ^. (fget x) <*> tx ^. (fget y)
             , tget = to $ \tx -> (,) <$> tx ^. (tget x) <*> tx ^. (tget y)
             , aiso = pariso (aiso x) (aiso y)
             , ciso = pariso (ciso x) (ciso y)
             }

-- constraints form a monoidal category
(<&&>) :: Connective s a' b'
       -> Connective s c' d'
       -> Connective s (PBuiltinPair a' c') (PBuiltinPair b' d')
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


-- implicationConnective is a metaprogramming operation
-- to compose constraints sequentially on chain we would require an on chain implication
implicationConnective :: forall from via to s a b c .
                    ConnectiveImplicationGlasses from via s a b
                 -> ConnectiveImplicationGlasses via to s b c
                 -> Connective s a c
implicationConnective x y = plam $ \a ->
                         plam $ \c -> (papp (papp (constraint x) a) (implicationVia a))
                                   <> (papp (papp (constraint y) (implicationVia a)) c)
  where
    implicationVia :: Term s a -> Term s b
    implicationVia t = case (implication x) $ (^. from (aiso x)) t of
                    Just so -> (^. ciso x) so
                    Nothing -> perror

-- ?
mkPair :: Term s (a :--> b :--> PBuiltinPair a b)
mkPair = undefined

