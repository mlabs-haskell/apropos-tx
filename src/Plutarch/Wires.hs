{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Plutarch.Wires (
  eqNode,
  notNode,
  notEqDag,
  notEqDagBothExposed,
  ) where
import Plutarch
import Plutarch.Prelude
import GHC.TypeLits
import Data.Proxy

type PNode :: [PType] -> [PLabeledType] -> PType

type family PNode is os where
  PNode (i ': is) os = i :--> (PNode is os)
  PNode '[] os = PDataRecord os

eqNode :: forall i. PEq i => WiringDiagram '[i,i] '["isEq" ':= PBool]
eqNode = Node "eq" $ plam $ \a b -> pdcons @"isEq" @PBool # (pdata (a #== b)) # pdnil

notNode :: WiringDiagram '[PBool] '["negated" ':= PBool]
notNode = Node "not" $ plam $ \a -> pdcons @"negated" @PBool # (pdata (papp pnot a)) # pdnil

notEqDag :: forall i. PEq i => WiringDiagram '[i,i] '["negated" ':= PBool]
notEqDag = Wire (Proxy :: Proxy "isEq") (Proxy :: Proxy 0) eqNode notNode

notEqDagBothExposed :: forall i. PEq i => WiringDiagram '[i,i] '["isEq" ':= PBool, "negated" ':= PBool]
notEqDagBothExposed = Wyre (Proxy :: Proxy "isEq") (Proxy :: Proxy 0) eqNode notNode

type ConsumeIndex :: Nat -> [a] -> [a]

type family ConsumeIndex n ps where
  ConsumeIndex 0 (_ ': ps) = ps
  ConsumeIndex n (p ': ps) = p ': (ConsumeIndex (n - 1) ps)
  ConsumeIndex _ '[] = '[]

type Consume :: Symbol -> [PLabeledType] -> [PLabeledType]
type family Consume s ps where
  Consume s ((s ':= _) ': ps) = ps
  Consume s (p ': ps) = p ': (Consume s ps)
  Consume _ '[] = '[]

type Concat :: [a] -> [a] -> [a]

type family Concat as bs where
  Concat (a ': as) bs = a ': (Concat as bs)
  Concat '[] bs = bs

type Index :: Nat -> [a] -> a

type family Index i as where
  Index 0 (a ': _) = a
  Index n (_ ': as) = Index (n - 1) as

data WiringDiagram is os where
  Node :: String -> Term s (PNode is os) -> WiringDiagram is os
  Wyre :: (is ~ (Concat isa (ConsumeIndex i isb)), os ~ (Concat osa osb))
       => Proxy o -> Proxy i -> WiringDiagram isa osa -> WiringDiagram isb osb -> WiringDiagram is os
  Wire :: (is ~ (Concat isa (ConsumeIndex i isb)), os ~ (Concat (Consume o osa) osb))
       => Proxy o -> Proxy i -> WiringDiagram isa osa -> WiringDiagram isb osb -> WiringDiagram is os

