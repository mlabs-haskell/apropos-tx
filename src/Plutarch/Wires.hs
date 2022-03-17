{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Plutarch.Wires (
  -- examples
  eqNode,
  notNode,
  notEqDag,
  notEqDagBothExposed,
  ) where
import Plutarch
import Plutarch.Prelude
import GHC.TypeLits
import Data.Proxy

type PNode :: [PLabeledType] -> [PLabeledType] -> PType

type family PNode is os where
  PNode ((_ ':= i) ': is) os = i :--> (PNode is os)
  PNode '[] os = PDataRecord os

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
  Wyre :: (is ~ (Concat isa (Consume i isb)), os ~ (Concat osa osb))
       => Proxy o -> Proxy i -> WiringDiagram isa osa -> WiringDiagram isb osb -> WiringDiagram is os
  Wire :: (is ~ (Concat isa (Consume i isb)), os ~ (Concat (Consume o osa) osb))
       => Proxy o -> Proxy i -> WiringDiagram isa osa -> WiringDiagram isb osb -> WiringDiagram is os

-- examples

eqNode :: forall i. PEq i => WiringDiagram '["eqL" ':= i, "eqR" ':= i] '["isEq" ':= PBool]
eqNode = Node "eq" $ plam $ \a b -> pdcons @"isEq" @PBool # (pdata (a #== b)) # pdnil

notNode :: WiringDiagram '["negate" ':= PBool] '["negated" ':= PBool]
notNode = Node "not" $ plam $ \a -> pdcons @"negated" @PBool # (pdata (papp pnot a)) # pdnil

notEqDag :: forall i. PEq i => WiringDiagram '["eqL" ':= i, "eqR" ':= i]  '["negated" ':= PBool]
notEqDag = Wire (Proxy :: Proxy "isEq") (Proxy :: Proxy "negate") eqNode notNode

notEqDagBothExposed :: forall i. PEq i
                    => WiringDiagram '["eqL" ':= i, "eqR" ':= i] '["isEq" ':= PBool, "negated" ':= PBool]
notEqDagBothExposed = Wyre (Proxy :: Proxy "isEq") (Proxy :: Proxy "negate") eqNode notNode


