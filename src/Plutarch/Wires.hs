{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Plutarch.Wires (
  eqNode,
  notNode,
  notEqDag,
  eqNodeWyed,
  notEqDagBothExposed,
  ) where
import Plutarch
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import GHC.TypeLits
import Data.Proxy

data PDataOutputs (as :: [PType]) (s :: S) where
  PDCons ::
    forall x xs s.
    Term s (PAsData x) ->
    (Term s (PDataOutputs xs)) ->
    PDataOutputs (x ': xs) s
  PDNil :: PDataOutputs '[] s

instance PlutusType (PDataOutputs (x ': xs)) where
  type PInner (PDataOutputs (x ': xs)) _ = PBuiltinList PData
  pcon' (PDCons x xs) = pto result
    where
      result :: Term _ (PDataOutputs (x ': xs))
      result = pocons # x # xs
  pmatch' l' f = plet l' $ \l ->
    let x :: Term _ (PAsData x)
        x = punsafeCoerce $ phead # l
        xs :: Term _ (PDataOutputs xs)
        xs = punsafeCoerce $ ptail # l
     in f $ PDCons x xs

instance PlutusType (PDataOutputs '[]) where
  type PInner (PDataOutputs '[]) _ = PBuiltinList PData
  pcon' PDNil = pnil
  pmatch' _ f = f PDNil

pocons :: forall a l s. Term s (PAsData a :--> PDataOutputs l :--> PDataOutputs (a ': l))
pocons = punsafeCoerce $ pcons @PBuiltinList @PData

ponil :: Term s (PDataOutputs '[])
ponil = punsafeCoerce $ pnil @PBuiltinList @PData

type PNode :: [PType] -> [PType] -> PType

type family PNode is os where
  PNode (i ': is) os = i :--> (PNode is os)
  PNode '[] os = PDataOutputs os

eqNode :: forall i. PEq i => WiringDiagram '[i,i] '[PBool]
eqNode = Node "eq" $ plam $ \a b -> pocons @PBool # (pdata (a #== b)) # ponil

notNode :: WiringDiagram '[PBool] '[PBool]
notNode = Node "not" $ plam $ \a -> pocons @PBool # (pdata (papp pnot a)) # ponil

notEqDag :: forall i. PEq i => WiringDiagram '[i,i] '[PBool]
notEqDag = Wire (Proxy :: Proxy 0) (Proxy :: Proxy 0) eqNode notNode

eqNodeWyed :: forall i. PEq i => WiringDiagram '[i,i] '[PBool,PBool]
eqNodeWyed = Wye (Proxy :: Proxy 0) eqNode

notEqDagBothExposed :: forall i. PEq i => WiringDiagram '[i,i] '[PBool,PBool]
notEqDagBothExposed = Wire (Proxy :: Proxy 0) (Proxy :: Proxy 0) eqNodeWyed notNode

type Consume :: Nat -> [a] -> [a]

type family Consume n ps where
  Consume 0 (_ ': ps) = ps
  Consume n (p ': ps) = p ': (Consume (n - 1) ps)
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
  Wye :: (os ~ ((Index o osa) ': osa))
      => Proxy o -> WiringDiagram is osa -> WiringDiagram is os
  Wire :: (is ~ (Concat isa (Consume i isb)), os ~ (Concat (Consume o osa) osb))
       => Proxy o -> Proxy i -> WiringDiagram isa osa -> WiringDiagram isb osb -> WiringDiagram is os

