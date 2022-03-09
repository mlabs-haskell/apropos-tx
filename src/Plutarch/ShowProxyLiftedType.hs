{-# LANGUAGE AllowAmbiguousTypes #-}
module Plutarch.ShowProxyLiftedType (
  showProxyLiftedType,
  ) where
import Plutarch.Lift
import Data.Proxy
import Type.Reflection

-- this works (it prints the type of (PLifted a) but only if you can explicitly tell
-- it what that type is... so not all that useful
showProxyLiftedType :: forall a . Typeable (PLifted a) => Proxy (PLifted a) -> String
showProxyLiftedType a = drop 8 $ show (typeOf a)

