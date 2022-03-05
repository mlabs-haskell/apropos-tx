module Apropos.HasResourceBounds (
  HasMemoryBounds(..),
  HasCPUBounds(..),
  ) where
import Plutus.V1.Ledger.Api (ExCPU (..), ExMemory (..))

class HasMemoryBounds o a where
  memoryBounds :: o -> a -> (ExMemory, ExMemory)
  memoryBounds _ _ = (ExMemory minBound, ExMemory maxBound)

class HasCPUBounds o a where
  cpuBounds :: o -> a -> (ExCPU,ExCPU)
  cpuBounds _ _ = (ExCPU minBound, ExCPU maxBound)

