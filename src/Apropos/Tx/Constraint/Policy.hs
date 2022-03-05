module Apropos.Tx.Constraint.Policy (
  ConstraintPolicy (..),
) where

import Plutus.V1.Ledger.Scripts (MintingPolicy)
import Plutus.V1.Ledger.Value (CurrencySymbol)

-- constraint policies are invoked by minting or burning the currency symbol
data ConstraintPolicy = ConstraintPolicy CurrencySymbol MintingPolicy

-- e.g.
--myConstraint :: TxConstraint ScriptContext debruijn PScriptContext
--myConstraint = undefined
--
--myConstraintPolicy :: ConstraintPolicy
--myConstraintPolicy = ConstraintPolicy cs mp
--  where mplam :: ClosedTerm PMintingPolicy
--        -- we can branch on constraints here with the redeemer arg
--        mplam = plam $ \_ ctx -> papp (plutarchConstraint myConstraint) ctx
--        mp = mkMintingPolicy mplam
--        cs = mintingPolicySymbol mp
