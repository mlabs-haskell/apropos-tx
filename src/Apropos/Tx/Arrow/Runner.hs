module Apropos.Tx.Arrow.Runner (
  HasMemoryBounds(..),
  HasCPUBounds(..),
  runArrowTestsWhere,
  ) where
import Apropos.Tx.Arrow
import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.LogicalModel
import Plutarch
import Plutarch.Prelude
import Plutarch.Lift
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Hedgehog (
  Group (..),
  Property,
 )
import Plutus.V1.Ledger.Api (ExCPU (..), ExMemory (..))
import Plutus.V1.Ledger.Scripts (ScriptError (..), evaluateScript)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import Text.PrettyPrint (
  Doc,
  Style (lineLength),
  colon,
  hang,
  renderStyle,
  style,
  text,
  vcat,
  ($+$),
  (<+>),
 )
import Text.PrettyPrint qualified as PP
import Text.Show.Pretty (ppDoc)
import Prelude (
  Bool (..),
  Bounded (..),
  Either (..),
  Int,
  Ord,
  Show (..),
  String,
  fmap,
  fst,
  pure,
  snd,
  zip,
  ($),
  (&&),
  (.),
  (<=),
  (<>),
  (>=),
 )
import Unsafe.Coerce

class HasMemoryBounds o a where
  memoryBounds :: o -> a -> (ExMemory, ExMemory)
  memoryBounds _ _ = (ExMemory minBound, ExMemory maxBound)

class HasCPUBounds o a where
  cpuBounds :: o -> a -> (ExCPU,ExCPU)
  cpuBounds _ _ = (ExCPU minBound, ExCPU maxBound)


--class (HasLogicalModel p f, HasParameterisedGenerator p f,HasMemoryBounds (TxArrow f t s a b) f
--      ,HasCPUBounds (TxArrow f t s a b) f)
--  => HasArrowRunner p f t s a b where
--  arrow :: (f :+ p) -> TxArrow f t s a b

type ArrowModel p s a b = ( HasLogicalModel p (PConstantRepr a)
                          , HasParameterisedGenerator p (PConstantRepr a)
                          , HasMemoryBounds (TxArrow s a b) a
                          , HasCPUBounds (TxArrow s a b) a
                          , PConstant a
                          , PConstant b
                          , PLifted (PConstanted a) ~ a
                          , PLifted (PConstanted b) ~ b
                          , PConstantRepr a ~ a
                          , PConstantRepr b ~ b
                          , PEq (PConstanted b)
                          )


runArrowTestsWhere :: ArrowModel p s a b => TxArrow s a b -> String -> Formula p -> Group
runArrowTestsWhere arrow name condition =
  Group (fromString name) $
    [ (fromString $ show $ Set.toList scenario, runArrowTest arrow scenario)
    | scenario <- enumerateScenariosWhere condition
    ]

runArrowTest :: ArrowModel p s a b => TxArrow s a b -> Set p -> Property
runArrowTest arrow targetProperties = genProp $ do
  (f :: f) <- parameterisedGenerator targetProperties
  let expect = (haskArrow arrow) f
      testScript = pconstant expect #== papp (plutarchArrow arrow) (pconstant f)
  case evaluateScript $ compile $ unsafeCoerce testScript of
    Left (EvaluationError logs err) -> deliverResult arrow f targetProperties (Left (logs, err))
    Right res -> deliverResult arrow f targetProperties (Right res)
    Left err -> failWithFootnote (show err)


deliverResult ::
  ( HasMemoryBounds (TxArrow s a b) a
  , HasCPUBounds (TxArrow s a b) a
  , Show a, Show p) =>
  TxArrow s a b ->
  a ->
  Set p ->
  Either ([Text], String) (ExBudget, [Text]) ->
  Gen ()
deliverResult arrow input inputProps res =
  case res of
    (Right (cost, _)) -> successWithBudgetCheck cost
    (Left err) -> failWithFootnote $ unexpectedFailure err
  where
    successWithBudgetCheck :: ExBudget -> Gen ()
    successWithBudgetCheck cost@(ExBudget cpu mem) =
      if inInterval cpu (cpuBounds arrow input) && inInterval mem (memoryBounds arrow input)
        then pure ()
        else failWithFootnote $ budgetCheckFailure cost
      where
        inInterval :: Ord a => a -> (a, a) -> Bool
        inInterval a (l, u) = a >= l && a <= u
    budgetCheckFailure :: ExBudget -> String
    budgetCheckFailure cost =
      renderStyle ourStyle $
        "Success! But at what cost?"
          $+$ hang "Lower Bound" 4 (ppDoc (ExBudget (fst (cpuBounds arrow input)) (fst (memoryBounds arrow input))))
          $+$ hang "Actual Cost" 4 (ppDoc cost)
          $+$ hang "Upper Bound" 4 (ppDoc (ExBudget (snd (cpuBounds arrow input)) (snd (memoryBounds arrow input))))
    unexpectedFailure :: ([Text], String) -> String
    unexpectedFailure (logs, reason) =
      renderStyle ourStyle $
        text ("Unexpected failure(" <> reason <> ")") $+$ dumpState logs
    dumpState :: [Text] -> Doc
    dumpState logs =
      ""
        $+$ hang "Inputs" 4 dumpInputs
        $+$ hang "Logs" 4 (dumpLogs logs)
        $+$ hang "Properties " 4 (ppDoc inputProps)
    dumpInputs :: Doc
    dumpInputs =
      "Parameters"
        $+$ ppDoc input
    dumpLogs :: [Text] -> Doc
    dumpLogs logs = vcat . fmap go . zip [1 ..] $ logs
    go :: (Int, Text) -> Doc
    go (ix, line) = (PP.int ix <> colon) <+> (text . show $ line)

ourStyle :: Style
ourStyle = style {lineLength = 80}
