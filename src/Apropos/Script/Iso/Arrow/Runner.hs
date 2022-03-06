module Apropos.Script.Iso.Arrow.Runner (
  HasMemoryBounds (..),
  HasCPUBounds (..),
  runArrowTestsWhere,
) where
import Apropos.Script.Iso.Arrow
import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasResourceBounds
import Apropos.LogicalModel

import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Hedgehog (
  Group (..),
  Property,
 )
import Plutarch
import Plutarch.Lift
import Plutarch.Prelude
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
import Unsafe.Coerce

type ArrowTest p s a b =
  ( HasLogicalModel p (PConstantRepr a)
  , HasParameterisedGenerator p (PConstantRepr a)
  , HasMemoryBounds (TxArrow s a b) a
  , HasCPUBounds (TxArrow s a b) a
  , PConstant a
  , PConstant b
  , PLifted (PConstanted a) ~ a
  , PLifted (PConstanted b) ~ b
  , PIsData (PConstanted a)
  , PIsData (PConstanted b)
  , PConstantRepr a ~ a
  , PConstantRepr b ~ b
  , PEq (PConstanted b)
  )

runArrowTestsWhere :: ArrowTest p s a b => TxArrow s a b -> String -> Formula p -> Group
runArrowTestsWhere arrow name condition =
  Group (fromString name) $
    [ (fromString $ show $ Set.toList scenario, runArrowTest arrow scenario)
    | scenario <- enumerateScenariosWhere condition
    ]

runArrowTest :: ArrowTest p s a b => TxArrow s a b -> Set p -> Property
runArrowTest arrow targetProperties = genProp $ do
  (f :: f) <- parameterisedGenerator targetProperties
  let expect = haskArrow arrow f
      testScript =
        pif
          (pdata (pconstant expect) #== papp (plutarchArrow arrow) (pdata $ pconstant f))
          (pcon PUnit)
          perror
  case evaluateScript $ compile $ unsafeCoerce testScript of
    Left (EvaluationError logs err) -> deliverResult arrow f targetProperties (Left (logs, err))
    Right res -> deliverResult arrow f targetProperties (Right res)
    Left err -> failWithFootnote (show err)

deliverResult ::
  ( HasMemoryBounds (TxArrow s a b) a
  , HasCPUBounds (TxArrow s a b) a
  , Show a
  , Show p
  ) =>
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
