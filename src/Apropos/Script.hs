module Apropos.Script (ScriptModel (..)) where

import Apropos.Gen
import Apropos.Gen.Enumerate
import Apropos.Gen.BacktrackingTraversal(traversalAsGen)
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.LogicalModel
import Apropos.Type
import Control.Monad (void)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Hedgehog (
  Group (..),
  Property,
  TestLimit,
  withTests,
  property,
 )
import Plutus.V1.Ledger.Api (ExCPU (..), ExMemory (..))
import Plutus.V1.Ledger.Scripts (Script, ScriptError (..), evaluateScript)
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
  sequence,
  snd,
  zip,
  ($),
  (&&),
  (.),
  (<$>),
  (<=),
  (<>),
  (>=),
 )

class (HasLogicalModel p m, HasParameterisedGenerator p m) => ScriptModel p m where
  expect :: (m :+ p) -> Formula p
  script :: (m :+ p) -> (m -> Script)

  modelMemoryBounds :: (m :+ p) -> m -> (ExMemory, ExMemory)
  modelMemoryBounds _ _ = (ExMemory minBound, ExMemory maxBound)

  modelCPUBounds :: (m :+ p) -> m -> (ExCPU, ExCPU)
  modelCPUBounds _ _ = (ExCPU minBound, ExCPU maxBound)

  runScriptTestsWhere :: m :+ p -> String -> Formula p -> Group
  runScriptTestsWhere apropos name condition =
    Group (fromString name) $
      [ (fromString $ show $ Set.toList scenario, runScriptTest apropos scenario)
      | scenario <- enumerateScenariosWhere condition
      ]


  runScriptTest :: m :+ p -> Set p -> Property
  runScriptTest apropos targetProperties = property . void . forAll $ do
    (m :: m) <- traversalAsGen $ parameterisedGenerator targetProperties
    case evaluateScript $ script apropos m of
      Left (EvaluationError logs err) -> deliverResult apropos m (Left (logs, err))
      Right res -> deliverResult apropos m (Right res)
      Left err -> failWithFootnote (show err)

  enumerateScriptTestsWhere :: m :+ p -> String -> Formula p -> Group
  enumerateScriptTestsWhere apropos name condition =
    Group (fromString name) $
      [ (fromString $ show $ Set.toList scenario, enumerateScriptTest apropos scenario)
      | scenario <- enumerateScenariosWhere condition
      ]

  enumerateScriptTest :: m :+ p -> Set p -> Property
  enumerateScriptTest apropos targetProperties = withTests (1 :: TestLimit) $
    property . void . forAll $ do
      let ms = enumerate $ traversalAsGen $ parameterisedGenerator targetProperties
      let run m = case evaluateScript $ script apropos m of
            Left (EvaluationError logs err) -> deliverResult apropos m (Left (logs, err))
            Right res -> deliverResult apropos m (Right res)
            Left err -> failWithFootnote (show err)
      sequence (run <$> ms)

  deliverResult ::
    m :+ p ->
    m ->
    Either ([Text], String) (ExBudget, [Text]) ->
    Gen ()
  deliverResult apropos model res =
    case (shouldPass, res) of
      (False, Left _) -> pure ()
      (True, Right (cost, _)) -> successWithBudgetCheck cost
      (True, Left err) -> failWithFootnote $ unexpectedFailure err
      (False, Right (_, logs)) -> failWithFootnote $ unexpectedSuccess logs
    where
      shouldPass :: Bool
      shouldPass = satisfiesFormula (expect apropos) $ properties model
      successWithBudgetCheck :: ExBudget -> Gen ()
      successWithBudgetCheck cost@(ExBudget cpu mem) =
        if inInterval cpu (modelCPUBounds apropos model) && inInterval mem (modelMemoryBounds apropos model)
          then pure ()
          else failWithFootnote $ budgetCheckFailure cost
        where
          inInterval :: Ord a => a -> (a, a) -> Bool
          inInterval a (l, u) = a >= l && a <= u
      budgetCheckFailure :: ExBudget -> String
      budgetCheckFailure cost =
        renderStyle ourStyle $
          "Success! But at what cost?"
            $+$ hang "Lower Bound" 4 (ppDoc (ExBudget (fst (modelCPUBounds apropos model)) (fst (modelMemoryBounds apropos model))))
            $+$ hang "Actual Cost" 4 (ppDoc cost)
            $+$ hang "Upper Bound" 4 (ppDoc (ExBudget (snd (modelCPUBounds apropos model)) (snd (modelMemoryBounds apropos model))))
      unexpectedSuccess :: [Text] -> String
      unexpectedSuccess logs =
        renderStyle ourStyle $
          "Unexpected success" $+$ dumpState apropos logs
      unexpectedFailure :: ([Text], String) -> String
      unexpectedFailure (logs, reason) =
        renderStyle ourStyle $
          text ("Unexpected failure(" <> reason <> ")") $+$ dumpState apropos logs
      dumpState :: m :+ p -> [Text] -> Doc
      dumpState _ logs =
        ""
          $+$ hang "Inputs" 4 dumpInputs
          $+$ hang "Logs" 4 (dumpLogs logs)
          $+$ hang "Expected " 4 (if shouldPass then "Pass" else "Fail")
          $+$ hang "Properties " 4 (ppDoc (properties model :: Set p))
      dumpInputs :: Doc
      dumpInputs =
        "Parameters"
          $+$ ppDoc model
      dumpLogs :: [Text] -> Doc
      dumpLogs = vcat . fmap go . zip [1 ..]
      go :: (Int, Text) -> Doc
      go (ix, line) = (PP.int ix <> colon) <+> (text . show $ line)

ourStyle :: Style
ourStyle = style {lineLength = 80}
