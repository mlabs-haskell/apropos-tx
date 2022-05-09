module Apropos.Script (
  ScriptModel (..),
  runScriptTestsWhere,
  runScriptTest,
  enumerateScriptTestsWhere,
  enumerateScriptTest,
) where

import Apropos.Gen
import Apropos.Gen.Enumerate
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.LogicalModel
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Hedgehog (
  Group (..),
  Property,
  TestLimit,
  withTests,
 )
import Hedgehog.Internal.Property (property)
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
  Either (..),
  Int,
  Monad ((>>=)),
  Ord,
  Show (..),
  String,
  fmap,
  fst,
  pure,
  sequence_,
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

data ScriptModel p m = ScriptModel
  { expect :: Formula p
  , script :: m -> Script
  , memoryBounds :: (ExMemory, ExMemory)
  , cpuBounds :: (ExCPU, ExCPU)
  }

runScriptTestsWhere ::
  forall p m.
  (HasParameterisedGenerator p m) =>
  ScriptModel p m ->
  String ->
  Formula p ->
  Group
runScriptTestsWhere sm name condition =
  Group (fromString name) $
    [ (fromString $ show $ Set.toList scenario, runScriptTest sm scenario)
    | scenario <- enumerateScenariosWhere condition
    ]

runScriptTest ::
  forall p m.
  (HasParameterisedGenerator p m) =>
  ScriptModel p m ->
  Set p ->
  Property
runScriptTest sm@ScriptModel {..} targetProperties = property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      (m :: m) <- parameterisedGenerator @p targetProperties
      case evaluateScript $ script m of
        Left (EvaluationError logs err) -> deliverResult sm m (Left (logs, err))
        Right res -> deliverResult sm m (Right res)
        Left err -> failWithFootnote (show err)

enumerateScriptTestsWhere ::
  forall p m.
  HasParameterisedGenerator p m =>
  ScriptModel p m ->
  String ->
  Formula p ->
  Group
enumerateScriptTestsWhere sm name condition =
  Group (fromString name) $
    [ (fromString $ show $ Set.toList scenario, enumerateScriptTest sm scenario)
    | scenario <- enumerateScenariosWhere condition
    ]

enumerateScriptTest ::
  forall p m.
  HasParameterisedGenerator p m =>
  ScriptModel p m ->
  Set p ->
  Property
enumerateScriptTest sm@ScriptModel {..} targetProperties = withTests (1 :: TestLimit) $ property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      let ms = enumerate $ parameterisedGenerator @p targetProperties
      let run m = case evaluateScript $ script m of
            Left (EvaluationError logs err) -> deliverResult sm m (Left (logs, err))
            Right res -> deliverResult sm m (Right res)
            Left err -> failWithFootnote (show err)
      sequence_ (run <$> ms)

deliverResult ::
  forall p m.
  (HasLogicalModel p m, Show m) =>
  ScriptModel p m ->
  m ->
  Either ([Text], String) (ExBudget, [Text]) ->
  Gen ()
deliverResult ScriptModel {..} model res =
  case (shouldPass, res) of
    (False, Left _) -> pure ()
    (True, Right (cost, _)) -> successWithBudgetCheck cost
    (True, Left err) -> failWithFootnote $ unexpectedFailure err
    (False, Right (_, logs)) -> failWithFootnote $ unexpectedSuccess logs
  where
    shouldPass :: Bool
    shouldPass = satisfiesFormula expect $ properties model
    successWithBudgetCheck :: ExBudget -> Gen ()
    successWithBudgetCheck cost@(ExBudget cpu mem) =
      if inInterval cpu cpuBounds && inInterval mem memoryBounds
        then pure ()
        else failWithFootnote $ budgetCheckFailure cost
      where
        inInterval :: Ord a => a -> (a, a) -> Bool
        inInterval a (l, u) = a >= l && a <= u
    budgetCheckFailure :: ExBudget -> String
    budgetCheckFailure cost =
      renderStyle ourStyle $
        "Success! But at what cost?"
          $+$ hang "Lower Bound" 4 (ppDoc (ExBudget (fst cpuBounds) (fst memoryBounds)))
          $+$ hang "Actual Cost" 4 (ppDoc cost)
          $+$ hang "Upper Bound" 4 (ppDoc (ExBudget (snd cpuBounds) (snd memoryBounds)))
    unexpectedSuccess :: [Text] -> String
    unexpectedSuccess logs =
      renderStyle ourStyle $
        "Unexpected success" $+$ dumpState logs
    unexpectedFailure :: ([Text], String) -> String
    unexpectedFailure (logs, reason) =
      renderStyle ourStyle $
        text ("Unexpected failure(" <> reason <> ")") $+$ dumpState logs
    dumpState :: [Text] -> Doc
    dumpState logs =
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
