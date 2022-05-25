{-# LANGUAGE AllowAmbiguousTypes #-}

module Apropos.Script (ScriptModel (..)) where

import Apropos.Gen
import Apropos.Gen.Enumerate
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.LogicalModel

import Control.Monad (void, (=<<))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Hedgehog (
  Group (..),
  Property,
  TestLimit,
  property,
  withTests,
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

class (Enumerable p, HasLogicalModel p m, HasParameterisedGenerator p m) => ScriptModel p m where
  expect :: Formula p
  script :: (m -> Script)

  modelMemoryBounds :: m -> (ExMemory, ExMemory)
  modelMemoryBounds _ = (ExMemory minBound, ExMemory maxBound)

  modelCPUBounds :: m -> (ExCPU, ExCPU)
  modelCPUBounds _ = (ExCPU minBound, ExCPU maxBound)

  runScriptTestsWhere :: String -> Formula p -> Group
  runScriptTestsWhere name condition =
    Group (fromString name) $
      [ (fromString $ show $ Set.toList scenario, runScriptTest scenario)
      | scenario <- enumerateScenariosWhere condition
      ]

  runScriptTest :: Set p -> Property
  runScriptTest targetProperties = property $
    (errorHandler =<<) $
      runGenModifiable $
        forAll $ do
          (m :: m) <- parameterisedGenerator targetProperties
          case evaluateScript $ script @p m of
            Left (EvaluationError logs err) -> deliverResult @p m (Left (logs, err))
            Right res -> deliverResult @p m (Right res)
            Left err -> failWithFootnote (show err)

  enumerateScriptTestsWhere :: String -> Formula p -> Group
  enumerateScriptTestsWhere name condition =
    Group (fromString name) $
      [ (fromString $ show $ Set.toList scenario, enumerateScriptTest scenario)
      | scenario <- enumerateScenariosWhere condition
      ]

  enumerateScriptTest :: Set p -> Property
  enumerateScriptTest targetProperties = withTests (1 :: TestLimit) $
    property $
      (errorHandler =<<) $
        runGenModifiable $
          void $
            forAll $ do
              let ms = enumerate $ parameterisedGenerator targetProperties
              let run m = case evaluateScript $ script @p m of
                    Left (EvaluationError logs err) -> deliverResult @p m (Left (logs, err))
                    Right res -> deliverResult @p m (Right res)
                    Left err -> failWithFootnote (show err)
              sequence (run <$> ms)

  deliverResult ::
    m ->
    Either ([Text], String) (ExBudget, [Text]) ->
    Gen ()
  deliverResult model res =
    case (shouldPass, res) of
      (False, Left _) -> pure ()
      (True, Right (cost, _)) -> successWithBudgetCheck cost
      (True, Left err) -> failWithFootnote $ unexpectedFailure err
      (False, Right (_, logs)) -> failWithFootnote $ unexpectedSuccess logs
    where
      shouldPass :: Bool
      shouldPass = satisfiesFormula @p expect $ properties model
      successWithBudgetCheck :: ExBudget -> Gen ()
      successWithBudgetCheck cost@(ExBudget cpu mem) =
        if inInterval cpu (modelCPUBounds @p model) && inInterval mem (modelMemoryBounds @p model)
          then pure ()
          else failWithFootnote $ budgetCheckFailure cost
        where
          inInterval :: Ord a => a -> (a, a) -> Bool
          inInterval a (l, u) = a >= l && a <= u
      budgetCheckFailure :: ExBudget -> String
      budgetCheckFailure cost =
        renderStyle ourStyle $
          "Success! But at what cost?"
            $+$ hang "Lower Bound" 4 (ppDoc (ExBudget (fst (modelCPUBounds @p model)) (fst (modelMemoryBounds @p model))))
            $+$ hang "Actual Cost" 4 (ppDoc cost)
            $+$ hang "Upper Bound" 4 (ppDoc (ExBudget (snd (modelCPUBounds @p model)) (snd (modelMemoryBounds @p model))))
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
