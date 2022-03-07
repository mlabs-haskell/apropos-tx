module Apropos.Script.Iso.Constraint.Runner (
  HasMemoryBounds (..),
  HasCPUBounds (..),
  runConstraintTestsWhere,
) where
import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasResourceBounds
import Apropos.LogicalModel
import Apropos.Script.Iso.Constraint

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

type ConstraintTest p s a =
  ( HasLogicalModel p (PConstantRepr a)
  , HasParameterisedGenerator p (PConstantRepr a)
  , HasMemoryBounds (IsoConstraint s a) a
  , HasCPUBounds (IsoConstraint s a) a
  , PConstant a
  , PLifted (PConstanted a) ~ a
  , PIsData (PConstanted a)
  , PConstantRepr a ~ a
  )

runConstraintTestsWhere :: ConstraintTest p s a => Formula p -> IsoConstraint s a -> String -> Formula p -> Group
runConstraintTestsWhere behaviour constraint name testFilter =
  Group (fromString name) $
    [ (fromString $ show $ Set.toList scenario, runConstraintTest behaviour constraint scenario)
    | scenario <- enumerateScenariosWhere testFilter
    ]

runConstraintTest :: ConstraintTest p s a => Formula p -> IsoConstraint s a -> Set p -> Property
runConstraintTest behaviour constraint targetProperties = genProp $ do
  (f :: f) <- parameterisedGenerator targetProperties
  let testScript = papp (plutarchConstraint constraint) (pdata $ pconstant f)
  case evaluateScript $ compile $ unsafeCoerce testScript of
    Left (EvaluationError logs err) -> deliverResult behaviour constraint f targetProperties (Left (logs, err))
    Right res -> deliverResult behaviour constraint f targetProperties (Right res)
    Left err -> failWithFootnote (show err)

deliverResult ::
  ( HasMemoryBounds (IsoConstraint s a) a
  , HasCPUBounds (IsoConstraint s a) a
  , Show a
  , LogicalModel p
  , PConstant a
  ) =>
  Formula p ->
  IsoConstraint s a ->
  a ->
  Set p ->
  Either ([Text], String) (ExBudget, [Text]) ->
  Gen ()
deliverResult behaviour constraint input inputProps res = do
  let haskRes = haskConstraint constraint (pconstantToRepr input)
      expect = satisfiesFormula behaviour inputProps
  case ((expect, haskRes), res) of
    ((False,False), Left _) -> pure ()
    ((False,False), Right (_, logs)) -> failWithFootnote $ unexpectedSuccess logs
    ((True,True), Right (cost, _)) -> successWithBudgetCheck cost
    ((True,True), Left err) -> failWithFootnote $ unexpectedFailure err
    -- TODO lift -- if we have a spec error we don't need to run the CEK machine
    (specError, _) -> failWithFootnote $ specificationError specError
  where
    successWithBudgetCheck :: ExBudget -> Gen ()
    successWithBudgetCheck cost@(ExBudget cpu mem) =
      if inInterval cpu (cpuBounds constraint input) && inInterval mem (memoryBounds constraint input)
        then pure ()
        else failWithFootnote $ budgetCheckFailure cost
      where
        inInterval :: Ord a => a -> (a, a) -> Bool
        inInterval a (l, u) = a >= l && a <= u
    budgetCheckFailure :: ExBudget -> String
    budgetCheckFailure cost =
      renderStyle ourStyle $
        "Success! But at what cost?"
          $+$ hang "Lower Bound" 4 (ppDoc (ExBudget (fst (cpuBounds constraint input)) (fst (memoryBounds constraint input))))
          $+$ hang "Actual Cost" 4 (ppDoc cost)
          $+$ hang "Upper Bound" 4 (ppDoc (ExBudget (snd (cpuBounds constraint input)) (snd (memoryBounds constraint input))))
    unexpectedFailure :: ([Text], String) -> String
    unexpectedFailure (logs, reason) =
      renderStyle ourStyle $
        text ("Unexpected failure(" <> reason <> ")") $+$ dumpState logs
    unexpectedSuccess :: [Text] -> String
    unexpectedSuccess logs =
      renderStyle ourStyle $
        "Unexpected success" $+$ dumpState logs
    specificationError :: (Bool,Bool) -> String
    specificationError (expected,observed) =
      renderStyle ourStyle $
        "Specification Error"
           $+$ hang "Expected" 4 (ppDoc expected)
           $+$ hang "Observed" 4 (ppDoc observed)
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
