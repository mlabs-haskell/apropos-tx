module Apropos.Tx.Constraint.Runner (
  HasMemoryBounds(..),
  HasCPUBounds(..),
  runConstraintTestsWhere,
  ) where
import Apropos.HasResourceBounds
import Apropos.Tx.Constraint
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

type ConstraintTest p s a = ( HasLogicalModel p (PConstantRepr a)
                          , HasParameterisedGenerator p (PConstantRepr a)
                          , HasMemoryBounds (TxConstraint s a) a
                          , HasCPUBounds (TxConstraint s a) a
                          , PConstant a
                          , PLifted (PConstanted a) ~ a
                          , PConstantRepr a ~ a
                          )


runConstraintTestsWhere :: ConstraintTest p s a => TxConstraint s a -> String -> Formula p -> Group
runConstraintTestsWhere constraint name condition =
  Group (fromString name) $
    [ (fromString $ show $ Set.toList scenario, runConstraintTest constraint scenario)
    | scenario <- enumerateScenariosWhere condition
    ]

runConstraintTest :: ConstraintTest p s a => TxConstraint s a -> Set p -> Property
runConstraintTest constraint targetProperties = genProp $ do
  (f :: f) <- parameterisedGenerator targetProperties
  let testScript = papp (plutarchConstraint constraint) (pconstant f)
  case evaluateScript $ compile $ unsafeCoerce testScript of
    Left (EvaluationError logs err) -> deliverResult constraint f targetProperties (Left (logs, err))
    Right res -> deliverResult constraint f targetProperties (Right res)
    Left err -> failWithFootnote (show err)


deliverResult ::
  ( HasMemoryBounds (TxConstraint s a) a
  , HasCPUBounds (TxConstraint s a) a
  , Show a, Show p,PConstant a) =>
  TxConstraint s a ->
  a ->
  Set p ->
  Either ([Text], String) (ExBudget, [Text]) ->
  Gen ()
deliverResult constraint input inputProps res = do
  let expect = (haskConstraint constraint) (pconstantToRepr input)
  case (expect,res) of
      (False, Left _) -> pure ()
      (True, Right (cost, _)) -> successWithBudgetCheck cost
      (True, Left err) -> failWithFootnote $ unexpectedFailure err
      (False, Right (_, logs)) -> failWithFootnote $ unexpectedSuccess logs
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
