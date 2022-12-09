module StagedTests (testAdStaged) where

import Data.Dependent.Map (DMap)
import Env
  ( SizedNet,
    SizedNetVar,
    exprSizedTestStaged,
    setupSizedInput,
    var2Getter,
  )
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax.Compat (Code (examineCode), SpliceQ, liftCode)
import Sized (SizedSemiring)
import Sized.LiftInstances ()
import Sized.Staged
  ( AutoDiffStagedType,
    CSpliceQ (CSpliceQ, splice),
  )

testAdStaged ::
  ( SizedSemiring d,
    forall tup. Semigroup (d tup)
  ) =>
  AutoDiffStagedType ->
  SpliceQ (SizedNet d -> DMap SizedNetVar d)
testAdStaged adFunc = liftCode $ do
  input <- runIO setupSizedInput
  let expression = exprSizedTestStaged input
  examineCode
    [||\net -> $$(adFunc (\var -> CSpliceQ [||($$(splice var) `var2Getter` net)||]) expression)||]
