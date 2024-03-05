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
import Sized (SizedSemiring, Dual, Hom, Sparse)
import Sized.LiftInstances ()
import Sized.Staged
  ( AutoDiffStagedType,
    CSpliceQ (CSpliceQ, splice),
  )
import Data.Monoid (Endo(..))

testAdStaged :: forall d.
  ( SizedSemiring d,
    forall tup. Semigroup (d tup),
    SizedSemiring (CSpliceQ (Dual d (Hom d (Endo (Sparse SizedNetVar d)))))
  ) =>
  AutoDiffStagedType ->
  SpliceQ (SizedNet d -> DMap SizedNetVar d)
testAdStaged adFunc = liftCode $ do
  input <- runIO setupSizedInput
  let expression = exprSizedTestStaged input
  examineCode
    [||\net -> $$(adFunc @SizedNetVar (\var -> CSpliceQ [||($$(splice var) `var2Getter` net)||]) expression)||]
