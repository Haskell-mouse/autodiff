{-# LANGUAGE ScopedTypeVariables #-}

module StagedTests where

import Data.Dependent.Map (DMap)
import Env
  ( SizedNet,
    SizedNetVar,
    exprSizedTestStaged,
    exprSizedTestStaged',
    setupSizedInput,
    var2Getter,
  )
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax.Compat (Code (examineCode), SpliceQ, liftCode)
import UnSized.UnSized
import Sized.LiftInstances ()
import UnSized.Staged
import Data.Monoid (Endo(..))
import GHC.TypeLits

{- testAdStaged :: forall v d.
  ( Semiring d,
    Semigroup d,Semiring
                          (SCont (SExpr d) (Hom (SExpr d) (Endo (Sparse Nat (SExpr d))))),
                          Convertable v (SCont (SExpr d) (Hom (SExpr d) (Endo (Sparse Nat (SExpr d))) ))
  ) =>
  AutoDiffStagedType ->
  SpliceQ (SizedNet d -> DMap SizedNetVar d)
testAdStaged adFunc = liftCode $ do
  input <- runIO setupSizedInput
  let expression = exprSizedTestStaged input
  examineCode
    [||\net -> $$(adFunc @SizedNetVar (\var -> CSpliceQ [||($$(splice var) `var2Getter` net)||]) expression)||]

-}