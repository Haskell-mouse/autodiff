{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module UnSized.StagedTest where 

import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax.Compat (Code (examineCode), SpliceQ, liftCode, examineSplice)
import UnSized.UnSized
import UnSized.Staged
  ( AutoDiffStagedType,
    SCont, 
    reverseADEndoStaged
  )
import Data.Monoid (Endo(..))

--import Data.Semiring
import Data.Map
import Numeric.Natural

import Language.Haskell.TH.Syntax
import Language.Haskell.TH (ppr)

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Control.Monad.IO.Class (liftIO)
import Debug.Trace

testAdStaged :: 
  AutoDiffStagedType ->
  (Double -> Map Double (SExpr Double))
testAdStaged adFunc =
    (\x -> adFunc (\var -> (SVar var)) (exprSizedTestStaged 1) )

der :: Double -> Double -> (SpliceQ (Double -> Double))
der y x = 
      let (Just f) = (Data.Map.lookup y $ (testAdStaged reverseADEndoStaged) x) -- :: Double -> Map Double (SpliceQ Double)
      in trace (show $ opt' f) $ codeGenerateZeroLvl $ opt' f

dummyCGFunc :: SExpr Double -> SpliceQ Double
dummyCGFunc _ = undefined

-- to test with 1-var solution
codeGenerateZeroLvl :: SExpr Double -> SpliceQ (Double -> Double)
codeGenerateZeroLvl e = [|| \x -> $$(codeGenerateZeroLvl' e [|| x ||]) ||]

codeGenerateZeroLvl' :: SExpr Double -> SpliceQ Double -> SpliceQ Double
codeGenerateZeroLvl' (Val a)  x = [|| a ||]
codeGenerateZeroLvl' (SVar v) x = [|| $$(x) ||]

codeGenerateZeroLvl' (l ::*:: r) x = [|| $$(codeGenerateZeroLvl' l x) * $$(codeGenerateZeroLvl' r x)||]
codeGenerateZeroLvl' (l ::+:: r) x = [|| $$(codeGenerateZeroLvl' l x) + $$(codeGenerateZeroLvl' r x)||]


opt' :: SExpr Double -> SExpr Double
opt' x@(Val a)  = x
opt' x@(SVar v) = x

opt' ((Val a) ::*:: (Val b)) = Val (a*b)
opt' ((Val a) ::+:: (Val b)) = Val (a+b)

opt' (l ::+:: r) = case opt' l of 
   Val 0.0 -> opt' r
   Val a -> case opt' r of 
     Val b -> Val (a+b)
     y     -> (Val a) ::+:: y
   x -> case opt' r of 
     Val 0 -> x 
     y     -> x ::+:: y
opt' (l ::*:: r) = case opt' l of 
   Val 0.0 -> Val 0
   Val 1.0 -> opt' r
   Val a -> case opt' r of 
     Val b -> Val (a*b)
     y     -> (Val a) ::*:: y
   x -> case opt' r of 
     Val 0 -> Val 0 
     Val 1 -> x  
     y     -> x ::*:: y

{-
type AutoDiffStagedType =
  forall v d.
  (Ord v, Semiring d, Semigroup d, Semiring
                          (SCont (SpliceQ d) (SpliceQ (Hom d (Endo (Sparse v d))))) 
  ) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (Map v d)
-}
{-
data Point v where 
    Point :: Semiring v => {
        x :: v,
        y :: v
    }
-}

sizedTest :: forall d. Semiring d => d -> SExpr d
sizedTest x = ((SVar x) `times` (SVar x) `times` (SVar x)) `plus` ((SVar x) `times` (SVar x) `plus` ((SVar x) `times` (SVar x))) `plus` ((SVar x) `plus` (SVar x) `plus` (SVar x)) `plus` (fromNatural 10)

--sizedTest x = (x `times` x `times`x) `plus` ((fromNatural 2 :: d) `times` x `times` x) `plus` ((fromNatural 3 :: d) `times` x)


exprSizedTestStaged :: (Semiring d, Lift d, Semiring d) => d -> SExpr d
exprSizedTestStaged var = sizedTest var

values :: Map Natural Double
values = fromList [(1,5),(2,2)]