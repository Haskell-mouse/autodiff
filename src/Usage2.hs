module Usage2 where

import qualified Numeric.LinearAlgebra.Static as LinAlg
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map (DMap)
import Data.Maybe (fromMaybe)

import Sized
import Expression

{-
testFn :: SizedSemiring d => d '(4, 3) -> d '(1, 1)
testFn x = fromMat m1 `times` (x `times` tr x) `times` fromMat m2
    where m1 :: Mat '(1, 4)
          m1 = Mat $ LinAlg.matrix [1, 2, 3, 4]

          m2 :: Mat '(4, 1)
          m2 = Mat $ LinAlg.matrix [5, 6, 7, 8]

testMap :: DMap Var (Expr Var)
testMap = reverseAD env (testFn (Var X))
    where env :: Var tup -> Expr Var tup
          env X = Var X
          env Y = Var Y
-}

testFn :: SizedSemiring d => d '(3, 3) -> d '(1, 1)
testFn x = fromMat m1 `times` x `times` x `times` fromMat m2
    where m1 :: Mat '(1, 3)
          m1 = Mat $ LinAlg.matrix [1, 2, 3]

          m2 :: Mat '(3, 1)
          m2 = Mat $ LinAlg.matrix [5, 6, 7]

testFnD :: Expr Arg '(3, 3)
testFnD = fromMaybe (FromMat zeroMat) $ DMap.lookup (Arg :: Arg '(3, 3)) $ reverseAD env (testFn (Var Arg))
    where env :: Arg tup -> Expr Arg tup
          env Arg = Var Arg
