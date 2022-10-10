module Usage where

import qualified Numeric.LinearAlgebra.Static as LinAlg
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map (DMap)
import Data.Maybe (fromJust)

import Sized
import Expression
import Usage2

testFn' :: Mat '(3, 3) -> Mat '(3, 3)
testFn' x = $(genBody' "x" testFnD)
-- genFnExpr "testFn'" testFn

-- testFn' :: Mat '(4, 3) -> Mat '(1, 1)
-- testFn' x = ((Mat (LinAlg.matrix [1, 2, 3, 4]) :: Mat '(1, 4)) `Sized.times` x) `Sized.times` (Mat (LinAlg.matrix [5, 6, 7]) :: Mat '(3, 1))

testingFn :: forall d. (SizedSemiring d) => DMap Var d -> d '(1, 1)
testingFn net = fromMat m1 `times` x `times` fromMat m2
    where m1 :: Mat '(1, 4)
          m1 = Mat $ LinAlg.matrix [1, 2, 3]

          m2 :: Mat '(3, 1)
          m2 = Mat $ LinAlg.matrix [5, 6, 7]

          x :: d '(4, 3)
          x = fromJust $ DMap.lookup X net
