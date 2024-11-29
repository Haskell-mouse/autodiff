{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module UnSized.StagedTest where 

import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax.Compat -- (Code (examineCode), expToSplice, SpliceQ, liftCode, examineSplice, liftSplice)
import UnSized.UnSized
import UnSized.Staged
  ( AutoDiffStagedType,
    SCont, 
    reverseADEndoStaged
  )
import qualified UnSized.Unstaged as US 
import qualified UnSized.StagedBasic as SB 
import Data.Monoid (Endo(..))

--import Data.Semiring
import Data.Map
import Numeric.Natural

import Language.Haskell.TH.Syntax
import Language.Haskell.TH (ppr)

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Control.Monad.IO.Class (liftIO)
import GHC.TypeLits

import Control.Monad

import UnSized.ExpToCode

type family Args (a :: Either * Nat) (b :: *) (r :: *) where 
  Args (Right 0) _ r = r
  Args (Right n) a r = a -> Args (Right (n-1)) a r
  Args (Left b) _ r  = b -> r


testAdStaged :: 
  AutoDiffStagedType ->
  (Map Nat (SExpr Double))
testAdStaged adFunc = adFunc (\var -> (SVar var)) exprSizedTestStaged

testAdStaged20 :: 
  AutoDiffStagedType ->
  (Map Nat (SExpr Double))
testAdStaged20 adFunc = adFunc (\var -> (SVar var)) exprSizedTestStaged20

testAdStaged20N360 :: 
  AutoDiffStagedType ->
  (Map Nat (SExpr Double))
testAdStaged20N360 adFunc = adFunc (\var -> (SVar var)) exprSizedTestStaged20N360

testAdSB20 :: 
  SB.AutoDiffStagedType ->
  [SpliceQ Double] ->
  (Map Nat (SpliceQ Double))
testAdSB20 adFunc vals = adFunc (\var -> (vals !! (fromNatural var))) exprSizedTestStaged20Basic

testAdStaged60 :: 
  AutoDiffStagedType ->
  (Map Nat (SExpr Double))
testAdStaged60 adFunc = adFunc (\var -> (SVar var)) exprSizedTestStaged60

testAd :: 
   US.AutoDiffStagedType ->
   [Double] -> 
   Map Nat Double
testAd adFunc vals = (adFunc (\var -> (vals !! (fromNatural var))) (exprTestUnStaged' @Double))

testAdSB :: 
  SB.AutoDiffStagedType ->
  [SpliceQ Double] ->
  Map Nat (SpliceQ (Double))
testAdSB adFunc args = (adFunc (\var -> (args !! (fromIntegral var))) (exprTestStaged'' 0 1 2))

exprTestStaged'' :: forall v. Semiring v => Nat -> Nat -> Nat -> Expr v
exprTestStaged'' x y z = (Var z) `plus` ((fromNatural 2) `times` (Var y)) `plus` ((((Var y) `times` (Var x) `times` (Var x) `times` (Var x)) `plus` ((fromNatural 2) `times` ((Var x) `times` (Var x)))) `times` (fromNatural 18)) `plus` ((fromNatural 3) `times` (Var x)) `plus` (fromNatural 10)



exprTestUnStaged' :: forall d. (Semiring d, Num d) => Expr d
exprTestUnStaged' = (Var 2) `plus` ((fromNatural 2) `times` (Var 1)) `plus` ((((Var 1) `times` (Var 0) `times` (Var 0) `times` (Var 0)) `plus` ((fromNatural 2) `times` ((Var 0) `times` (Var 0)))) `times` (fromNatural 18)) `plus` ((fromNatural 3) `times` (Var 0)) `plus` (fromNatural 10)

-- x*x*x + x*x + x*x + x*x + x 
-- 3x*x + 6x + 1

--exprTestStaged = exprTestStaged' (12.5 :: Double) 2 1 

sizedTest :: forall d. Semiring d => SExpr d -> SExpr d -> SExpr d -> SExpr d
sizedTest x y z = z `plus` ((fromNatural 2) `times` y) `plus` (((y `times` x `times` x `times` x) `plus` ((fromNatural 2) `times` (x `times` x))) `times` (fromNatural 18)) `plus` ((fromNatural 3) `times` x) `plus` (fromNatural 10)

--2y + 18(y*x^3 + 2x^2) + 3x + 10

-- 3*18*y*x^2 + 18*4x + 3

-- x -> y*x^3+2x^2+3x
-- y -> 2 + 18*x^3 

--x*x*x + 2*x*x + 3x + 10 
--sizedTest x = (x `times` x `times`x) `plus` ((fromNatural 2 :: d) `times` x `times` x) `plus` ((fromNatural 3 :: d) `times` x)

sizedTest20 :: forall d. Semiring d => SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d
sizedTest20 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 = 
      ((fromNatural 20) `times` (x1 `times` (x2 `times` (x3 `times` (x4 `times` x5))))) `plus`  ((fromNatural 19) `times` (x2 `times` (x3 `times` (x4 `times` (x5 `times` x6)))))  `plus`  ((fromNatural 18) `times` (x3 `times` (x4 `times` (x5 `times` (x6 `times` x7)))))  `plus`  
      ((fromNatural 17) `times` (x4 `times` (x5 `times` (x6 `times` (x7 `times` x8)))))  `plus`  ((fromNatural 16) `times` (x5 `times` (x6 `times` (x7 `times` (x8 `times` x9)))))  `plus`  ((fromNatural 15) `times` (x6 `times` (x7 `times` (x8 `times` (x9 `times` x10)))))  `plus`  
      ((fromNatural 14) `times` (x7 `times` (x8 `times` (x9 `times` (x10 `times` x11)))))  `plus`  ((fromNatural 13) `times` (x8 `times` (x9 `times` (x10 `times` (x11 `times` x12)))))  `plus`  ((fromNatural 12) `times` (x9 `times` (x10 `times` (x11 `times` (x12 `times` x13)))))  `plus`  
      ((fromNatural 11) `times` (x10 `times` (x11 `times` (x12 `times` (x13 `times` x14)))))  `plus`  ((fromNatural 10) `times` (x11 `times` (x12 `times` (x13 `times` (x14 `times` x15)))))  `plus`  ((fromNatural 9) `times` (x12 `times` (x13 `times` (x14 `times` (x15 `times` x16)))))  `plus`  
      ((fromNatural 8) `times` (x13 `times` (x14 `times` (x15 `times` (x16 `times` x17)))))  `plus`  ((fromNatural 7) `times` (x14 `times` (x15 `times` (x16 `times` (x17 `times` x18)))))  `plus`  ((fromNatural 6) `times` (x15 `times` (x16 `times` (x17 `times` (x18 `times` x19)))))  `plus`  
      ((fromNatural 7) `times` (x16 `times` (x17 `times` (x18 `times` (x19 `times` x20)))))  `plus`  ((fromNatural 6) `times` (x17 `times` (x18 `times` (x19 `times` x20))))  `plus`  ((fromNatural 5) `times` x18 `times` (x19 `times` x20))  `plus`  
      ((fromNatural 4) `times` (x19 `times` x20))  `plus`  ((fromNatural 3) `times` x20)

sizedTest20N360 :: forall d. Semiring d => SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d
sizedTest20N360 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 = 
      ((fromNatural 20) `times` (x1 `times` (x2 `times` (x3 `times` (x4 `times` x5))))) `plus`  ((fromNatural 19) `times` (x2 `times` (x3 `times` (x4 `times` (x5 `times` x6)))))  `plus`  ((fromNatural 18) `times` (x3 `times` (x4 `times` (x5 `times` (x6 `times` x7)))))  `plus`  
      ((fromNatural 17) `times` (x4 `times` (x5 `times` (x6 `times` (x7 `times` x8)))))  `plus`  ((fromNatural 16) `times` (x5 `times` (x6 `times` (x7 `times` (x8 `times` x9)))))  `plus`  ((fromNatural 15) `times` (x6 `times` (x7 `times` (x8 `times` (x9 `times` x10)))))  `plus`  
      ((fromNatural 14) `times` (x7 `times` (x8 `times` (x9 `times` (x10 `times` x11)))))  `plus`  ((fromNatural 13) `times` (x8 `times` (x9 `times` (x10 `times` (x11 `times` x12)))))  `plus`  ((fromNatural 12) `times` (x9 `times` (x10 `times` (x11 `times` (x12 `times` x13)))))  `plus`  
      ((fromNatural 11) `times` (x10 `times` (x11 `times` (x12 `times` (x13 `times` x14)))))  `plus`  ((fromNatural 10) `times` (x11 `times` (x12 `times` (x13 `times` (x14 `times` x15)))))  `plus`  ((fromNatural 9) `times` (x12 `times` (x13 `times` (x14 `times` (x15 `times` x16)))))  `plus`  
      ((fromNatural 8) `times` (x13 `times` (x14 `times` (x15 `times` (x16 `times` x17)))))  `plus`  ((fromNatural 7) `times` (x14 `times` (x15 `times` (x16 `times` (x17 `times` x18)))))  `plus`  ((fromNatural 6) `times` (x15 `times` (x16 `times` (x17 `times` (x18 `times` x19)))))  `plus`  
      ((fromNatural 7) `times` (x16 `times` (x17 `times` (x18 `times` (x19 `times` x20)))))  `plus`  ((fromNatural 6) `times` (x17 `times` (x18 `times` (x19 `times` x20))))  `plus`  ((fromNatural 5) `times` x18 `times` (x19 `times` x20))  `plus`  
      ((fromNatural 4) `times` (x19 `times` x20))  `plus`  ((fromNatural 3) `times` x20) `plus` 
      ((fromNatural 11) `times` (x1 `times` (x2 `times` (x3 `times` (x4 `times` x5))))) `plus`  ((fromNatural 19) `times` (x2 `times` (x3 `times` (x4 `times` (x5 `times` x6)))))  `plus`  ((fromNatural 18) `times` (x3 `times` (x4 `times` (x5 `times` (x6 `times` x7)))))  `plus`  
      ((fromNatural 12) `times` (x4 `times` (x5 `times` (x6 `times` (x7 `times` x8)))))  `plus`  ((fromNatural 16) `times` (x5 `times` (x6 `times` (x7 `times` (x8 `times` x9)))))  `plus`  ((fromNatural 15) `times` (x6 `times` (x7 `times` (x8 `times` (x9 `times` x10)))))  `plus`  
      ((fromNatural 13) `times` (x7 `times` (x8 `times` (x9 `times` (x10 `times` x11)))))  `plus`  ((fromNatural 13) `times` (x8 `times` (x9 `times` (x10 `times` (x11 `times` x12)))))  `plus`  ((fromNatural 12) `times` (x9 `times` (x10 `times` (x11 `times` (x12 `times` x13)))))  `plus`  
      ((fromNatural 14) `times` (x10 `times` (x11 `times` (x12 `times` (x13 `times` x14)))))  `plus`  ((fromNatural 10) `times` (x11 `times` (x12 `times` (x13 `times` (x14 `times` x15)))))  `plus`  ((fromNatural 9) `times` (x12 `times` (x13 `times` (x14 `times` (x15 `times` x16)))))  `plus`  
      ((fromNatural 15) `times` (x13 `times` (x14 `times` (x15 `times` (x16 `times` x17)))))  `plus`  ((fromNatural 7) `times` (x14 `times` (x15 `times` (x16 `times` (x17 `times` x18)))))  `plus`  ((fromNatural 6) `times` (x15 `times` (x16 `times` (x17 `times` (x18 `times` x19)))))  `plus`  
      ((fromNatural 16) `times` (x16 `times` (x17 `times` (x18 `times` (x19 `times` x20)))))  `plus`  ((fromNatural 6) `times` (x17 `times` (x18 `times` (x19 `times` x20))))  `plus`  ((fromNatural 5) `times` x18 `times` (x19 `times` x20))  `plus`  
      ((fromNatural 17) `times` (x19 `times` x20))  `plus`  ((fromNatural 3) `times` x20) `plus`
      ((fromNatural 12) `times` (x1 `times` (x2 `times` (x3 `times` (x4 `times` x5))))) `plus`  ((fromNatural 19) `times` (x2 `times` (x3 `times` (x4 `times` (x5 `times` x6)))))  `plus`  ((fromNatural 18) `times` (x3 `times` (x4 `times` (x5 `times` (x6 `times` x7)))))  `plus`  
      ((fromNatural 13) `times` (x4 `times` (x5 `times` (x6 `times` (x7 `times` x8)))))  `plus`  ((fromNatural 16) `times` (x5 `times` (x6 `times` (x7 `times` (x8 `times` x9)))))  `plus`  ((fromNatural 15) `times` (x6 `times` (x7 `times` (x8 `times` (x9 `times` x10)))))  `plus`  
      ((fromNatural 14) `times` (x7 `times` (x8 `times` (x9 `times` (x10 `times` x11)))))  `plus`  ((fromNatural 13) `times` (x8 `times` (x9 `times` (x10 `times` (x11 `times` x12)))))  `plus`  ((fromNatural 12) `times` (x9 `times` (x10 `times` (x11 `times` (x12 `times` x13)))))  `plus`  
      ((fromNatural 15) `times` (x10 `times` (x11 `times` (x12 `times` (x13 `times` x14)))))  `plus`  ((fromNatural 10) `times` (x11 `times` (x12 `times` (x13 `times` (x14 `times` x15)))))  `plus`  ((fromNatural 9) `times` (x12 `times` (x13 `times` (x14 `times` (x15 `times` x16)))))  `plus`  
      ((fromNatural 16) `times` (x13 `times` (x14 `times` (x15 `times` (x16 `times` x17)))))  `plus`  ((fromNatural 7) `times` (x14 `times` (x15 `times` (x16 `times` (x17 `times` x18)))))  `plus`  ((fromNatural 6) `times` (x15 `times` (x16 `times` (x17 `times` (x18 `times` x19)))))  `plus`  
      ((fromNatural 17) `times` (x16 `times` (x17 `times` (x18 `times` (x19 `times` x20)))))  `plus`  ((fromNatural 6) `times` (x17 `times` (x18 `times` (x19 `times` x20))))  `plus`  ((fromNatural 5) `times` x18 `times` (x19 `times` x20))  `plus`  
      ((fromNatural 18) `times` (x19 `times` x20))  `plus`  ((fromNatural 3) `times` x20) `plus` 
      ((fromNatural 20) `times` (x1 `times` (x2 `times` (x3 `times` (x4 `times` x5))))) `plus`  ((fromNatural 19) `times` (x2 `times` (x3 `times` (x4 `times` (x5 `times` x6)))))  `plus`  ((fromNatural 18) `times` (x3 `times` (x4 `times` (x5 `times` (x6 `times` x7)))))  `plus`  
      ((fromNatural 17) `times` (x4 `times` (x5 `times` (x6 `times` (x7 `times` x8)))))  `plus`  ((fromNatural 16) `times` (x5 `times` (x6 `times` (x7 `times` (x8 `times` x9)))))  `plus`  ((fromNatural 15) `times` (x6 ))

sizedTest20' :: forall d. Semiring d => Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d -> Expr d
sizedTest20' x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 = 
      ((fromNatural 20) `times` (x1 `times` (x2 `times` (x3 `times` (x4 `times` x5))))) `plus`  ((fromNatural 19) `times` (x2 `times` (x3 `times` (x4 `times` (x5 `times` x6)))))  `plus`  ((fromNatural 18) `times` (x3 `times` (x4 `times` (x5 `times` (x6 `times` x7)))))  `plus`  
      ((fromNatural 17) `times` (x4 `times` (x5 `times` (x6 `times` (x7 `times` x8)))))  `plus`  ((fromNatural 16) `times` (x5 `times` (x6 `times` (x7 `times` (x8 `times` x9)))))  `plus`  ((fromNatural 15) `times` (x6 `times` (x7 `times` (x8 `times` (x9 `times` x10)))))  `plus`  
      ((fromNatural 14) `times` (x7 `times` (x8 `times` (x9 `times` (x10 `times` x11)))))  `plus`  ((fromNatural 13) `times` (x8 `times` (x9 `times` (x10 `times` (x11 `times` x12)))))  `plus`  ((fromNatural 12) `times` (x9 `times` (x10 `times` (x11 `times` (x12 `times` x13)))))  `plus`  
      ((fromNatural 11) `times` (x10 `times` (x11 `times` (x12 `times` (x13 `times` x14)))))  `plus`  ((fromNatural 10) `times` (x11 `times` (x12 `times` (x13 `times` (x14 `times` x15)))))  `plus`  ((fromNatural 9) `times` (x12 `times` (x13 `times` (x14 `times` (x15 `times` x16)))))  `plus`  
      ((fromNatural 8) `times` (x13 `times` (x14 `times` (x15 `times` (x16 `times` x17)))))  `plus`  ((fromNatural 7) `times` (x14 `times` (x15 `times` (x16 `times` (x17 `times` x18)))))  `plus`  ((fromNatural 6) `times` (x15 `times` (x16 `times` (x17 `times` (x18 `times` x19)))))  `plus`  
      ((fromNatural 7) `times` (x16 `times` (x17 `times` (x18 `times` (x19 `times` x20)))))  `plus`  ((fromNatural 6) `times` (x17 `times` (x18 `times` (x19 `times` x20))))  `plus`  ((fromNatural 5) `times` x18 `times` (x19 `times` x20))  `plus`  
      ((fromNatural 4) `times` (x19 `times` x20))  `plus`  ((fromNatural 3) `times` x20)

sizedTest60 :: forall d. Semiring d => SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d 
                                    -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d 
                                    -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d 
                                    -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d 
                                    -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d 
                                    -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d 
                                    -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d 
                                    -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d 
                                    -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d 
                                    -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d -> SExpr d
sizedTest60 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
                x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 
                x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 
                x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 
                x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 
                x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 = 
       (x22 `times` x36 `times` x50 `times` x39 `times` x53 `times` x44) `plus` 
       (x13 `times` x23 `times` x50 `times` x4 `times` x21 `times` x57) `plus`  
       (x9 `times` x17 `times` x23 `times` x12 `times` x53 `times` x16) `plus` 
       (x52 `times` x17 `times` x7 `times` x56 `times` x1 `times` x33) `plus` 
       (x18 `times` x58 `times` x16 `times` x52 `times` x50 `times` x17) `plus` 
       (x6 `times` x43 `times` x14 `times` x2 `times` x59 `times` x46) `plus`  
       (x19 `times` x5 `times` x17 `times` x23 `times` x34 `times` x45) `plus` 
       (x35 `times` x34 `times` x8 `times` x56 `times` x2 `times` x13) `plus` 
       (x23 `times` x52 `times` x20 `times` x57 `times` x37 `times` x11) `plus`  
       (x15 `times` x53 `times` x14 `times` x19 `times` x36 `times` x23) `plus` 
       (x6 `times` x17 `times` x35 `times` x57 `times` x24 `times` x54) `plus` 
       (x40 `times` x34 `times` x47 `times` x13 `times` x11 `times` x29) `plus` 
       (x57 `times` x38 `times` x56 `times` x6 `times` x54 `times` x20) `plus` 
       (x36 `times` x5 `times` x57 `times` x4 `times` x41 `times` x15) `plus` 
       (x55 `times` x19 `times` x17 `times` x40 `times` x9 `times` x46) `plus` 
       (x22 `times` x58 `times` x39 `times` x3 `times` x17 `times` x44) `plus` 
       (x53 `times` x33 `times` x8 `times` x23 `times` x35 `times` x11) `plus` 
       (x58 `times` x38 `times` x35 `times` x30 `times` x34 `times` x28) `plus` 
       (x11 `times` x57 `times` x42 `times` x58 `times` x39 `times` x17) `plus` 
       (x36 `times` x23 `times` x14 `times` x27 `times` x1 `times` x8) `plus` 
       (x55 `times` x41 `times` x7 `times` x28 `times` x3 `times` x29) `plus` 
       (x16 `times` x35 `times` x56 `times` x44 `times` x26 `times` x28) `plus` 
       (x56 `times` x32 `times` x53 `times` x25 `times` x46 `times` x4) `plus` 
       (x10 `times` x2 `times` x7 `times` x57 `times` x45 `times` x8) `plus` 
       (x27 `times` x58 `times` x60 `times` x21 `times` x6 `times` x37) `plus` 
       (x56 `times` x23 `times` x34 `times` x17 `times` x1 `times` x49) `plus` 
       (x18 `times` x56 `times` x52 `times` x57 `times` x45 `times` x49) `plus` 
       (x37 `times` x40 `times` x23 `times` x5 `times` x47 `times` x41) `plus` 
       (x17 `times` x35 `times` x43 `times` x50 `times` x2 `times` x10) `plus` 
       (x49 `times` x55 `times` x12 `times` x11 `times` x26 `times` x31) `plus` 
       (x16 `times` x58 `times` x48 `times` x30 `times` x14 `times` x24) `plus` 
       (x5 `times` x50 `times` x37 `times` x55 `times` x38 `times` x19) `plus` 
       (x20 `times` x1 `times` x46 `times` x22 `times` x13 `times` x25) `plus` 
       (x56 `times` x52 `times` x25 `times` x23 `times` x16 `times` x37) `plus` 
       (x41 `times` x2 `times` x57 `times` x34 `times` x22 `times` x1) `plus` 
       (x37 `times` x10 `times` x21 `times` x44 `times` x45 `times` x58) `plus` 
       (x14 `times` x22 `times` x27 `times` x9 `times` x51 `times` x7) `plus` 
       (x48 `times` x52 `times` x23 `times` x40 `times` x34 `times` x13) `plus` 
       (x40 `times` x36 `times` x41 `times` x9 `times` x50 `times` x54) `plus` 
       (x39 `times` x9 `times` x41 `times` x50 `times` x33 `times` x21) `plus` 
       (x7 `times` x20 `times` x16 `times` x11 `times` x15 `times` x10) `plus` 
       (x1 `times` x5 `times` x43 `times` x55 `times` x31 `times` x3) `plus` 
       (x19 `times` x39 `times` x41 `times` x6 `times` x53 `times` x1) `plus` 
       (x37 `times` x2 `times` x38 `times` x43 `times` x60 `times` x52) `plus` 
       (x60 `times` x56 `times` x12 `times` x34 `times` x27 `times` x42) `plus` 
       (x19 `times` x54 `times` x52 `times` x28 `times` x32 `times` x38) `plus` 
       (x16 `times` x51 `times` x15 `times` x36 `times` x37 `times` x57) `plus` 
       (x29 `times` x23 `times` x56 `times` x33 `times` x1 `times` x3) `plus` 
       (x34 `times` x40 `times` x4 `times` x1 `times` x26 `times` x12) `plus` 
       (x58 `times` x20 `times` x14 `times` x26 `times` x60 `times` x16) `plus` 
       (x50 `times` x49 `times` x37 `times` x44 `times` x7 `times` x18) `plus` 
       (x37 `times` x24 `times` x59 `times` x33 `times` x42 `times` x23) `plus` 
       (x37 `times` x11 `times` x19 `times` x46 `times` x6 `times` x41) `plus` 
       (x27 `times` x3 `times` x52 `times` x41 `times` x11 `times` x56) `plus` 
       (x36 `times` x3 `times` x24 `times` x25 `times` x37 `times` x38) `plus` 
       (x28 `times` x60 `times` x30 `times` x36 `times` x51 `times` x25) `plus` 
       (x5 `times` x36 `times` x41 `times` x55 `times` x49 `times` x10) `plus` 
       (x45 `times` x53 `times` x49 `times` x39 `times` x17 `times` x14) `plus` 
       (x37 `times` x55 `times` x11 `times` x35 `times` x49 `times` x7) `plus` 
       (x23 `times` x55 `times` x4 `times` x24 `times` x37 `times` x19)



exprSizedTestStaged :: SExpr Double
exprSizedTestStaged = instr 0 (sizedTest @Double)

exprSizedTestStaged20 :: SExpr Double
exprSizedTestStaged20 = instr 0 (sizedTest20 @Double)

exprSizedTestStaged20N360 :: SExpr Double
exprSizedTestStaged20N360 = instr 0 (sizedTest20N360 @Double)

exprSizedTestStaged60 :: SExpr Double
exprSizedTestStaged60 = instr 0 (sizedTest60 @Double)

exprSizedTestStaged20Basic :: Expr Double
exprSizedTestStaged20Basic = instr' 0 (sizedTest20' @Double)

der'' :: SpliceQ Double -> SpliceQ Double -> SpliceQ Double -> SpliceQ [Double]
der'' x y z = 
      let f = snd <$> toList (testAdSB SB.reverseADEndoStaged [x, y, z]) -- :: Double -> Map Double (SpliceQ Double)
      in listToSplice f

der' :: Double -> Double -> Double -> [(Nat, Double)]
der' x y z = 
  let mg = toList (testAd US.reverseADEndoStaged [x,y,z])
  in mg

der :: SpliceQ [Double] -> (SpliceQ [Double])
der z = 
      let f = snd <$> toList (testAdStaged reverseADEndoStaged)
      in  codeGenerateZeroLvl z $ (opt' <$> f)

der20 :: SpliceQ [Double] -> (SpliceQ [Double])
der20 z = 
      let f = snd <$> toList (testAdStaged20 reverseADEndoStaged)
      in codeGenerateZeroLvl z $ (opt' <$> f)

der20Basic :: [SpliceQ Double] -> SpliceQ [Double]
der20Basic z = 
      let f = snd <$> toList (testAdSB20 SB.reverseADEndoStaged z) -- :: Double -> Map Double (SpliceQ Double)
      in listToSplice f

der20N360 :: SpliceQ [Double] -> (SpliceQ [Double])
der20N360 z = 
      let f = snd <$> toList (testAdStaged20N360 reverseADEndoStaged) -- :: Double -> Map Double (SpliceQ Double)
      in codeGenerateZeroLvl z $ (opt' <$> f)

der60 :: SpliceQ [Double] -> (SpliceQ [Double])
der60 z = 
      let f = snd <$> toList (testAdStaged60 reverseADEndoStaged)
      in codeGenerateZeroLvl z $ (opt' <$> f)
