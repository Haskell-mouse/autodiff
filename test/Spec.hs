{-# LANGUAGE TemplateHaskell #-}
import UnSized.StagedTest
import UnSized.Staged
import Data.Map
import Test.QuickCheck (Gen, quickCheck, arbitrary)
import Test.QuickCheck.Property 

import Control.Monad.IO.Class

import Language.Haskell.TH.Syntax.Compat (Code (examineCode), SpliceQ, liftCode)

main :: IO ()
main = quickCheck deriveProp
 
--der x = 
--      let (Just f) = (Data.Map.lookup 1 $ (testAdStaged reverseADEndoStaged) (18.01)) -- :: Double -> Map Double (SpliceQ Double)
--      in $$(f)

-- 1) x*x + x --> 2x + 1 
-- 2) x*x*x + 2*x*x + 3x -> 3*x*x + 4x + 3 

class ApproxEq a where
  (=~=)  :: a -> a -> Bool
  (=/~=) :: a -> a -> Bool
 
instance ApproxEq Double where
  x =~= x'  = abs (x-x') < epsilon 
    where epsilon = 0.0000001
  x =/~= x' = not $ x =~= x'

instance ApproxEq (Maybe Double) where 
    (Just x) =~= (Just y) = x =~= y 
    _ =~= _ = False 
    x =/~= y = not $ x =~= y

deriveProp :: Gen Result 
deriveProp = do  
  --x <- arbitrary :: Gen Double
--  let x = fromIntegral y :: Double
  let x = 12.5
  let tan = $$(der 1 12.5) 12.5

  return $ if tan =~= (3*x*x + 4*x + 3)
           then succeeded
           else failed { reason = (show $ $$(der 1 12.5) 12.5) ++ " /= " ++ (show $ ((3*x*x + 4*x + 3))) ++ " ; x=" ++ (show x)}