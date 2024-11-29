{-# LANGUAGE TemplateHaskell #-}
import UnSized.StagedTest
import UnSized.Staged
import Data.Map
import Test.QuickCheck (Gen, quickCheck, arbitrary)
import Test.QuickCheck.Gen (genDouble)
import Test.QuickCheck.Property 

import Control.Monad.IO.Class

import Language.Haskell.TH.Syntax.Compat (Code (examineCode), SpliceQ, liftCode)

main :: IO ()
main = do 
  quickCheck deriveProp
  quickCheck deriveProp'
  quickCheck deriveProp''
 
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

 
-- Test unstaged version 
deriveProp'' :: Gen Result 
deriveProp'' = do  
  x <- return 8
  y <- return 8
  z <- return 8
  let tan' = der' x y z 
      tan = snd <$> tan'

  return $ if (tan !! 0) =~= (3*18*y*x*x + 18*4*x + 3)
           then succeeded
           else failed { reason = (show tan') ++ " /= " ++ (show $ ((3*18*x*x*y + 4*18*x + 3))) ++ " ; x=" ++ (show x)} 

-- Test basic staged version
deriveProp' :: Gen Result 
deriveProp' = do  
  x <- genDouble
  y <- genDouble
  z <- genDouble
  let tan'' = $$(der'' [|| x ||] [|| y ||] [|| z ||]) 

  return $ if (tan'' !! 0) =~= (3*18*x*x*y + 4*18*x + 3)
           then succeeded
           else failed { reason = (show tan'') ++ " /= " ++ (show $ ((3*18*x*x*y + 4*18*x + 3))) ++ " ; x=" ++ (show x)}

-- Test advanced staged version
deriveProp :: Gen Result 
deriveProp = do  
  x <- genDouble
  y <- genDouble 
  z <- genDouble
  let tan = $$(der [|| [x,y,z] ||])

  return $ if (tan !! 0) =~= (3*18*x*x*y + 4*18*x + 3)
           then succeeded
           else failed { reason = (show tan) ++ " /= " ++ (show $ ((6*18*x*x + 4*18*x + 3))) ++ " ; x=" ++ (show x)} 