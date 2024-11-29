{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module UnSized.ExpToCode where 

import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax.Compat
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
import Debug.Trace
import GHC.TypeLits

import Control.Monad

listTE :: [TExp a] -> TExp [a]
listTE = TExp . ListE . Prelude.map unType

apprec :: Exp -> [Exp] -> Exp
apprec val [] = val 
apprec f (x : xs) = apprec (AppE f x) xs 

--listToSplice :: [SpliceQ Double] -> SpliceQ [Double]
listToSplice xs = liftSplice $ do 
  f <- mapM examineCode xs
  return $ listTE f
-- 
-- to test with 2-var solution
codeGenerateZeroLvl :: SpliceQ [Double] -> [SExpr Double] -> SpliceQ [Double]
codeGenerateZeroLvl vars exprs = liftSplice $ do
      ListE varsUntyped <- unTypeCode vars
      let n = length varsUntyped
          (bnds', exprs') = letCollect exprs
          bnds = Data.Map.toList $ Data.Map.fromList bnds'
          lVars = fst <$> bnds
          lBodies = snd <$> bnds 
      lVarNames <- mapM (\n -> (qNewName $ "xl" ++ (show n))) lVars
      let lVarPatNames = VarP <$> lVarNames
          lVarsAndNumbers = Data.Map.fromList $ zip lVars lVarNames
      args <- replicateM n (qNewName "x")
      let argsP = return <$> (TExp <$> (VarE <$> args)) :: [Q (TExp Double)]
      let ppc = codeGenerateZeroLvl' (liftCode <$> argsP) lVarsAndNumbers <$> exprs'
          lBodies' = codeGenerateZeroLvl' (liftCode <$> argsP) lVarsAndNumbers <$> lBodies
      lBodies'' <- mapM unTypeCode lBodies'
      let decls = (\(n,b) -> ValD n (NormalB b) []) <$> zip lVarPatNames lBodies''
      body <- mapM unTypeCode ppc
      let newBody = LetE decls (ListE body) 
      let lam = LamE (VarP <$> args) newBody -- [|| codeGenerateZeroLvl' $$() ||]
      return $ TExp (apprec lam varsUntyped)

   -- [|| \x -> $$(codeGenerateZeroLvl' e [|| x ||]) ||]

--replaceLVars 

codeGenerateZeroLvl' :: [SpliceQ Double] -> (Data.Map.Map Int Name) -> SExpr Double -> SpliceQ Double
codeGenerateZeroLvl' xs _ (Val a)  = [|| a ||]
codeGenerateZeroLvl' xs _ (SVar v) = 
  let var = xs !! (fromNatural v)
  in [|| $$(var) ||]
codeGenerateZeroLvl' xs varMap (LVar n) = liftSplice $ do  
  case Data.Map.lookup n varMap of 
    Just name -> return $ TExp $ VarE name
    Nothing -> error $ "NothingVar: " ++ (show n)

codeGenerateZeroLvl' xs varMap (l ::*:: r) = [|| $$(codeGenerateZeroLvl' xs varMap l) * $$(codeGenerateZeroLvl' xs varMap r)||]
codeGenerateZeroLvl' xs varMap (l ::+:: r) = [|| $$(codeGenerateZeroLvl' xs varMap l) + $$(codeGenerateZeroLvl' xs varMap r)||]

letCollect :: [SExpr Double] -> ([(Int, SExpr Double)],[SExpr Double])
letCollect [] = ([],[])
letCollect exprs = letCollect' 0 [] exprs 
  where 
    letCollect' :: Int -> [(Int, SExpr Double)] -> [SExpr Double] -> ([(Int, SExpr Double)],[SExpr Double])
    letCollect' n prevRes []  = (prevRes,[])
    letCollect' n prevRes (expr:exprs) = 
      let (varExps,newExpr,finalVar) = extractLets n prevRes expr
          newRes = prevRes ++ varExps
          (varExpss, newExprs) = letCollect' finalVar newRes exprs 
      in (newRes ++ varExpss, (newExpr : newExprs))

extractLets :: Int -> [(Int, SExpr Double)] -> SExpr Double -> ([(Int, SExpr Double)], SExpr Double, Int)
extractLets n prevRes (e1 ::*:: e2) = 
  let ((varExps1),newExpr1,finalVar) = extractLets n prevRes e1 
      newRes = varExps1
      ((varExps2),newExpr2,finalVar2) = extractLets finalVar newRes e2 
  in ((newRes ++ varExps2), (newExpr1 ::*:: newExpr2), finalVar2)
extractLets n prevRes (e1 ::+:: e2) = 
  let ((varExps1),newExpr1,finalVar) = extractLets n prevRes e1 
      newRes = varExps1
      ((varExps2),newExpr2,finalVar2) = extractLets finalVar newRes e2 
  in ((newRes ++ varExps2), (newExpr1 ::+:: newExpr2), finalVar2)
extractLets n prevRes e@(Val _) = ([],e,n) 
extractLets n prevRes e@(SVar _) = ([],e,n)
extractLets n prevRes e@(LVar _) = ([],e,n)
extractLets n prevRes (LetBind [] e) = ([], e, n)
extractLets n prevRes (LetBind (b:bs) e) = 
  let (vs,newExpr, fVar) = extractLets' n prevRes b e
      newRes =  vs
      (varExps2, newExprs', finalVar) = extractLets fVar newRes (LetBind bs newExpr)
  in ((newRes ++ varExps2), newExprs', finalVar)

extractLets' n prevRes (_,b) e = 
  let alreadyDoneRess = [] -- Prelude.filter (\(_,e1) -> e1 == b) prevRes
      (alreadyNum, newNum) = case alreadyDoneRess of
        [] -> (n, n+1)
        ((k,_):_) -> (k,n)
      e' = changeLVars alreadyNum e
      (vs, nbindExpr, fVar') = extractLets newNum prevRes b
      newNum' = if alreadyNum == n
                then fVar'
                else newNum
      (vs', nexpr, fVar) = extractLets (newNum) prevRes e'
  in if alreadyNum == n 
     then ((n,nbindExpr):(vs ++ vs'), nexpr, fVar)
     else (vs', nexpr, fVar)

changeLVars n (e1 ::*:: e2) = (changeLVars n e1) ::*:: (changeLVars n e2)
changeLVars n (e1 ::+:: e2) = (changeLVars n e1) ::+:: (changeLVars n e2)
changeLVars _ e@(Val _) = e
changeLVars n e@(SVar _) = e 
changeLVars n (LVar _) = LVar n 
changeLVars n e@(LetBind b ex) = 
  let b' = (fmap (changeLVars n)) <$> b
  in LetBind b' (changeLVars n ex) 

opt' :: SExpr Double -> SExpr Double
opt' x@(Val a)  = x
opt' x@(SVar v) = x
opt' x@(LVar _) = x 
opt' x@(LetBind _ _) = x

opt' ((Val a) ::*:: (Val b)) = Val (a*b)
opt' ((Val a) ::+:: (Val b)) = Val (a+b)

opt' (l ::+:: r) = case opt' l of 
   Val 0.0 -> opt' r
   Val a -> case opt' r of 
     Val b -> Val (a+b)
     y     -> (Val a) ::+:: y
   x -> case opt' r of 
     Val 0 -> x 
     y     -> case (x,y) of 
       (SVar a, SVar b) | a == b -> (Val 2 ::*:: SVar a)
       _ -> x ::+:: y
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
