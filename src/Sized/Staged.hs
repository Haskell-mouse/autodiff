{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Sized.Staged (module Sized.Staged) where

-- TODO: Add export list

import GHC.TypeLits
import Data.Kind
import Language.Haskell.TH.Syntax.Compat
import Data.GADT.Compare (GCompare)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Sized
import Data.Semigroup (Endo)
import Data.Monoid (Endo(..))
import Sized.LiftInstances ()
import qualified Numeric.LinearAlgebra.Static as LinAlg
import qualified Numeric.LinearAlgebra as LinAlg (Additive(..))

import Data.Functor.Identity
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Control.Monad.IO.Class (liftIO)

import Unsafe.Coerce
-- | Constrained splice.
-- 
-- Wraps a SpliceQ into a type constructor.
-- Used to implement splicing for indexed matrices.
type CSpliceQ :: (constr -> Type) -> constr -> Type
newtype CSpliceQ typ constr = CSpliceQ { splice :: SpliceQ (typ constr) }

--instance SizedSemiring a => SizedSemiring (CSpliceQ a) where
--  (CSpliceQ e1) `plus` (CSpliceQ e2) = CSpliceQ [|| $$e1 `plus` $$e2 ||]
--  (CSpliceQ e1) `times` (CSpliceQ e2) = CSpliceQ [|| $$e1 `times` $$e2 ||]
--  tr (CSpliceQ e1) = CSpliceQ [|| tr $$e1 ||]
--  fromMat mat = CSpliceQ [|| fromMat mat ||]

-- TODO: Debug.Trace the `fromMat` function

-- TODO: Test if this instance improves performance
{-instance SizedSemiring (CSpliceQ Mat) where
  (CSpliceQ e1) `plus` (CSpliceQ e2) = CSpliceQ
    [|| let (Mat lhs) = $$e1
            (Mat rhs) = $$e2
        in Mat $ lhs `LinAlg.add` rhs ||]
  (CSpliceQ e1) `times` (CSpliceQ e2) = CSpliceQ
    [|| let (Mat lhs) = $$e1
            (Mat rhs) = $$e2  
        in Mat $ (LinAlg.<>) lhs rhs ||]
  tr (CSpliceQ e1) = CSpliceQ
     [|| let (Mat mat) = $$e1 in Mat $ LinAlg.tr mat ||]
  fromMat mat = CSpliceQ [|| id mat ||]
-}
-- Inlined by hands functions from other instances
{- instance (GCompare k) => SizedSemiring (CSpliceQ (Dual Mat (Hom Mat (Endo (Sparse k Mat))))) where 
  (CSpliceQ e1) `plus` (CSpliceQ e2) = CSpliceQ [||
      let Dual (Mat matL) (Hom df1) = $$e1
          Dual (Mat matR) (Hom df2) = $$e2
      in Dual (Mat $ (LinAlg.add) matL matR) (Hom $ \grad -> (df1 grad) <> (df2 grad)) ||]

  (CSpliceQ e1) `times` (CSpliceQ e2) = CSpliceQ [||
      let Dual (Mat matL) (Hom df1) = $$e1
          Dual (Mat matR) (Hom df2) = $$e2
      in Dual (Mat $ (LinAlg.mul) matL matR) 
                 ((Hom $ \(Mat grad) -> df2 $ (Mat $ (LinAlg.tr matL) `LinAlg.mul` grad)) <> 
                  (Hom $ \(Mat grad) -> df1 $ (Mat $ grad `LinAlg.mul` (LinAlg.tr matR)))) ||]

  tr (CSpliceQ e1) = CSpliceQ [||
    let Dual (Mat mat) (Hom f) = $$e1
    in Dual (Mat $ LinAlg.tr mat) (Hom $ \(Mat grad) -> f (Mat (LinAlg.tr grad))) ||]
  fromMat mat = CSpliceQ [|| Dual mat mempty ||]
-}
{-
--type SCont :: Type -> Type
type SCont :: ((Nat, Nat) -> Type) -> ((Nat, Nat) -> Type) -> Type -> (Nat, Nat) -> Type
newtype SCont a b r tup = Cont { runCont :: ((a tup, b tup) -> r) -> r }
-- ((a tup, b tup) -> r tup) -> r tup

instance SizedSemiring (CSpliceQ (SCont Mat (Hom Mat (Endo (Sparse k Mat))) (Mat '(m, n), Hom Mat (Endo (Sparse k Mat)) '(m, n)))) where
  fromMat mat = CSpliceQ [|| Cont (\f -> f (mat, mempty)) ||]
  tr (CSpliceQ e1) = CSpliceQ $ liftSplice $ do 
    b <- examineCode   [||
                           let Cont f = $$e1
                               z = f (\(Mat v, Hom df) ->
                                           let t = Hom (\(Mat grad) -> df $ (Mat (LinAlg.tr grad)))
                                               k = Mat $ LinAlg.tr v :: Mat '(m, n)
                                           in (k,t))
                           in Cont (\f' -> f' z)
                        ||]
    return b
  (CSpliceQ e1) `plus` (CSpliceQ e2) = CSpliceQ $ liftSplice $ do   
    b <- examineCode $ [||
                           let Cont f1 = $$e1
                               Cont f2 = $$e1
                           in undefined
                      ||]
    return b
  times = undefined
-}

--type SCont :: Type -> Type
type SCont :: ((Nat, Nat) -> Type) -> ((Nat, Nat) -> Type) -> (Nat, Nat) -> Type
newtype SCont a b tup = Cont { runCont :: forall r. ((a tup, b tup) -> r) -> r }

--instance SizedSemiring SCont () 
 

instance SizedSemiring (SCont (CSpliceQ Mat) (Hom (CSpliceQ Mat) (Endo (SpliceQ (Sparse k Mat))))) where 
   fromMat mat = undefined -- Cont (\f -> f (CSpliceQ [|| mat ||], CSpliceQ mempty))
   tr (Cont f) = undefined {-
     let 
         o =     (\((CSpliceQ pr),(Hom tan)) -> 
                    let df' = Hom $ (\grad -> [||
                                          let (CSpliceQ grad') = $$grad   
                                          in (df'') $ ( CSpliceQ undefined {-[|| (tr grad') ||]-} ) ||])
                        pr' = [|| tr $$pr ||]
                    in (CSpliceQ pr', CSpliceQ df'))
     in  Cont (\g -> f (g . o)) -}

   (Cont f1) `plus` (Cont f2) = undefined {-
     let p =  (\(CSpliceQ pr1, CSpliceQ tan1) (CSpliceQ pr2, CSpliceQ tan2) -> 
                            (CSpliceQ [|| plus $$pr1 $$pr2 ||], CSpliceQ [|| Hom (\grad -> 
                                                                               let (Hom df1) = $$tan1
                                                                                   (Hom df2) = $$tan2 
                                                                               in (df1 (grad)) <> (df2 grad)) 
                                                                  ||]))
     in Cont $ \k -> f1 (\x -> f2 (\y -> k (p x y))) -}

   (Cont f1) `times` (Cont f2) = undefined  {-
     let p = (\(CSpliceQ pr1, CSpliceQ tan1) (CSpliceQ pr2, CSpliceQ tan2) -> 
                           (CSpliceQ [|| (times $$pr1 $$pr2 ) ||], CSpliceQ [|| Hom (\grad -> 
                                                                                let (Hom df1) = $$tan1
                                                                                    (Hom df2) = $$tan2 
                                                                                    m = $$pr1
                                                                                in ((df2 $ ((tr m) `times` grad)) <> (df1 $ (grad `times` (tr $$pr2))) ))
                                                                           ||]))
     in Cont $ \k -> f1 (\x -> f2 (\y -> k (p x y)))

--      Cont (\g -> (f2 . f1) $ (\a -> g . (p a)))

instance SizedSemiring (CSpliceQ (SCont Mat (Hom Mat (Endo (Sparse k Mat))))) where
  fromMat mat = CSpliceQ [|| Cont (\f -> f (mat, mempty)) ||]
  tr (CSpliceQ e1) = CSpliceQ $ liftSplice $ do 
    b <- examineCode   [||
                           let Cont f = $$e1
                               z = f (\(Mat v, Hom df) ->
                                           let t = Hom (\(Mat grad) -> df $ (Mat (LinAlg.tr grad)))
                                               k = Mat $ LinAlg.tr v
                                           in (k,t))
                           in Cont (\f' -> f' z)
                        ||]
    return b
  (CSpliceQ e1) `plus` (CSpliceQ e2) = CSpliceQ $ liftSplice $ do   
    b <- examineCode $ [||
                           let Cont f1 = $$e1
                               Cont f2 = $$e1
                               (Mat v1, Hom df1) = f1 id
                               (Mat v2, Hom df2) = f2 id
                               res = (Mat $ LinAlg.add v1 v2, Hom $ \grad -> (df2 grad) <> (df1 grad))                               
                           in Cont (\f' -> f' res)
                      ||]
    return b
  (CSpliceQ e1) `times` (CSpliceQ e2) = CSpliceQ $ liftSplice $ do   
    b <- examineCode $ [||
                           let Cont f1 = $$e1
                               Cont f2 = $$e2
                               (Mat v1, Hom df1) = f1 id
                               (Mat v2, Hom df2) = f2 id
                               res = (Mat $ LinAlg.mul v1 v2, 
                                      (Hom $ \(Mat grad) -> (df2 $ (Mat $ (LinAlg.tr v1) `LinAlg.mul` grad)) <> (df1 $ (Mat $ grad `LinAlg.mul` (LinAlg.tr v2)))))
                           in Cont (\f' -> f' res)
                      ||]
    return b
    -}
{-
instance (GCompare k) => SizedSemiring (CSpliceQ (Dual Mat (Hom Mat (Endo (Sparse k Mat))))) where 
  fromMat mat = CSpliceQ [|| Dual mat mempty ||]
  tr (CSpliceQ e1) = CSpliceQ $ liftSplice $ do
    a <- TH.unTypeCode e1
--    _ <- liftIO $ appendFile "/home/rinat/autodiff/autodiff-exp/splices.txt" $ show $ TH.ppr a
--    _ <-   liftIO $ appendFile "/home/rinat/autodiff/autodiff-exp/splices.txt" "\ntt1\n\n"   
    b <- examineCode   [||
                           let Dual (Mat mat) (Hom f) = $$e1
                           in Dual (Mat $ LinAlg.tr mat) (Hom $ \(Mat grad) -> f (Mat (LinAlg.tr grad))) 
                        ||]
    return b
  (CSpliceQ e1) `plus` (CSpliceQ e2) = CSpliceQ $ liftSplice $ do 
    a <- TH.unTypeCode e1
--    _ <- liftIO $ appendFile "/home/rinat/autodiff/autodiff-exp/splices.txt" $ show $ TH.ppr a
--    _ <-   liftIO $ appendFile "/home/rinat/autodiff/autodiff-exp/splices.txt" "\nuu2\n\n"   
    b <- examineCode $ [||
                           let (matL, df1) = $$(opt e1)
                               (matR, df2) = $$(opt e2)
                           in Dual (Mat $ (LinAlg.add) matL matR) (Hom $ \grad -> (df2 grad) <> (df1 grad))
                      ||]
    return b

  (CSpliceQ e1) `times` (CSpliceQ e2) = CSpliceQ $ liftSplice $ do 
    a <- TH.unTypeCode e1
--    _ <- liftIO $ appendFile "/home/rinat/autodiff/autodiff-exp/splices.txt" $ show $ TH.ppr a
--    _ <-   liftIO $ appendFile "/home/rinat/autodiff/autodiff-exp/splices.txt" "\nii3\n\n"

    b <- examineCode $ [|| let (matL, df1) = $$(opt e1)
                               (matR, df2) = $$(opt e2)
                           in Dual (Mat $ (LinAlg.mul) matL matR) $ 
                                    (Hom $ \(Mat grad) -> (df2 $ (Mat $ (LinAlg.tr matL) `LinAlg.mul` grad)) <> (df1 $ (Mat $ grad `LinAlg.mul` (LinAlg.tr matR))) )
          ||]

--    a' <- TH.unTypeCode $ TH.liftCode $ return b
--    b' <- case a' of
--            TH.LetE [exp1, exp2] mainExp -> do
--                                          liftIO $ appendFile "/home/rinat/autodiff/autodiff-exp/splices3.txt" (show $ mainExp)
--                                          liftIO $ appendFile "/home/rinat/autodiff/autodiff-exp/splices3.txt" "\npp67\n\n"
--                                          liftIO $ appendFile "/home/rinat/autodiff/autodiff-exp/splices3.txt" (show $ exp2)
--                                          liftIO $ appendFile "/home/rinat/autodiff/autodiff-exp/splices3.txt" "\npp68\n\n"
--                                          return $ TH.LetE [exp1, exp2] mainExp
--            _ -> return a'
--    _ <- liftIO $ appendFile "/home/rinat/autodiff/autodiff-exp/splicesqq.txt" $ show (TH.ppr a')
--    _ <- liftIO $ appendFile "/home/rinat/autodiff/autodiff-exp/splicesqq.txt" "\npp3\n\n"
    return b
-}
opt :: SpliceQ (Dual Mat (Hom Mat (Endo (Sparse k Mat))) '(n, k1))
    -> Code TH.Q (LinAlg.L n k1, Mat '(n, k1) -> Endo (Sparse k Mat))
opt e1 = [|| let Dual (Mat matL) (Hom df1) = $$(e1)
                           in (matL, df1)||]

opt' :: SpliceQ (SCont Mat (Hom Mat (Endo (Sparse k Mat))) '(n, k1)) 
     -> Code TH.Q ((LinAlg.L n k1, Mat '(n, k1) -> Endo (Sparse k Mat)))
opt' e1 = [|| let Cont f = $$(e1)
                  (Mat m, Hom df) = f (\(a,b) -> (a,b))
              in (m, df) ||] 
{-
reverseADStaged ::
  forall v d.
  (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup), SizedSemiring
                          (CSpliceQ (Dual d (Hom d (Sparse v d)))) ) =>
  (forall n' m'. CSpliceQ v '(n', m') -> CSpliceQ d '(n', m')) ->
  Expr (CSpliceQ v) '(1, 1) ->
  SpliceQ (DMap v d)
reverseADStaged env expr =
  [|| let Dual _ (Hom rev) = $$(splice (eval env' expr))
          Sparse dmap = rev $ fromMat idMat
       in dmap ||]
  where
    env' :: forall n' m'. CSpliceQ v '(n', m') -> CSpliceQ (Dual d (Hom d (Sparse v d))) '(n', m')
    env' v = CSpliceQ
      [|| Dual
            $$(splice (env v))
            (Hom $ \grad -> Sparse $ DMap.singleton $$(splice v) grad) ||]
-}

reverseADStaged ::
  forall v d.
  (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup), SizedSemiring
                          (CSpliceQ (SCont d (Hom d (Sparse v d)))) ) =>
  (forall n' m'. CSpliceQ v '(n', m') -> CSpliceQ d '(n', m')) ->
  Expr (CSpliceQ v) '(1, 1) ->
  SpliceQ (DMap v d)
reverseADStaged env expr =
  [|| let Cont f = $$(splice (eval env' expr))
          (_, Hom rev) = f id
          Sparse dmap = rev $ fromMat idMat
       in dmap ||]
  where
    env' :: forall n' m'. CSpliceQ v '(n', m') -> CSpliceQ (SCont d (Hom d (Sparse v d))) '(n', m')
    env' v = CSpliceQ
      [|| let m = $$(splice (env v))
              k = (Hom $ \grad -> Sparse $ DMap.singleton $$(splice v) grad)
          in Cont (\f -> f (m,k))||]
{- 
reverseADEndoStaged ::
  forall v d.
  (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup), SizedSemiring
                          (CSpliceQ (SCont d (Hom d (Endo (Sparse v d)))))) =>
  (forall n' m'. CSpliceQ v '(n', m') -> CSpliceQ d '(n', m')) ->
  Expr (CSpliceQ v) '(1, 1) ->
  SpliceQ (DMap v d)
reverseADEndoStaged env expr =
  [|| let Cont f = $$(splice (eval env' expr))
          (_, Hom rev) = f id
          Sparse dmap = appEndo (rev $ fromMat idMat) mempty
       in dmap ||]
  where
    env' ::
      forall n' m'.
      (KnownNat n', KnownNat m') =>
      CSpliceQ v '(n', m') ->
      CSpliceQ (SCont d (Hom d (Endo (Sparse v d)))) '(n', m')
    env' v = CSpliceQ
      [|| let m = $$(splice (env v))
              k = (Hom $ \grad -> Endo $ \(Sparse acc) -> Sparse $ DMap.insertWith plus $$(splice v) grad acc) 
          in Cont (\f -> f (m,k))||] 
-}      

reverseADEndoStaged ::
  forall (v :: (Nat, Nat) -> Type) (d :: (Nat, Nat) -> Type).
  (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup), SizedSemiring
                          (SCont (CSpliceQ d) (CSpliceQ (Hom d (Endo (Sparse v d))))) ) =>
  (forall n' m'. CSpliceQ v '(n', m') -> CSpliceQ d '(n', m')) ->
  Expr (CSpliceQ v) '(1, 1) ->
  SpliceQ (DMap v d)
reverseADEndoStaged env expr =
  let (Cont f) = (eval env' expr)
  in  f (\(_, (CSpliceQ sRev)) -> 
            [|| let (Hom rev) = $$sRev
                    Sparse dmap = appEndo (rev $ fromMat (idMat)) mempty
                in dmap ||])
  where
    env' ::
      forall n' m'.
      (KnownNat n', KnownNat m') =>
      CSpliceQ v '(n', m') ->
      SCont (CSpliceQ d) (CSpliceQ (Hom d (Endo (Sparse v d)))) '(n', m')
    env' v = 
      let tan = CSpliceQ [|| (Hom $ \grad -> Endo $ \(Sparse acc) -> Sparse $ DMap.insertWith plus $$(splice v) grad acc) ||]
      in Cont (\f -> f (env v, tan))   

{-
reverseADEndo :: forall v d. (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup))
              => (forall n' m'. v '(n', m') -> d '(n', m'))
              -> Expr v '(1, 1)
              -> DMap v d
reverseADEndo env expr = let Dual _ (Hom rev) = eval env' expr
                             Sparse map = appEndo (rev $ fromMat idMat) mempty
                          in map
    where env' :: forall n' m'. (KnownNat n', KnownNat m')
               => v '(n', m')
               -> Dual d (Hom d (Endo (Sparse v d))) '(n', m')
          env' v = Dual (env v) (Hom $ \grad -> Endo $ \(Sparse acc) -> Sparse $ DMap.insertWith plus v grad acc)
-}

type AutoDiffStagedType =
  forall v d.
  (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup), SizedSemiring
                          (SCont (CSpliceQ d) (CSpliceQ (Hom d (Endo (Sparse v d))))) 
  ) =>
  (forall n' m'. CSpliceQ v '(n', m') -> CSpliceQ d '(n', m')) ->
  Expr (CSpliceQ v) '(1, 1) ->
  SpliceQ (DMap v d)  