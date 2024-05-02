{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

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

-- TODO: Try to use TH to use "SizedSemiring (CSpliceQ Mat)" instance 

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

opt :: SpliceQ (Dual Mat (Hom Mat (Endo (Sparse k Mat))) '(n, k1))
    -> Code TH.Q (LinAlg.L n k1, Mat '(n, k1) -> Endo (Sparse k Mat))
opt e1 = [|| let Dual (Mat matL) (Hom df1) = $$(e1)
                           in (matL, df1)||]

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

reverseADEndoStaged ::
  forall v d.
  (GCompare v, SizedSemiring d, forall tup. Semigroup (d tup), SizedSemiring
                          (CSpliceQ (Dual d (Hom d (Endo (Sparse v d)))))) =>
  (forall n' m'. CSpliceQ v '(n', m') -> CSpliceQ d '(n', m')) ->
  Expr (CSpliceQ v) '(1, 1) ->
  SpliceQ (DMap v d)
reverseADEndoStaged env expr =
  [|| let Dual _ (Hom rev) = $$(splice (eval env' expr))
          Sparse dmap = appEndo (rev $ fromMat idMat) mempty
       in dmap ||]
  where
    env' ::
      forall n' m'.
      (KnownNat n', KnownNat m') =>
      CSpliceQ v '(n', m') ->
      CSpliceQ (Dual d (Hom d (Endo (Sparse v d)))) '(n', m')
    env' v = CSpliceQ
      [|| Dual
            $$(splice (env v))
            (Hom $ \grad -> Endo $ \(Sparse acc) -> Sparse $ DMap.insertWith plus $$(splice v) grad acc) ||] 
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
                          (CSpliceQ (Dual d (Hom d (Endo (Sparse v d)))))) =>
  (forall n' m'. CSpliceQ v '(n', m') -> CSpliceQ d '(n', m')) ->
  Expr (CSpliceQ v) '(1, 1) ->
  SpliceQ (DMap v d)  
