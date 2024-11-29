{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances, IncoherentInstances #-}

module UnSized.Staged where 

import GHC.TypeLits
import Data.Kind
import Language.Haskell.TH.Syntax.Compat
import Data.Semigroup (Endo)
import Data.Monoid (Endo(..))
import qualified Numeric.LinearAlgebra.Static as LinAlg
import qualified Numeric.LinearAlgebra as LinAlg (Additive(..))

import Data.Functor.Identity
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Control.Monad.IO.Class (liftIO)

import UnSized.UnSized

--import Data.Semiring

import Data.Map

type SCont :: Type -> Type -> Type
newtype SCont a b = Cont { runCont :: forall r. ((a, b) -> r) -> r }

-- todo 
-- turn [||Hom d e||] to Hom ([||d||] [|| e ||])
-- similar with endo

instance Semiring (SCont (SExpr Double) (Hom (SExpr Double) (Endo (Sparse k (SExpr Double)))) ) where 
    zero = Cont (\f -> f (Val zero,mempty))
    one = Cont (\f -> f (Val one,mempty))
    (Cont f1) `plus` (Cont f2) = 
        let p =  (\(pr1,(Hom df1)) (pr2,(Hom df2)) -> 
                       (pr1 ::+:: pr2, Hom (\grad -> (df1 grad) <> (df2 grad))))

        in Cont $ \k -> f1 (\x -> f2 (\y -> k (p x y)))
    (Cont f1) `times` (Cont f2) =
        let p = (\(pr1, tan1) (pr2,tan2) ->
                           let (Hom df1) = tan1
                               (Hom df2) = tan2
                --               pr1' = $$pr1
                           in (LetBind [((-1), times pr1 pr2)] (LVar (-1)), Hom (\grad -> ((df2 (pr1 `times` grad)) <> (df1 (grad `times` (pr2)))) )))
        in Cont $ \k -> f1 (\x -> f2 (\y -> k (p x y)))
{-}
optPlus :: SpliceQ Double -> SpliceQ Double -> SpliceQ Double 
optPlus f s = 
  let x = [|| $$f ||]
      y = [|| $$s ||]
      z =  do 
            spX <- TH.runQ $ examineSplice f-- :: TH.Q TH.TExp
            spY <- TH.runQ $ examineSplice s-- :: TH.Q TH.TExp
            let uSpX = TH.unType spX 
                uSpY = TH.unType spY 
            case (uSpX, uSpX) of 
              (TH.LitE (TH.DoublePrimL t), _) | fromRational t == (zero :: Double) -> return spY
              (_, TH.LitE (TH.DoublePrimL t)) | fromRational t == (zero :: Double) -> return spX
              _ ->  liftIO (writeFile "SplicePlusFirst1.txt" (show uSpX)) >> examineSplice [|| $$f + $$s ||]
  in TH.liftCode z
     

optMult :: SpliceQ Double -> SpliceQ Double -> SpliceQ Double 
optMult f s = 
  let x = [|| $$f ||]
      y = [|| $$s ||]
      z =  do 
            spX <- TH.runQ $ examineSplice f-- :: TH.Q TH.TExp
            spY <- TH.runQ $ examineSplice s-- :: TH.Q TH.TExp
            let uSpX = TH.unType spX 
                uSpY = TH.unType spY 
            case (uSpX, uSpX) of 
              (TH.LitE (TH.DoublePrimL t), _) | fromRational t == (zero :: Double) -> return spX
              (_, TH.LitE (TH.DoublePrimL t)) | fromRational t == (zero :: Double) -> return spY
              (TH.LitE (TH.DoublePrimL t), _) | fromRational t == (one :: Double) -> return spY
              (_, TH.LitE (TH.DoublePrimL t)) | fromRational t == (one :: Double) -> return spX
              _ ->  liftIO (writeFile "SplicePlusFirst1.txt" (show uSpX)) >> examineSplice [|| $$f `times` $$s ||]
  in TH.liftCode z -}

reverseADEndoStaged ::
  forall v d.
  (Semiring d, Semigroup d, Semiring
                          (SCont (SExpr d) (Hom (SExpr d) (Endo (Sparse Nat (SExpr d))) )), 
                          Convertable v (SCont (SExpr d) (Hom (SExpr d) (Endo (Sparse Nat (SExpr d))) ))) =>
  (Nat -> (SExpr d)) ->
  SExpr v ->
  Map Nat (SExpr d)
reverseADEndoStaged env expr =
  let (Cont f) = (eval' env' expr)
  in  f (\(_, (sRev)) -> 
            let (Hom rev) = sRev
                sMmap = appEndo (rev $ fromNatural 1) mempty
            in let Sparse mmap = sMmap
                   in mmap)
  where
    env' :: Nat -> SCont (SExpr d) (Hom (SExpr d) (Endo (Sparse Nat (SExpr d))))
    env' v = 
      let tan = Hom (\grad -> Endo $ \acc -> insertWithSparse plus v grad acc)
      in Cont (\f -> f (env v, tan))

instance (Convertable v d, Semiring d) => Convertable v (SCont (SExpr d) (Hom (SExpr d) (Endo (Sparse Nat (SExpr d))))) where 
  cast v = 
    let tan = Hom (\grad -> Endo $ \acc -> acc)
    in Cont (\f -> f (Val $ cast v, tan))


type AutoDiffStagedType =
  forall v d.
  (Ord v, Semiring d, Semigroup d, Semiring
                          (SCont (SExpr d) (Hom (SExpr d) (Endo (Sparse Nat (SExpr d))))),
                          Convertable v (SCont (SExpr d) (Hom (SExpr d) (Endo (Sparse Nat (SExpr d))) ))
  ) =>
  (Nat -> (SExpr d)) ->
  SExpr v ->
  (Map Nat (SExpr d))
