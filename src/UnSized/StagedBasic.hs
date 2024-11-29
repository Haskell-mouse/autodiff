{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances, IncoherentInstances #-}

module UnSized.StagedBasic where 

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

instance Semiring (SCont (SpliceQ Double) (Hom (SpliceQ Double) (Endo (Sparse k (SpliceQ Double)))) ) where 
    zero = Cont (\f -> f ([|| zero ||] , mempty))
    one = Cont (\f -> f ([|| one ||] , mempty))
    (Cont f1) `plus` (Cont f2) = 
        let p =  (\(pr1,(Hom df1)) (pr2,(Hom df2)) -> 
                       ([|| $$pr1 + $$pr2 ||], Hom (\grad -> (df1 grad) <> (df2 grad))))

        in Cont $ \k -> f1 (\x -> f2 (\y -> k (p x y)))
    (Cont f1) `times` (Cont f2) =
        let p = (\(pr1, tan1) (pr2,tan2) ->
                           let (Hom df1) = tan1
                               (Hom df2) = tan2
                           in (times pr1 pr2, Hom (\grad -> ((df2 $ pr1 `times` grad) <> (df1 $ grad `times` (pr2))) )))
        in Cont $ \k -> f1 (\x -> f2 (\y -> k (p x y)))

reverseADEndoStaged ::
  forall d.
  (Semiring d, Ord d, TH.Lift d, Semigroup d, Semiring 
                          (SCont (SpliceQ d) (Hom (SpliceQ d) (Endo (Sparse Nat (SpliceQ d))))), 
                          Convertable Nat (SCont (SpliceQ d) (Hom (SpliceQ d) (Endo (Sparse Nat (SpliceQ d)))))) =>
  (Nat -> SpliceQ d) ->
  Expr d ->
  Map Nat (SpliceQ d)
reverseADEndoStaged env expr =
  let (Cont f) = (eval env' expr)
  in  f (\(_, (sRev)) -> 
            let (Hom rev) = sRev
                sMmap = appEndo (rev $ fromNatural 1) mempty
            in let Sparse mmap = sMmap
                   in mmap)
  where
    env' :: Nat -> SCont (SpliceQ d) (Hom (SpliceQ d) (Endo (Sparse Nat (SpliceQ d))))
    env' v = 
      let tan = Hom (\grad -> Endo $ \acc -> insertWithSparse plus v grad acc)
      in Cont (\f -> f ([|| $$(env v) ||], tan))

instance (Convertable v d, Semiring d, TH.Lift v) => Convertable v (SCont (SpliceQ d) (Hom (SpliceQ d) (Endo (Sparse Nat (SpliceQ d))))) where 
  cast v = 
    let tan = Hom (\grad -> Endo $ \acc -> acc)
    in Cont (\f -> f ([|| cast v ||], tan))

instance Convertable Nat Double where 
  cast a = fromNatural a 


{-
eval :: (Semiring d, Convertable v d)
     => (Nat -> d)
     -> Expr v
     -> d
-}

type AutoDiffStagedType =
  forall d.
  (Semiring d, Ord d, TH.Lift d, Semigroup d, Semiring 
                          (SCont (SpliceQ d) (Hom (SpliceQ d) (Endo (Sparse Nat (SpliceQ d))))), 
                          Convertable Nat (SCont (SpliceQ d) (Hom (SpliceQ d) (Endo (Sparse Nat (SpliceQ d)))))) =>
  (Nat -> SpliceQ d) ->
  Expr d ->
  Map Nat (SpliceQ d)


-- (Double -> Double) -> 
-- Expr Double -> 
-- Map Double Double